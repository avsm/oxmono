#define _GNU_SOURCE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <stdlib.h>
#include <caml/unixsupport.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#ifdef __linux__
#include <linux/openat2.h>
#include <sys/syscall.h>
#endif

static void component(value name) {
  mlsize_t n = caml_string_length(name);
  const char *s = String_val(name);
  if (n == 0 || n != strlen(s) || strchr(s, '/') ||
      !strcmp(s, ".") || !strcmp(s, ".."))
    caml_invalid_argument("DAV: invalid private component");
}

CAMLprim value proffer_dav_open(value root, value path) {
  CAMLparam2(root, path);
#ifdef __linux__
  if (caml_string_length(path) != strlen(String_val(path)))
    caml_invalid_argument("DAV: NUL path");
  struct open_how how = {
    .flags = O_PATH | O_CLOEXEC,
    .resolve = RESOLVE_BENEATH | RESOLVE_NO_SYMLINKS | RESOLVE_NO_XDEV
  };
  int fd = syscall(SYS_openat2, Int_val(root), String_val(path), &how, sizeof how);
  if (fd < 0) uerror("dav_open", Nothing);
  struct stat st;
  if (fstat(fd, &st) < 0) { int e = errno; close(fd); errno = e; uerror("dav_stat", Nothing); }
  if ((!S_ISREG(st.st_mode) && !S_ISDIR(st.st_mode)) ||
      (S_ISREG(st.st_mode) && st.st_nlink != 1)) {
    close(fd); errno = EACCES; uerror("dav_kind", Nothing);
  }
  /* Reopen the validated O_PATH object, not its replaceable pathname.
     The proc path contains only our live descriptor number. */
  char proc[64];
  snprintf(proc, sizeof proc, "/proc/self/fd/%d", fd);
  int result = open(proc, O_RDONLY | O_CLOEXEC | O_NONBLOCK |
    (S_ISDIR(st.st_mode) ? O_DIRECTORY : 0));
  int saved = errno;
  close(fd);
  if (result < 0) { errno = saved; uerror("dav_reopen", Nothing); }
  CAMLreturn(Val_int(result));
#else
  caml_failwith("DAV hardened filesystem requires Linux openat2");
#endif
}

CAMLprim value proffer_dav_create(value root, value name) {
  CAMLparam2(root, name);
  component(name);
  int fd = openat(Int_val(root), String_val(name),
    O_CREAT | O_EXCL | O_RDWR | O_CLOEXEC | O_NOFOLLOW, 0600);
  if (fd < 0) uerror("dav_create", Nothing);
  CAMLreturn(Val_int(fd));
}
CAMLprim value proffer_dav_unlink(value root, value name) {
  CAMLparam2(root, name);
  component(name);
  if (unlinkat(Int_val(root), String_val(name), 0) < 0)
    uerror("dav_unlink", Nothing);
  CAMLreturn(Val_unit);
}
CAMLprim value proffer_dav_publish(value root, value src, value dst) {
  CAMLparam3(root, src, dst);
  component(src); component(dst);
  if (renameat(Int_val(root), String_val(src), Int_val(root), String_val(dst)) < 0)
    uerror("dav_publish", Nothing);
  CAMLreturn(Val_unit);
}
CAMLprim value proffer_dav_lock(value fd) {
  CAMLparam1(fd);
  if (flock(Int_val(fd), LOCK_EX | LOCK_NB) < 0) uerror("dav_lock", Nothing);
  CAMLreturn(Val_unit);
}
struct names {
  DIR *dir;
  char **items;
  size_t count;
};
static void free_names(value owner) {
  struct names *names = Data_custom_val(owner);
  if (names->dir) closedir(names->dir);
  for (size_t i = 0; i < names->count; ++i) free(names->items[i]);
  free(names->items);
  names->dir = NULL;
  names->items = NULL;
  names->count = 0;
}
static struct custom_operations names_ops = {
  .identifier = "proffer.dav.directory.names",
  .finalize = free_names,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default,
  .compare_ext = custom_compare_ext_default,
  .fixed_length = custom_fixed_length_default
};

CAMLprim value proffer_dav_readdir(value fd, value maximum) {
  CAMLparam2(fd, maximum);
  CAMLlocal4(result, item, cell, owner);
  int limit = Int_val(maximum);
  if (limit < 1 || limit > 1000000)
    caml_invalid_argument("DAV directory limit");
  /* A rooted owner also frees names if allocating the OCaml list raises. */
  owner = caml_alloc_custom(&names_ops, sizeof(struct names), 0, 1);
  struct names *names = Data_custom_val(owner);
  *names = (struct names){0};
  names->items = calloc(limit, sizeof(char *));
  if (!names->items) caml_raise_out_of_memory();
  int copy = dup(Int_val(fd));
  if (copy < 0) { int e = errno; free_names(owner); errno = e;
    uerror("dav_dup", Nothing); }
  names->dir = fdopendir(copy);
  if (!names->dir) { int e = errno; close(copy); free_names(owner);
    errno = e; uerror("dav_readdir", Nothing); }
  for (;;) {
    errno = 0;
    struct dirent *entry = readdir(names->dir);
    if (!entry) {
      int e = errno;
      closedir(names->dir);
      names->dir = NULL;
      if (e) { free_names(owner); errno = e;
        uerror("dav_readdir", Nothing); }
      break;
    }
    if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, ".."))
      continue;
    if (names->count == (size_t)limit) {
      free_names(owner); errno = EFBIG; uerror("dav_readdir", Nothing);
    }
    char *name = strdup(entry->d_name);
    if (!name) { free_names(owner); caml_raise_out_of_memory(); }
    names->items[names->count++] = name;
  }
  result = Val_emptylist;
  size_t count = names->count;
  for (size_t i = 0; i < count; ++i) {
    /* [owner] may move at either allocation, so reacquire its C payload. */
    names = Data_custom_val(owner);
    item = caml_copy_string(names->items[i]);
    cell = caml_alloc_small(2, 0);
    Field(cell, 0) = item; Field(cell, 1) = result;
    result = cell;
  }
  free_names(owner);
  CAMLreturn(result);
}
