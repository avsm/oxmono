#include <stdint.h>
#include <errno.h>
#include <sys/statvfs.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

CAMLprim value zarr_proxy_available_bytes(value vfd) {
  CAMLparam1(vfd);
  struct statvfs stat;
  int fd = Int_val(vfd);
  caml_enter_blocking_section();
  int result = fstatvfs(fd, &stat);
  int saved_errno = errno;
  caml_leave_blocking_section();
  if (result < 0) {
    errno = saved_errno;
    caml_uerror("fstatvfs", Nothing);
  }
  uint64_t blocks = stat.f_bavail, unit = stat.f_frsize;
  int64_t bytes = unit && blocks > INT64_MAX / unit
    ? INT64_MAX : (int64_t)(blocks * unit);
  CAMLreturn(caml_copy_int64(bytes));
}
