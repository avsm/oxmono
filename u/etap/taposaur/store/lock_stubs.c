#include <sys/file.h>
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

CAMLprim value taposaur_try_lock(value fd) {
  if (flock(Int_val(fd), LOCK_EX | LOCK_NB) == 0) return Val_true;
  if (errno == EWOULDBLOCK || errno == EAGAIN) return Val_false;
  uerror("flock", Nothing);
}
