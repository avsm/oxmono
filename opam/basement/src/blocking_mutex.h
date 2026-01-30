#pragma once

/* Mutex implementation via binary semaphore, mirroring OCaml stdlib. */

#ifdef CAML_INTERNALS

#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>

#ifdef __APPLE__
#include <dispatch/dispatch.h>
#define platform_sem_t dispatch_semaphore_t
#else
#include <semaphore.h>
#define platform_sem_t sem_t
#endif

#include "caml/mlvalues.h"
#include "caml/memory.h"

typedef struct {
  platform_sem_t sem;
} * blocking_mutex;
#define Mutex_val(v) (*(blocking_mutex *)Data_custom_val(v))

#define MUTEX_SUCCESS 0

Caml_inline int blocking_mutex_lock(blocking_mutex mut) {

#ifdef __APPLE__
  /* This only returns non-zero on timeout; don't need the other checks */
  if (dispatch_semaphore_wait(mut->sem, DISPATCH_TIME_NOW) == MUTEX_SUCCESS) {
    return MUTEX_SUCCESS;
  }
#else
  if (sem_trywait(&mut->sem) == MUTEX_SUCCESS) {
    return MUTEX_SUCCESS;
  } else if (errno == EINTR) {
    return blocking_mutex_lock(mut);
  } else if (errno != EAGAIN) {
    return errno;
  }
#endif

  caml_enter_blocking_section();
#ifdef __APPLE__
  // No need to check return value, as we're not setting a timeout
  dispatch_semaphore_wait(mut->sem, DISPATCH_TIME_FOREVER);
#else
  int rc = sem_wait(&mut->sem);
#endif
  caml_leave_blocking_section();

#ifdef __APPLE__
  return MUTEX_SUCCESS;
#else
  if (rc == MUTEX_SUCCESS) {
    return MUTEX_SUCCESS;
  } else if (errno == EINTR) {
    return blocking_mutex_lock(mut);
  }
  return errno;
#endif
}

Caml_inline int blocking_mutex_unlock(blocking_mutex mut) {
#ifdef __APPLE__
  dispatch_semaphore_signal(mut->sem);
  return MUTEX_SUCCESS;
#else
  if (sem_post(&mut->sem) == MUTEX_SUCCESS) {
    return MUTEX_SUCCESS;
  }
  return errno;
#endif
}

Caml_inline int blocking_mutex_create(blocking_mutex *res) {
  blocking_mutex mut = caml_stat_alloc_noexc(sizeof(*mut));
  if (mut == NULL) {
    return ENOMEM;
  }
#ifdef __APPLE__
  mut->sem = dispatch_semaphore_create(1);
  if (mut->sem == NULL) {
    caml_stat_free(mut);
    return ENOMEM;
  }
#else
  // 0 = thread-shared, 1 = initial value
  if (sem_init(&mut->sem, 0, 1) != MUTEX_SUCCESS) {
    caml_stat_free(mut);
    return errno;
  }
#endif
  *res = mut;
  return MUTEX_SUCCESS;
}

Caml_inline int blocking_mutex_destroy(blocking_mutex mut) {
#ifdef __APPLE__
  dispatch_release(mut->sem);
  caml_stat_free(mut);
  return MUTEX_SUCCESS;
#else
  if (sem_destroy(&mut->sem) == MUTEX_SUCCESS) {
    caml_stat_free(mut);
    return MUTEX_SUCCESS;
  }
  caml_stat_free(mut);
  return errno;
#endif
}

#endif /* CAML_INTERNALS */
