/* NSURLSession bindings for OCaml/Eio 
 *
 * Threading model: NSURLSession delegate callbacks run on a GCD-backed
 * NSOperationQueue that is unknown to the OCaml runtime.  */

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <stdlib.h>

#ifdef __APPLE__

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <stdatomic.h>
#include <pthread.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#import <Foundation/Foundation.h>

CAMLprim value ocaml_nsurl_available(value vunit)
{
  (void)vunit;
  return Val_true;
}

typedef struct chunk {
  struct chunk *next;
  size_t len;
  size_t off;            /* bytes already consumed */
  unsigned char data[];
} chunk_t;

static void free_chunks(chunk_t *c)
{
  while (c) { chunk_t *next = c->next; caml_stat_free(c); c = next; }
}

typedef struct {
  _Atomic int refs;
  pthread_mutex_t lock;
  pthread_cond_t upcond; /* signals the upload pump */
  /* Guarded by lock — download side: */
  chunk_t *head, *tail;
  size_t buffered;
  int response_received;
  int done;
  int task_suspended;
  int canceled;
  int oom;               /* allocation failed on a no-lock thread */
  long status;
  char **headers;        /* 2 * n_headers strings: key, value, ... */
  size_t n_headers;
  char *error;           /* NULL unless the request failed */
  char *error_domain;    /* NSError domain, with [error] */
  long error_code;       /* NSError code, with [error] */
  /* Guarded by lock — upload side: */
  chunk_t *uq_head, *uq_tail;
  size_t uq_buffered;
  int uq_eof;
  int up_broken;
  void *body_input;      /* __bridge_retained NSInputStream, until handed out */
  void *body_output;     /* __bridge_retained NSOutputStream, taken by pump */
  /* Immutable after start: */
  int fd;                /* write end of the download notification pipe */
  int fd2;               /* write end of the upload-space pipe, or -1 */
  int no_redirect;       /* deliver 3xx responses instead of following */
  size_t max_buffer;
  void *task;            /* __bridge_retained NSURLSessionDataTask */
} nsfetch_t;

static void nsfetch_decref(nsfetch_t *f)
{
  if (atomic_fetch_sub(&f->refs, 1) == 1) {
    free_chunks(f->head);
    free_chunks(f->uq_head);
    for (size_t i = 0; i < 2 * f->n_headers; i++) caml_stat_free(f->headers[i]);
    caml_stat_free(f->headers);
    caml_stat_free(f->error);
    caml_stat_free(f->error_domain);
    if (f->task) {
      NSURLSessionDataTask *t = (__bridge_transfer NSURLSessionDataTask *)f->task;
      (void)t; /* ARC releases it here */
    }
    if (f->body_input) {
      NSInputStream *s = (__bridge_transfer NSInputStream *)f->body_input;
      (void)s;
    }
    pthread_mutex_destroy(&f->lock);
    pthread_cond_destroy(&f->upcond);
    caml_stat_free(f);
  }
}

static void signal_pipe(int fd)
{
  char byte = 1;
  /* Non-blocking; EAGAIN just means the pipe already holds a wakeup. */
  (void)!write(fd, &byte, 1);
}

/* No-lock threads only; NULL means out of memory. */
static char *dup_nsstring(NSString *s)
{
  const char *u = [s UTF8String];
  return caml_stat_strdup_noexc(u ? u : "");
}

#define Nsfetch_val(v) (*(nsfetch_t **)Data_custom_val(v))

static void nsfetch_finalize(value v) { nsfetch_decref(Nsfetch_val(v)); }

static struct custom_operations nsfetch_ops = {
  "nssessionurl.fetch",
  nsfetch_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* ------------------------------------------------------------------ */
/* Upload pump thread                                                  */

static void *upload_pump(void *arg)
{
  nsfetch_t *f = arg;
  @autoreleasepool {
    NSOutputStream *os = (__bridge_transfer NSOutputStream *)f->body_output;
    f->body_output = NULL;
    [os open];
    int broken = 0;
    pthread_mutex_lock(&f->lock);
    for (;;) {
      while (!f->uq_head && !f->uq_eof && !f->canceled && !f->up_broken)
        pthread_cond_wait(&f->upcond, &f->lock);
      if (f->canceled || f->up_broken) break;
      if (!f->uq_head) break; /* EOF and fully drained */
      chunk_t *c = f->uq_head;
      f->uq_head = c->next;
      if (!f->uq_head) f->uq_tail = NULL;
      pthread_mutex_unlock(&f->lock);

      size_t off = 0;
      while (off < c->len) {
        /* Unscheduled bound-pair stream blocks until the session reads.
           If the task dies, the session closes the input end and the
           write fails, unblocking us. */
        NSInteger n = [os write:c->data + off maxLength:c->len - off];
        if (n < 0) { broken = 1; break; }
        if (n == 0) { usleep(1000); continue; }
        off += (size_t)n;
      }
      size_t len = c->len;
      caml_stat_free(c);

      pthread_mutex_lock(&f->lock);
      f->uq_buffered -= len;
      if (broken) { f->up_broken = 1; break; }
      if (f->uq_buffered < f->max_buffer / 2) signal_pipe(f->fd2);
    }
    pthread_mutex_unlock(&f->lock);
    [os close];
  }
  signal_pipe(f->fd2);
  close(f->fd2);
  nsfetch_decref(f);
  return NULL;
}

/* ------------------------------------------------------------------ */
/* Session delegate, shared by all tasks of one NSURLSession           */

@interface OcamlSessionDelegate : NSObject <NSURLSessionDataDelegate> {
  pthread_mutex_t tlock;
  NSMutableDictionary<NSNumber *, NSValue *> *tasks;
}
- (void)registerFetch:(nsfetch_t *)f forTask:(NSURLSessionTask *)task;
- (nsfetch_t *)lookup:(NSURLSessionTask *)task;
@end

@implementation OcamlSessionDelegate

- (instancetype)init
{
  self = [super init];
  if (self) {
    pthread_mutex_init(&tlock, NULL);
    tasks = [NSMutableDictionary new];
  }
  return self;
}

- (void)dealloc
{
  pthread_mutex_destroy(&tlock);
}

- (void)registerFetch:(nsfetch_t *)f forTask:(NSURLSessionTask *)task
{
  pthread_mutex_lock(&tlock);
  tasks[@(task.taskIdentifier)] = [NSValue valueWithPointer:f];
  pthread_mutex_unlock(&tlock);
}

- (nsfetch_t *)lookup:(NSURLSessionTask *)task
{
  pthread_mutex_lock(&tlock);
  nsfetch_t *f = [tasks[@(task.taskIdentifier)] pointerValue];
  pthread_mutex_unlock(&tlock);
  return f;
}

- (void)URLSession:(NSURLSession *)session
                          task:(NSURLSessionTask *)task
    willPerformHTTPRedirection:(NSHTTPURLResponse *)response
                    newRequest:(NSURLRequest *)request
             completionHandler:(void (^)(NSURLRequest *))completionHandler
{
  nsfetch_t *f = [self lookup:task];
  /* nil makes the session deliver the 3xx itself. */
  completionHandler(f && f->no_redirect ? nil : request);
}

- (void)URLSession:(NSURLSession *)session
                 task:(NSURLSessionTask *)task
    needNewBodyStream:(void (^)(NSInputStream *))completionHandler
{
  nsfetch_t *f = [self lookup:task];
  NSInputStream *is = nil;
  if (f) {
    pthread_mutex_lock(&f->lock);
    if (f->body_input) {
      is = (__bridge_transfer NSInputStream *)f->body_input;
      f->body_input = NULL;
    }
    pthread_mutex_unlock(&f->lock);
  }
  completionHandler(is);
}

- (void)URLSession:(NSURLSession *)session
              dataTask:(NSURLSessionDataTask *)task
    didReceiveResponse:(NSURLResponse *)resp
     completionHandler:(void (^)(NSURLSessionResponseDisposition))completionHandler
{
  nsfetch_t *f = [self lookup:task];
  if (!f) { completionHandler(NSURLSessionResponseAllow); return; }
  pthread_mutex_lock(&f->lock);
  if (f->response_received) {
    /* Can fire again for multipart responses; keep the first. */
    pthread_mutex_unlock(&f->lock);
    completionHandler(NSURLSessionResponseAllow);
    return;
  }
  if ([resp isKindOfClass:[NSHTTPURLResponse class]]) {
    NSHTTPURLResponse *hr = (NSHTTPURLResponse *)resp;
    f->status = hr.statusCode;
    NSDictionary *hd = hr.allHeaderFields;
    f->headers = caml_stat_calloc_noexc(2 * hd.count, sizeof(char *));
    if (f->headers == NULL) {
      f->oom = 1;
    } else {
      size_t i = 0;
      for (id k in hd) {
        char *ck = dup_nsstring([k description]);
        char *cv = dup_nsstring([hd[k] description]);
        if (ck == NULL || cv == NULL) {
          caml_stat_free(ck);
          caml_stat_free(cv);
          f->oom = 1;
          break;
        }
        f->headers[i++] = ck;
        f->headers[i++] = cv;
        f->n_headers++;
      }
    }
  }
  f->response_received = 1;
  int oom = f->oom;
  pthread_mutex_unlock(&f->lock);
  if (oom) [task cancel]; /* surfaces as an error on the reading fibre */
  signal_pipe(f->fd);
  completionHandler(NSURLSessionResponseAllow);
}

- (void)URLSession:(NSURLSession *)session
          dataTask:(NSURLSessionDataTask *)task
    didReceiveData:(NSData *)data
{
  nsfetch_t *f = [self lookup:task];
  size_t len = data.length;
  if (!f || len == 0) return;
  chunk_t *c = caml_stat_alloc_noexc(sizeof(chunk_t) + len);
  if (c == NULL) {
    pthread_mutex_lock(&f->lock);
    f->oom = 1;
    pthread_mutex_unlock(&f->lock);
    [task cancel]; /* surfaces as an error on the reading fibre */
    signal_pipe(f->fd);
    return;
  }
  c->next = NULL;
  c->len = len;
  c->off = 0;
  [data getBytes:c->data length:len];

  pthread_mutex_lock(&f->lock);
  if (f->tail) f->tail->next = c; else f->head = c;
  f->tail = c;
  f->buffered += len;
  if (!f->task_suspended && f->buffered >= f->max_buffer) {
    f->task_suspended = 1;
    [task suspend];
  }
  pthread_mutex_unlock(&f->lock);
  signal_pipe(f->fd);
}

- (void)URLSession:(NSURLSession *)session
                    task:(NSURLSessionTask *)task
    didCompleteWithError:(NSError *)err
{
  nsfetch_t *f = [self lookup:task];
  if (!f) return;
  pthread_mutex_lock(&f->lock);
  if (err != nil) {
    f->error = dup_nsstring(err.localizedDescription);
    f->error_domain = dup_nsstring(err.domain);
    f->error_code = err.code;
    if (f->error == NULL || f->error_domain == NULL) f->oom = 1;
    /* Unstick an idle upload pump; a dead task reads no more body. */
    f->up_broken = 1;
    pthread_cond_signal(&f->upcond);
  }
  f->done = 1;
  pthread_mutex_unlock(&f->lock);
  signal_pipe(f->fd);
  close(f->fd); /* last delegate call for this task; no more signals */

  pthread_mutex_lock(&tlock);
  [tasks removeObjectForKey:@(task.taskIdentifier)];
  pthread_mutex_unlock(&tlock);
  nsfetch_decref(f); /* drop the table's reference */
}

@end

/* ------------------------------------------------------------------ */
/* Session handles                                                     */

typedef struct {
  void *session;   /* __bridge_retained NSURLSession */
  void *delegate;  /* __bridge_retained OcamlSessionDelegate */
} nssession_t;

#define Nssession_val(v) (*(nssession_t **)Data_custom_val(v))

static void nssession_finalize(value v)
{
  nssession_t *ns = Nssession_val(v);
  @autoreleasepool {
    NSURLSession *s = (__bridge_transfer NSURLSession *)ns->session;
    [s finishTasksAndInvalidate];
    OcamlSessionDelegate *d = (__bridge_transfer OcamlSessionDelegate *)ns->delegate;
    (void)d;
  }
  caml_stat_free(ns);
}

static struct custom_operations nssession_ops = {
  "nssessionurl.session",
  nssession_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* session_create : Session.config -> session

   Field order and constructor numbering must match the config record in
   urlsession.ml:
     0 ephemeral : bool
     1 request_timeout : float option
     2 resource_timeout : float option
     3 max_connections_per_host : int option
     4 allows_cellular : bool option
     5 allows_expensive : bool option
     6 allows_constrained : bool option
     7 waits_for_connectivity : bool option
     8 cache_policy : cache_policy option
     9 set_cookies : bool option
     10 cookie_accept_policy : cookie_accept_policy option
     11 min_tls : tls_version option
     12 additional_headers : (string * string) array */
CAMLprim value ocaml_nsurl_session_create(value vcfg)
{
  CAMLparam1(vcfg);
  CAMLlocal1(vsession);

  static const NSURLRequestCachePolicy cache_policies[] = {
    NSURLRequestUseProtocolCachePolicy,       /* Use_protocol_cache_policy */
    NSURLRequestReloadIgnoringLocalCacheData, /* Reload_ignoring_local_cache */
    NSURLRequestReturnCacheDataElseLoad,      /* Return_cache_data_else_load */
    NSURLRequestReturnCacheDataDontLoad,      /* Return_cache_data_dont_load */
    NSURLRequestReloadRevalidatingCacheData,  /* Reload_revalidating_cache */
  };
  static const NSHTTPCookieAcceptPolicy cookie_policies[] = {
    NSHTTPCookieAcceptPolicyAlways,                    /* Always */
    NSHTTPCookieAcceptPolicyNever,                     /* Never */
    NSHTTPCookieAcceptPolicyOnlyFromMainDocumentDomain,/* Only_from_main_... */
  };
  static const tls_protocol_version_t tls_versions[] = {
    tls_protocol_version_TLSv12, /* TLS_1_2 */
    tls_protocol_version_TLSv13, /* TLS_1_3 */
  };

  @autoreleasepool {
    NSURLSessionConfiguration *cfg =
      Bool_val(Field(vcfg, 0))
        ? [NSURLSessionConfiguration ephemeralSessionConfiguration]
        : [NSURLSessionConfiguration defaultSessionConfiguration];
    value v;
    if (Is_some(v = Field(vcfg, 1)))
      cfg.timeoutIntervalForRequest = Double_val(Some_val(v));
    if (Is_some(v = Field(vcfg, 2)))
      cfg.timeoutIntervalForResource = Double_val(Some_val(v));
    if (Is_some(v = Field(vcfg, 3)))
      cfg.HTTPMaximumConnectionsPerHost = Int_val(Some_val(v));
    if (Is_some(v = Field(vcfg, 4)))
      cfg.allowsCellularAccess = Bool_val(Some_val(v));
    if (Is_some(v = Field(vcfg, 5)))
      cfg.allowsExpensiveNetworkAccess = Bool_val(Some_val(v));
    if (Is_some(v = Field(vcfg, 6)))
      cfg.allowsConstrainedNetworkAccess = Bool_val(Some_val(v));
    if (Is_some(v = Field(vcfg, 7)))
      cfg.waitsForConnectivity = Bool_val(Some_val(v));
    if (Is_some(v = Field(vcfg, 8)))
      cfg.requestCachePolicy = cache_policies[Int_val(Some_val(v))];
    if (Is_some(v = Field(vcfg, 9)))
      cfg.HTTPShouldSetCookies = Bool_val(Some_val(v));
    if (Is_some(v = Field(vcfg, 10)))
      cfg.HTTPCookieAcceptPolicy = cookie_policies[Int_val(Some_val(v))];
    if (Is_some(v = Field(vcfg, 11)))
      cfg.TLSMinimumSupportedProtocolVersion = tls_versions[Int_val(Some_val(v))];
    v = Field(vcfg, 12);
    if (Wosize_val(v) > 0) {
      NSMutableDictionary *hd = [NSMutableDictionary new];
      for (mlsize_t i = 0; i < Wosize_val(v); i++) {
        value pair = Field(v, i);
        NSString *k = [NSString stringWithUTF8String:String_val(Field(pair, 0))];
        NSString *hv = [NSString stringWithUTF8String:String_val(Field(pair, 1))];
        hd[k] = hv;
      }
      cfg.HTTPAdditionalHeaders = hd;
    }

    OcamlSessionDelegate *d = [OcamlSessionDelegate new];
    NSURLSession *s =
      [NSURLSession sessionWithConfiguration:cfg
                                    delegate:d
                               delegateQueue:nil];
    nssession_t *ns = caml_stat_alloc(sizeof *ns);
    ns->session = (__bridge_retained void *)s;
    ns->delegate = (__bridge_retained void *)d;
    vsession = caml_alloc_custom(&nssession_ops, sizeof(nssession_t *), 0, 1);
    Nssession_val(vsession) = ns;
  }
  CAMLreturn(vsession);
}

/* ------------------------------------------------------------------ */
/* Task stubs                                                          */

/* Shared by the two start entry points.  vbody_opt is a string option and
   only meaningful when fd2 < 0; fd2 >= 0 selects a streamed body. */
static value nsurl_start(value vsession, value vurl, value vmeth,
                         value vheaders, value vbody_opt, int follow_redirects,
                         int fd, int fd2, size_t max_buffer)
{
  CAMLparam5(vsession, vurl, vmeth, vheaders, vbody_opt);
  CAMLlocal1(vhandle);
  int bad_url = 0;

  @autoreleasepool {
    NSString *urlstr = [NSString stringWithUTF8String:String_val(vurl)];
    NSURL *url = urlstr ? [NSURL URLWithString:urlstr] : nil;
    if (url == nil || url.scheme == nil) {
      bad_url = 1;
    } else {
      nssession_t *ns = Nssession_val(vsession);
      NSURLSession *session = (__bridge NSURLSession *)ns->session;
      OcamlSessionDelegate *d = (__bridge OcamlSessionDelegate *)ns->delegate;

      NSMutableURLRequest *req = [NSMutableURLRequest requestWithURL:url];
      req.HTTPMethod = [NSString stringWithUTF8String:String_val(vmeth)];
      for (mlsize_t i = 0; i < Wosize_val(vheaders); i++) {
        value pair = Field(vheaders, i);
        NSString *k = [NSString stringWithUTF8String:String_val(Field(pair, 0))];
        NSString *v = [NSString stringWithUTF8String:String_val(Field(pair, 1))];
        [req setValue:v forHTTPHeaderField:k];
      }
      if (fd2 < 0 && Is_some(vbody_opt)) {
        value b = Some_val(vbody_opt);
        req.HTTPBody = [NSData dataWithBytes:Bytes_val(b)
                                      length:caml_string_length(b)];
      }

      /* The C side must never block or raise a signal on the pipes. */
      fcntl(fd, F_SETNOSIGPIPE, 1);
      fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
      if (fd2 >= 0) {
        fcntl(fd2, F_SETNOSIGPIPE, 1);
        fcntl(fd2, F_SETFL, fcntl(fd2, F_GETFL) | O_NONBLOCK);
      }

      nsfetch_t *f = caml_stat_calloc_noexc(1, sizeof *f);
      if (f == NULL) caml_raise_out_of_memory();
      atomic_store(&f->refs, fd2 >= 0 ? 3 : 2); /* handle + table (+ pump) */
      pthread_mutex_init(&f->lock, NULL);
      pthread_cond_init(&f->upcond, NULL);
      f->fd = fd;
      f->fd2 = fd2;
      f->no_redirect = !follow_redirects;
      f->max_buffer = max_buffer;

      vhandle = caml_alloc_custom(&nsfetch_ops, sizeof(nsfetch_t *), 0, 1);
      Nsfetch_val(vhandle) = f;

      NSURLSessionDataTask *task;
      if (fd2 >= 0) {
        NSInputStream *is = nil;
        NSOutputStream *os = nil;
        [NSStream getBoundStreamsWithBufferSize:65536
                                    inputStream:&is
                                   outputStream:&os];
        f->body_input = (__bridge_retained void *)is;
        f->body_output = (__bridge_retained void *)os;
        pthread_t tid;
        if (pthread_create(&tid, NULL, upload_pump, f) != 0)
          abort(); /* out of threads; no sane recovery mid-construction */
        pthread_detach(tid);
        task = [session uploadTaskWithStreamedRequest:req];
      } else {
        task = [session dataTaskWithRequest:req];
      }
      f->task = (__bridge_retained void *)task;
      [d registerFetch:f forTask:task];
      [task resume];
    }
  }

  if (bad_url) {
    close(fd);
    if (fd2 >= 0) close(fd2);
    caml_failwith("nssessionurl: invalid URL");
  }
  CAMLreturn(vhandle);
}

/* start_data : session -> url -> meth -> headers -> body option
             -> follow_redirects -> fd -> max -> handle */
CAMLprim value ocaml_nsurl_start_data(value vsession, value vurl, value vmeth,
                                      value vheaders, value vbody,
                                      value vfollow, value vfd, value vmax)
{
  return nsurl_start(vsession, vurl, vmeth, vheaders, vbody,
                     Bool_val(vfollow), Int_val(vfd), -1, Long_val(vmax));
}

CAMLprim value ocaml_nsurl_start_data_byte(value *argv, int argn)
{
  return ocaml_nsurl_start_data(argv[0], argv[1], argv[2], argv[3],
                                argv[4], argv[5], argv[6], argv[7]);
}

/* start_stream : session -> url -> meth -> headers -> follow_redirects
              -> fd -> fd2 -> max -> handle */
CAMLprim value ocaml_nsurl_start_stream(value vsession, value vurl, value vmeth,
                                        value vheaders, value vfollow,
                                        value vfd, value vfd2, value vmax)
{
  return nsurl_start(vsession, vurl, vmeth, vheaders, Val_none,
                     Bool_val(vfollow), Int_val(vfd), Int_val(vfd2),
                     Long_val(vmax));
}

CAMLprim value ocaml_nsurl_start_stream_byte(value *argv, int argn)
{
  return ocaml_nsurl_start_stream(argv[0], argv[1], argv[2], argv[3],
                                  argv[4], argv[5], argv[6], argv[7]);
}

/* The OCaml record { domain : string; code : int; message : string }.
   Field order must match [type error] in nssessionurl.ml. */
static value make_error(const char *domain, long code, const char *msg)
{
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_tuple(3);
  Store_field(v, 0, caml_copy_string(domain ? domain : "nssessionurl"));
  Store_field(v, 1, Val_long(code));
  Store_field(v, 2, caml_copy_string(msg));
  CAMLreturn(v);
}

/* poll_info : handle -> info_poll
   type info_poll = Pending | Info of int * (string * string) array
                  | Failed of error */
CAMLprim value ocaml_nsurl_poll_info(value vhandle)
{
  CAMLparam1(vhandle);
  CAMLlocal3(vres, vheaders, vpair);
  nsfetch_t *f = Nsfetch_val(vhandle);

  pthread_mutex_lock(&f->lock);
  if (!f->response_received && f->done) {
    const char *msg = f->error ? f->error
                    : f->oom  ? "nssessionurl: out of memory"
                              : "nssessionurl: no response";
    const char *domain = f->error ? f->error_domain : NULL;
    long code = f->error ? f->error_code : 0;
    pthread_mutex_unlock(&f->lock);
    vres = caml_alloc(1, 1); /* Failed */
    Store_field(vres, 0, make_error(domain, code, msg));
    CAMLreturn(vres);
  }
  if (!f->response_received) {
    pthread_mutex_unlock(&f->lock);
    CAMLreturn(Val_int(0)); /* Pending */
  }
  long status = f->status;
  size_t n = f->n_headers;
  char **hdrs = f->headers;
  pthread_mutex_unlock(&f->lock);

  if (n == 0)
    vheaders = Atom(0);
  else {
    vheaders = caml_alloc(n, 0);
    for (size_t i = 0; i < n; i++) {
      vpair = caml_alloc_tuple(2);
      Store_field(vpair, 0, caml_copy_string(hdrs[2 * i]));
      Store_field(vpair, 1, caml_copy_string(hdrs[2 * i + 1]));
      Store_field(vheaders, i, vpair);
    }
  }
  vres = caml_alloc(2, 0); /* Info */
  Store_field(vres, 0, Val_long(status));
  Store_field(vres, 1, vheaders);
  CAMLreturn(vres);
}

/* read : handle -> bigarray -> ofs -> len -> int
   Returns >0 bytes copied, 0 would-block, -1 EOF, -2 error pending. */
CAMLprim value ocaml_nsurl_read(value vhandle, value vbuf, value vofs, value vlen)
{
  nsfetch_t *f = Nsfetch_val(vhandle);
  unsigned char *dst = (unsigned char *)Caml_ba_data_val(vbuf) + Long_val(vofs);
  size_t len = Long_val(vlen);
  size_t copied = 0;

  pthread_mutex_lock(&f->lock);
  while (f->head != NULL && copied < len) {
    chunk_t *c = f->head;
    size_t avail = c->len - c->off;
    size_t take = avail < len - copied ? avail : len - copied;
    memcpy(dst + copied, c->data + c->off, take);
    c->off += take;
    copied += take;
    f->buffered -= take;
    if (c->off == c->len) {
      f->head = c->next;
      if (f->head == NULL) f->tail = NULL;
      caml_stat_free(c);
    }
  }
  if (copied > 0) {
    if (f->task_suspended && f->buffered < f->max_buffer / 2) {
      f->task_suspended = 0;
      NSURLSessionDataTask *t = (__bridge NSURLSessionDataTask *)f->task;
      [t resume]; /* under the lock, mirroring the suspend site */
    }
    pthread_mutex_unlock(&f->lock);
    return Val_long(copied);
  }
  int done = f->done;
  int failed = f->error != NULL || f->oom;
  pthread_mutex_unlock(&f->lock);
  if (done) return Val_long(failed ? -2 : -1);
  return Val_long(0);
}

/* write_body : handle -> bigarray -> ofs -> len -> int
   Returns >0 bytes accepted, 0 buffer full, -1 upload closed/broken. */
CAMLprim value ocaml_nsurl_write_body(value vhandle, value vbuf, value vofs,
                                      value vlen)
{
  nsfetch_t *f = Nsfetch_val(vhandle);
  unsigned char *src = (unsigned char *)Caml_ba_data_val(vbuf) + Long_val(vofs);
  size_t len = Long_val(vlen);

  chunk_t *c = caml_stat_alloc(sizeof(chunk_t) + len);

  pthread_mutex_lock(&f->lock);
  if (f->canceled || f->up_broken || f->uq_eof) {
    pthread_mutex_unlock(&f->lock);
    caml_stat_free(c);
    return Val_long(-1);
  }
  if (f->uq_buffered >= f->max_buffer) {
    pthread_mutex_unlock(&f->lock);
    caml_stat_free(c);
    return Val_long(0);
  }
  size_t room = f->max_buffer - f->uq_buffered;
  size_t take = len < room ? len : room;
  c->next = NULL;
  c->len = take;
  c->off = 0;
  memcpy(c->data, src, take);
  if (f->uq_tail) f->uq_tail->next = c; else f->uq_head = c;
  f->uq_tail = c;
  f->uq_buffered += take;
  pthread_cond_signal(&f->upcond);
  pthread_mutex_unlock(&f->lock);
  return Val_long(take);
}

/* close_body : handle -> unit */
CAMLprim value ocaml_nsurl_close_body(value vhandle)
{
  nsfetch_t *f = Nsfetch_val(vhandle);
  pthread_mutex_lock(&f->lock);
  f->uq_eof = 1;
  pthread_cond_signal(&f->upcond);
  pthread_mutex_unlock(&f->lock);
  return Val_unit;
}

/* error_info : handle -> error */
CAMLprim value ocaml_nsurl_error_info(value vhandle)
{
  CAMLparam1(vhandle);
  CAMLlocal1(vres);
  nsfetch_t *f = Nsfetch_val(vhandle);
  pthread_mutex_lock(&f->lock);
  /* Immutable once done is set; copy outside the lock so a GC cannot run
     while we hold it. */
  const char *msg = f->error ? f->error
                  : f->oom  ? "nssessionurl: out of memory"
                            : "unknown error";
  const char *domain = f->error ? f->error_domain : NULL;
  long code = f->error ? f->error_code : 0;
  pthread_mutex_unlock(&f->lock);
  vres = make_error(domain, code, msg);
  CAMLreturn(vres);
}

/* cancel : handle -> unit */
CAMLprim value ocaml_nsurl_cancel(value vhandle)
{
  nsfetch_t *f = Nsfetch_val(vhandle);
  @autoreleasepool {
    NSURLSessionDataTask *t = (__bridge NSURLSessionDataTask *)f->task;
    pthread_mutex_lock(&f->lock);
    f->canceled = 1;
    pthread_cond_signal(&f->upcond);
    /* A suspended task ignores -cancel until resumed; doing both under the
       lock serialises against the suspend in didReceiveData. */
    if (f->task_suspended) {
      f->task_suspended = 0;
      [t resume];
    }
    [t cancel];
    pthread_mutex_unlock(&f->lock);
  }
  return Val_unit;
}

#else /* !__APPLE__ */

/* Stub build for non-macOS platforms. */

CAMLprim value ocaml_nsurl_available(value vunit)
{
  (void)vunit;
  return Val_false;
}

CAMLprim value ocaml_nsurl_session_create(value vcfg)
{
  (void)vcfg;
  caml_failwith("nssessionurl: NSURLSession is only available on macOS");
}

CAMLprim value ocaml_nsurl_start_data(value v1, value v2, value v3, value v4,
                                      value v5, value v6, value v7, value v8)
{
  (void)v1; (void)v2; (void)v3; (void)v4; (void)v5; (void)v6; (void)v7;
  (void)v8;
  /* Unreachable in practice (needs a session), but raise for robustness:
     this stub is not [@@noalloc]. */
  caml_failwith("nssessionurl: NSURLSession is only available on macOS");
}

CAMLprim value ocaml_nsurl_start_data_byte(value *argv, int argn)
{
  (void)argn;
  return ocaml_nsurl_start_data(argv[0], argv[1], argv[2], argv[3], argv[4],
                                argv[5], argv[6], argv[7]);
}

CAMLprim value ocaml_nsurl_start_stream(value v1, value v2, value v3, value v4,
                                        value v5, value v6, value v7, value v8)
{
  (void)v1; (void)v2; (void)v3; (void)v4; (void)v5; (void)v6; (void)v7;
  (void)v8;
  caml_failwith("nssessionurl: NSURLSession is only available on macOS");
}

CAMLprim value ocaml_nsurl_start_stream_byte(value *argv, int argn)
{
  (void)argn;
  return ocaml_nsurl_start_stream(argv[0], argv[1], argv[2], argv[3], argv[4],
                                  argv[5], argv[6], argv[7]);
}

CAMLprim value ocaml_nsurl_poll_info(value v)      { (void)v; abort(); }
CAMLprim value ocaml_nsurl_read(value v1, value v2, value v3, value v4)
{ (void)v1; (void)v2; (void)v3; (void)v4; abort(); }
CAMLprim value ocaml_nsurl_write_body(value v1, value v2, value v3, value v4)
{ (void)v1; (void)v2; (void)v3; (void)v4; abort(); }
CAMLprim value ocaml_nsurl_close_body(value v)     { (void)v; abort(); }
CAMLprim value ocaml_nsurl_error_info(value v)     { (void)v; abort(); }
CAMLprim value ocaml_nsurl_cancel(value v)         { (void)v; abort(); }

#endif /* __APPLE__ */
