#!/usr/bin/perl
# Re-apply the local diagnostics-routing patch to a freshly vendored csrc tree.
#
# Upstream ds4 writes its diagnostics straight to stderr with fprintf.  This
# binding is a library inside someone else's process, so every such call is
# rewritten to ds4_diag(), which funnels through an installable sink (see
# ds4_set_log_callback in ds4.h).  The rewrite is mechanical, with a small set
# of deliberate exceptions spelled out below.
#
# Run by vendor.sh; every substitution is anchored on unique upstream text and
# is checked, so an upstream refactor that moves an anchor fails the vendor run
# loudly instead of silently dropping part of the patch.

use strict;
use warnings;

my $dir = shift or die "usage: logging-patch.pl <csrc-dir>\n";

# Files whose fprintf(stderr, ...) calls all become ds4_diag(...).
my @mechanical = qw(
    ds4.c ds4_metal.m ds4_cuda.cu ds4_distributed.c ds4_ssd.c ds4_tp.c
);

sub slurp {
    my ($p) = @_;
    open my $fh, '<', $p or die "open $p: $!\n";
    local $/;
    my $s = <$fh>;
    close $fh;
    return $s;
}

sub spew {
    my ($p, $s) = @_;
    open my $fh, '>', $p or die "open > $p: $!\n";
    print $fh $s;
    close $fh;
}

# Heredocs carry a trailing newline the anchors do not want.
sub chomped {
    my ($s) = @_;
    chomp $s;
    return $s;
}

# Apply a single anchored edit, insisting on exactly one occurrence.
sub edit {
    my ($text, $file, $what, $from, $to) = @_;
    my $n = () = ($$text =~ /\Q$from\E/g);
    die "$file: expected 1 occurrence of '$what', found $n\n" unless $n == 1;
    $$text =~ s/\Q$from\E/$to/;
}

# ---- mechanical rewrite ---------------------------------------------------

my %count;
for my $f (@mechanical) {
    my $p = "$dir/$f";
    my $s = slurp($p);
    # The lookbehind keeps vfprintf out of the plain-fprintf rule; it gets its
    # own va_list-shaped replacement below.
    my $n = ($s =~ s/(?<![A-Za-z_])fprintf\(stderr,/ds4_diag(/g) || 0;
    my $v = ($s =~ s/(?<![A-Za-z_])vfprintf\(stderr,/vds4_diag(/g) || 0;
    die "$f: no fprintf(stderr, ...) calls found - has upstream changed?\n"
        unless $n > 0;
    $count{$f} = $v ? "$n+${v}v" : $n;
    spew($p, $s);
}

# ---- exceptions and the sink implementation -------------------------------

my $ds4c = slurp("$dir/ds4.c");

# ds4_die/ds4_die_errno are fatal, so they must survive the verbosity gating
# that ds4_diag's DEFAULT level is subject to.
edit(\$ds4c, 'ds4.c', 'ds4_die', chomped(<<'FROM'), chomped(<<'TO'));
    ds4_diag( "ds4: %s\n", msg);
FROM
    /* Fatal: route at ERROR level (not ds4_diag's debug-level DEFAULT) so the
     * cause stays visible even when engine diagnostics are otherwise gated. */
    ds4_log(stderr, DS4_LOG_ERROR, "ds4: %s\n", msg);
TO

edit(\$ds4c, 'ds4.c', 'ds4_die_errno', chomped(<<'FROM'), chomped(<<'TO'));
    ds4_diag( "ds4: %s '%s': %s\n", what, path, strerror(errno));
FROM
    /* Fatal: ERROR level so it survives default verbosity gating (see ds4_die). */
    ds4_log(stderr, DS4_LOG_ERROR, "ds4: %s '%s': %s\n", what, path, strerror(errno));
TO

# The sink itself, inserted just above the colour helper it sits beside.
edit(\$ds4c, 'ds4.c', 'sink implementation', chomped(<<'FROM'), chomped(<<'TO'));
static const char *ds4_log_color_code(
FROM
/* Diagnostic sink.  Set by ds4_set_log_callback(); NULL means "write to stderr"
 * (the historical behaviour).  ds4_diag and ds4_vlog both funnel through
 * ds4_emit, so a single callback intercepts every routed diagnostic. */
static ds4_log_fn g_ds4_log_fn = NULL;
static void *g_ds4_log_ud = NULL;

void ds4_set_log_callback(ds4_log_fn fn, void *ud) {
    g_ds4_log_fn = fn;
    g_ds4_log_ud = ud;
}

/* Format one message and hand it to the sink (or stderr when none is set).  The
 * common case fits the stack buffer; longer messages (e.g. logit dumps) grow to
 * the heap so nothing is truncated. */
static void ds4_emit(ds4_log_type type, const char *fmt, va_list ap) {
    char stackbuf[512];
    va_list ap2;
    va_copy(ap2, ap);
    int n = vsnprintf(stackbuf, sizeof(stackbuf), fmt, ap);
    char *msg = stackbuf;
    char *heap = NULL;
    if (n >= (int)sizeof(stackbuf) && n > 0) {
        heap = (char *)malloc((size_t)n + 1);
        if (heap) {
            vsnprintf(heap, (size_t)n + 1, fmt, ap2);
            msg = heap;
        }
    }
    va_end(ap2);
    if (g_ds4_log_fn)
        g_ds4_log_fn(g_ds4_log_ud, type, msg);
    else
        fputs(msg, stderr);
    free(heap);
}

void ds4_diag(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    ds4_emit(DS4_LOG_DEFAULT, fmt, ap);
    va_end(ap);
}

/* va_list counterpart of ds4_diag, for the engine's own trace helpers that
 * forward an already-started va_list (upstream's vfprintf(stderr, ...) calls).
 * File-local: nothing outside the engine needs it. */
static void vds4_diag(const char *fmt, va_list ap) {
    ds4_emit(DS4_LOG_DEFAULT, fmt, ap);
}

static const char *ds4_log_color_code(
TO

edit(\$ds4c, 'ds4.c', 'ds4_log_is_tty sink guard', chomped(<<'FROM'), chomped(<<'TO'));
bool ds4_log_is_tty(FILE *fp) {
FROM
bool ds4_log_is_tty(FILE *fp) {
    /* A host log sink (ds4_set_log_callback) owns stderr/stdout, so TTY-style
     * inline output — colour codes and the begin/".."/"done" progress bars —
     * is suppressed for them even when the fd is a real terminal; ds4_diag and
     * ds4_log still deliver their messages through the sink.  This keeps the
     * engine from writing raw bytes around a host that owns the console. */
    if (g_ds4_log_fn && (fp == stderr || fp == stdout)) return false;
TO

edit(\$ds4c, 'ds4.c', 'ds4_vlog sink routing', chomped(<<'FROM'), chomped(<<'TO'));
static void ds4_vlog(FILE *fp, ds4_log_type type, const char *fmt, va_list ap) {
FROM
static void ds4_vlog(FILE *fp, ds4_log_type type, const char *fmt, va_list ap) {
    /* When a sink is installed, route the typed stderr/stdout logs through it
     * too (uncolourised — the consumer owns colour).  Logs aimed at a real file
     * still go to that file. */
    if (g_ds4_log_fn && (fp == stderr || fp == stdout)) {
        ds4_emit(type, fmt, ap);
        return;
    }
TO

spew("$dir/ds4.c", $ds4c);

# The streaming-expert pread pool runs on raw pthreads that are not registered
# with the OCaml runtime, so this one site must not reach a sink whose callback
# may call into OCaml.  It stays on stderr.
my $metal = slurp("$dir/ds4_metal.m");
edit(\$metal, 'ds4_metal.m', 'pread worker stderr exception',
     chomped(<<'FROM'), chomped(<<'TO'));
        ds4_diag(
                "ds4: Metal streaming expert explicit pread failed
FROM
        /* Reached from the streaming-expert pread worker pool, which runs on
         * raw pthreads that are NOT registered with the OCaml runtime — so this
         * one site stays on stderr rather than routing through the sink (whose
         * callback may cross into OCaml).  See ds4_stubs.c. */
        fprintf(stderr,
                "ds4: Metal streaming expert explicit pread failed
TO
spew("$dir/ds4_metal.m", $metal);

# ds4_cuda.cu is C++ and includes neither ds4.h nor a header carrying the
# declaration below, so the rewritten calls need one of their own.  It must be
# extern "C": ds4_diag is defined in ds4.c, and without the linkage marker the
# C++ compiler would emit a mangled reference that does not resolve.
my $cuda = slurp("$dir/ds4_cuda.cu");
edit(\$cuda, 'ds4_cuda.cu', 'ds4_diag declaration',
     chomped(<<'FROM'), chomped(<<'TO'));
#include "ds4_gpu_mgpu.h"
FROM
#include "ds4_gpu_mgpu.h"

/* Routed diagnostic primitive, as declared in ds4.h.  Repeated here because
 * this file includes no header that carries it, and marked extern "C" so the
 * C++ compiler references the definition in ds4.c rather than a mangled name. */
extern "C" void ds4_diag(const char *fmt, ...);
TO
spew("$dir/ds4_cuda.cu", $cuda);

# ---- header declarations --------------------------------------------------

my $ds4h = slurp("$dir/ds4.h");
edit(\$ds4h, 'ds4.h', 'log sink declarations', chomped(<<'FROM'), chomped(<<'TO'));
void ds4_log(FILE *fp, ds4_log_type type, const char *fmt, ...);
FROM
void ds4_log(FILE *fp, ds4_log_type type, const char *fmt, ...);

/* Diagnostic log sink.
 *
 * By default the engine writes its diagnostics straight to stderr (via
 * ds4_diag, and via ds4_log when its FILE* is stderr/stdout).  A host
 * application can install a callback to intercept them instead — e.g. to route
 * them into its own logging library rather than the process's standard error.
 *
 * The callback receives the fully formatted, NUL-terminated message (the engine
 * does not add or strip a trailing newline) and the ds4_log_type, so the
 * consumer can pick a severity and colourise as it sees fit.  Pass fn = NULL to
 * restore the stderr default.  The callback may be invoked from whichever thread
 * the engine runs work on (including internal worker threads), so the
 * implementation is responsible for any locking / runtime-lock handling. */
typedef void (*ds4_log_fn)(void *ud, ds4_log_type type, const char *msg);
void ds4_set_log_callback(ds4_log_fn fn, void *ud);

/* Emit an untyped (DS4_LOG_DEFAULT) diagnostic through the sink.  This is the
 * routed replacement for the engine's historical `fprintf(stderr, ...)` calls:
 * with no callback installed it behaves exactly like that fprintf. */
void ds4_diag(const char *fmt, ...);
TO
spew("$dir/ds4.h", $ds4h);

# ds4_ssd.c and ds4_tp.c do not include ds4.h, so the primitive is declared in
# the headers they do include.
my $decl = <<'DECL';
/* Routed diagnostic primitive.  Declared here, the base layer, so files that do
 * not pull in ds4.h (e.g. ds4_ssd.c, ds4_tp.c) can still report through the
 * sink; ds4.h documents the sink and re-declares this identically. */
void ds4_diag(const char *fmt, ...);
DECL

for my $h (qw(ds4_ssd.h ds4_tp.h)) {
    my $s = slurp("$dir/$h");
    # Anchor on the final include guard terminator.
    die "$h: no trailing #endif to anchor on\n" unless $s =~ /#endif\s*\z/;
    $s =~ s/(#endif\s*)\z/$decl\n$1/;
    spew("$dir/$h", $s);
}

printf "  logging patch applied (%s)\n",
    join(', ', map { "$_: $count{$_}" } sort keys %count);
