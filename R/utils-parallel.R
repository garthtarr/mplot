# Pin BLAS/LAPACK (and OpenMP) threads to 1 inside a parallel worker.
#
# When mplot spawns process-level parallelism (e.g. via future::multisession)
# for bootstrap loops in af(), vis(), and bglmnet(), each worker process
# still has its own BLAS/LAPACK library, which may itself be multithreaded
# (OpenBLAS, MKL, Accelerate). Without pinning, `cores` worker processes each
# spawning their own BLAS threads can oversubscribe the machine (up to
# `cores^2` threads), causing CPU thrashing and slower wall-clock time than
# a naive sequential run.
#
# This is a no-op (with no hard dependency) if RhpcBLASctl is not installed,
# so it is safe to call unconditionally from within a worker.
#
# @noRd
mplot_pin_blas_threads <- function() {
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    RhpcBLASctl::blas_set_num_threads(1)
    RhpcBLASctl::omp_set_num_threads(1)
  }
  invisible(NULL)
}
