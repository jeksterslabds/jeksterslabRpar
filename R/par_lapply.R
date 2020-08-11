#' Parallel `lapply`
#'
#' This is a wrapper function for different implementations of `lapply`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param X Vector or list.
#' Each element will be passed as the first argument of `FUN`.
#' @param FUN Function to be applied to each element of `X`.
#' @param ... Arguments to pass to `FUN`.
#' @param par Logical.
#' If `TRUE`, use multiple cores.
#' If `FALSE`, use [`lapply()`].
#' @param ncores Integer.
#' Number of cores to use if `par = TRUE`.
#' If unspecified, defaults to `detectCores() - 1`.
#' @param blas_threads Logical.
#' If `TRUE`, set BLAS threads using `blas_set_num_threads(threads = blas_get_num_procs())`.
#' If `FALSE`, set BLAS threads using `blas_set_num_threads(threads = 1)`.
#' If `par = TRUE`, `blas_threads` is automatically set to `FALSE`
#' to prevent conflicts in parallel processing.
#' This argument is useful when `FUN` can handle implicit parallelism
#' when `par = FALSE`, for example linear algebra operations.
#' @param mc Logical.
#' If `TRUE`, use [`parallel::mclapply()`].
#' If `FALSE`, use [`parallel::parLapply()`] or [`parallel::parLapplyLB()`].
#' Ignored if `par = FALSE`.
#' @param lb Logical.
#' If `TRUE` use [`parallel::parLapplyLB()`].
#' If `FALSE`, use [`parallel::parLapply()`].
#' Ignored if `par = FALSE` and `mc = TRUE`.
#' @param cl_eval Logical.
#' Execute [`parallel::clusterEvalQ()`] using `cl_expr`.
#' Ignored if `mc = TRUE`.
#' @param cl_export Logical.
#' Execute [`parallel::clusterExport()`] using `cl_vars`.
#' Ignored if `mc = TRUE`.
#' @param cl_expr Expression.
#' Expression passed to [`parallel::clusterEvalQ()`]
#' Ignored if `mc = TRUE`.
#' @param cl_vars Character vector.
#' Names of objects to pass to [`parallel::clusterExport()`]
#' Ignored if `mc = TRUE`.
#' @param rbind NULL or logical.
#' If `rbind = NULL`,
#' returns the list produced.
#' If `TRUE`, uses [`rbind()`]
#' to bind the rows of the list produced.
#' If `FALSE`, uses [`cbind()`]
#' to bind the columns of the list produced.
#' Test that each element of the output list
#' has the appropriate dimensions for binding
#' before using this option.
#' @return Returns the results of `FUN` for each element of `X`.
#' @examples
#' X <- rep(x = 5, times = 5)
#' # rbind = NULL----------------------------------------------------------------
#' set.seed(42)
#' par_lapply(
#'   X = X,
#'   FUN = rnorm,
#'   par = FALSE
#' )
#' #-----------------------------------------------------------------------------
#' # rbind = TRUE----------------------------------------------------------------
#' set.seed(42)
#' par_lapply(
#'   X = X,
#'   FUN = rnorm,
#'   par = FALSE,
#'   rbind = TRUE
#' )
#' #-----------------------------------------------------------------------------
#' # rbind = FALSE---------------------------------------------------------------
#' set.seed(42)
#' par_lapply(
#'   X = X,
#'   FUN = rnorm,
#'   par = FALSE,
#'   rbind = FALSE
#' )
#' #-----------------------------------------------------------------------------
#' @seealso [`lapply()`], [`parallel::mclapply()`], and [`parallel::parLapply()`].
#' @import parallel
#' @importFrom RhpcBLASctl blas_set_num_threads
#' @importFrom RhpcBLASctl blas_get_num_procs
#' @export
par_lapply <- function(X,
                       FUN,
                       ...,
                       par = TRUE,
                       ncores = NULL,
                       blas_threads = FALSE,
                       mc = TRUE,
                       lb = FALSE,
                       cl_eval = FALSE,
                       cl_export = FALSE,
                       cl_expr,
                       cl_vars,
                       rbind = NULL) {
  # reset threads on exit
  on.exit(
    blas_set_num_threads(threads = blas_get_num_procs())
  )
  #------------------------------------------------------------------------------
  # turn all run options off-----------------------------------------------------
  run_mclapply <- run_parLapplyLB <- run_parLapply <- setup_cluster <- run_lapply <- FALSE
  #------------------------------------------------------------------------------
  # negotiate which function to run----------------------------------------------
  if (par) {
    # turn off implicit parallelism in blas--------------------------------------
    blas_threads <- FALSE
    # get os---------------------------------------------------------------------
    get_os <- function() {
      sysinf <- Sys.info()
      if (!is.null(sysinf)) {
        os <- sysinf["sysname"]
        if (os == "Darwin") {
          os <- "osx"
        }
      } else {
        ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os)
        ) {
          os <- "osx"
        }
        if (grepl("linux-gnu", R.version$os)
        ) {
          os <- "linux"
        }
      }
      tolower(os)
    }
    os <- get_os()
    #----------------------------------------------------------------------------
    # negotiate mc --------------------------------------------------------------
    if (os == "windows") {
      mc <- FALSE
    }
    #----------------------------------------------------------------------------
    # negotiate ncores-----------------------------------------------------------
    if (is.null(ncores)) {
      ncores <- detectCores()
    }
    #----------------------------------------------------------------------------
    # negotiate which parrallel lapply to run------------------------------------
    if (mc) {
      run_mclapply <- TRUE
    } else {
      if (lb) {
        run_parLapplyLB <- TRUE
      } else {
        run_parLapply <- TRUE
      }
      # setup cluster for both vanilla parLapply and load balancing
      setup_cluster <- TRUE
    }
    #----------------------------------------------------------------------------
  } else {
    # vanilla lapply-------------------------------------------------------------
    run_lapply <- TRUE
    #----------------------------------------------------------------------------
  }
  #------------------------------------------------------------------------------
  # negotiate bind---------------------------------------------------------------
  if (!is.null(rbind)) {
    bind <- TRUE
    if (rbind) {
      do_call_what <- "rbind"
    } else {
      do_call_what <- "cbind"
    }
  } else {
    bind <- FALSE
  }
  #------------------------------------------------------------------------------
  # blas-------------------------------------------------------------------------
  if (blas_threads) {
    # turn on implicit parallelism-----------------------------------------------
    blas_set_num_threads(threads = blas_get_num_procs())
  } else {
    # turn off implicit parallelism in blas--------------------------------------
    blas_set_num_threads(threads = 1)
  }
  #------------------------------------------------------------------------------
  # execute----------------------------------------------------------------------
  if (setup_cluster) {
    cl <- makeCluster(ncores)
    if (cl_eval) {
      clusterEvalQ(
        cl,
        expr = cl_expr
      )
    }
    if (cl_export) {
      clusterExport(
        cl,
        varlist = cl_vars
      )
    }
    on.exit(
      stopCluster(cl)
    )
  }
  if (run_mclapply) {
    out <- mclapply(
      X = X,
      FUN = FUN,
      mc.cores = ncores,
      ...
    )
  }
  if (run_parLapply) {
    out <- parLapply(
      cl = cl,
      X = X,
      fun = FUN,
      ...
    )
  }
  if (run_parLapplyLB) {
    out <- parLapplyLB(
      cl = cl,
      X = X,
      fun = FUN,
      ...
    )
  }
  if (run_lapply) {
    out <- lapply(
      X = X,
      FUN = FUN,
      ...
    )
  }
  #------------------------------------------------------------------------------
  # bind if TRUE-----------------------------------------------------------------
  if (bind) {
    out <- do.call(
      what = do_call_what,
      args = out
    )
  }
  #------------------------------------------------------------------------------
  out
}
