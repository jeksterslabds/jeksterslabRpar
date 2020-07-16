#' Future Plan
#'
#' This is a wrapper function around [`doFuture::registerDoFuture()`]
#' [`foreach::registerDoSEQ()`], and [`future::plan()`].
#' The `strategy` is the evaluation function
#' to use in resolving a future.
#' This function quickly enables the user
#' to toggle between `sequential` and `multiprocess` strategies
#' with an option to set a custom strategy when needed.
#' This wrapper function will continue to evolve to adjust to my needs.
#'
#' @param par Logical, or `strategy`.
#' If `par = TRUE`, sets `multiprocess` future plan.
#' In this case,
#' if `multicore` evaluation is supported, it will be used,
#' otherwise `multisession` is used.
#' If `par = FALSE`, sets `sequential` strategy.
#' If `par` is NOT logical,
#' `par` is passed as the `strategy` argument
#' to [`future::plan()`].
#' @param foreach_seq Logical.
#' If `TRUE`, use [`foreach::registerDoSEQ()`] for `sequential` strategy.
#' If `FALSE`, use `sequential` strategy in [`future::plan()`].
#' @param ... Other arguments (other than `strategy`)
#' to pass to [`future::plan()`].
#' @return Nothing.
#' @examples
#' library(future)
#' # set `sequential` strategy--------------------------------------------------
#' future_plan(par = FALSE)
#' plan(strategy = NULL)
#' # set `multiprocess` strategy------------------------------------------------
#' future_plan(par = TRUE)
#' plan(strategy = NULL)
#' # set `multisession` strategy------------------------------------------------
#' future_plan(par = multisession)
#' plan(strategy = NULL)
#' # set `sequential` strategy--------------------------------------------------
#' future_plan(par = FALSE, foreach_seq = FALSE)
#' plan(strategy = NULL)
#' @seealso [`future::plan()`] for more details.
#' @import doFuture
#' @import future
#' @import foreach
#' @export
future_plan <- function(par = FALSE,
                        foreach_seq = TRUE,
                        ...) {
  if (is.logical(par)) {
    if (par) {
      run_future <- TRUE
      strategy <- multiprocess
    } else {
      if (foreach_seq) {
        run_future <- FALSE
      } else {
        run_future <- TRUE
        strategy <- sequential
      }
    }
  } else {
    run_future <- TRUE
    strategy <- par
  }
  if (run_future) {
    registerDoFuture()
    plan(
      strategy = strategy,
      ...
    )
  } else {
    registerDoSEQ()
  }
}
