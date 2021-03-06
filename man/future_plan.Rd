% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/future_plan.R
\name{future_plan}
\alias{future_plan}
\title{Future Plan}
\usage{
future_plan(par = FALSE, foreach_seq = TRUE, ...)
}
\arguments{
\item{par}{Logical, or \code{strategy}.
If \code{par = TRUE}, sets \code{multiprocess} future plan.
In this case,
if \code{multicore} evaluation is supported, it will be used,
otherwise \code{multisession} is used.
If \code{par = FALSE}, sets \code{sequential} strategy.
If \code{par} is NOT logical,
\code{par} is passed as the \code{strategy} argument
to \code{\link[future:plan]{future::plan()}}.}

\item{foreach_seq}{Logical.
If \code{TRUE}, use \code{\link[foreach:registerDoSEQ]{foreach::registerDoSEQ()}} for \code{sequential} strategy.
If \code{FALSE}, use \code{sequential} strategy in \code{\link[future:plan]{future::plan()}}.}

\item{...}{Other arguments (other than \code{strategy})
to pass to \code{\link[future:plan]{future::plan()}}.}
}
\value{
Nothing.
}
\description{
This is a wrapper function around \code{\link[doFuture:registerDoFuture]{doFuture::registerDoFuture()}}
\code{\link[foreach:registerDoSEQ]{foreach::registerDoSEQ()}}, and \code{\link[future:plan]{future::plan()}}.
The \code{strategy} is the evaluation function
to use in resolving a future.
This function quickly enables the user
to toggle between \code{sequential} and \code{multiprocess} strategies
with an option to set a custom strategy when needed.
This wrapper function will continue to evolve to adjust to my needs.
}
\examples{
library(future)
# set `sequential` strategy--------------------------------------------------
future_plan(par = FALSE)
plan(strategy = NULL)
# set `multiprocess` strategy------------------------------------------------
future_plan(par = TRUE)
plan(strategy = NULL)
# set `multisession` strategy------------------------------------------------
future_plan(par = multisession)
plan(strategy = NULL)
# set `sequential` strategy--------------------------------------------------
future_plan(par = FALSE, foreach_seq = FALSE)
plan(strategy = NULL)
}
\seealso{
\code{\link[future:plan]{future::plan()}} for more details.
}
