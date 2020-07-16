#' ---
#' title: "Test: future_plan"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' resultput: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: future_plan}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ include=FALSE, cache=FALSE
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  result.width = "100%"
)
#'
#+ setup
library(testthat)
library(future)
library(jeksterslabRpar)
context("Test future_plan.")
#'
#' This test goes through different values that can be passed to `par`
#' and determines if the results of using `future_plan()`
#' are identical to the results of calling
#' the original `future::plan()` function.
#' The results of both as expected to be identical
#' since `future_plan()` is simply
#' a conveniece wrapper function around `future::plan()`.
#'
#' The effect when `par = FALSE` and `foreach_seq = TRUE`
#' are identical to `par = FALSE` and `foreach_seq = FALSE`,
#' that is, setting a `sequential` strategy.
#' However, the method is different because
#' `foreach_seq = TRUE` uses `foreach::registerDoSEQ()`
#' while `foreach_seq = FALSE` use `doFuture::registerDoFuture()`.
#'
#+ test
future_plan_args <- list(
  FALSE, # sequential
  TRUE, # multiprocess
  multisession,
  FALSE # sequential
)
plan_args <- list(
  sequential,
  multiprocess,
  multisession,
  sequential
)
result <- rep(
  x = NA,
  times = length(future_plan_args)
)
for (i in seq_along(future_plan_args)) {
  future_plan(par = future_plan_args[[i]], foreach_seq = FALSE)
  result_future_plan <- plan(strategy = NULL)
  plan(strategy = plan_args[[i]])
  result_plan <- plan(strategy = NULL)
  result[i] <- all.equal(
    attributes(result_future_plan)[["class"]],
    attributes(result_plan)[["class"]],
  )
  message(
    paste0(
      "Result for test ", i, ": ", result[i]
    )
  )
  # reset to foreach sequential
  future_plan(par = FALSE, foreach_seq = TRUE)
}
#+ testthat
test_that("future_plan returns the same class as plan", {
  expect_true(
    all(
      result
    )
  )
})
