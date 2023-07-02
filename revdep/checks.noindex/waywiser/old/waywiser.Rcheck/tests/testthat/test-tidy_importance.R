test_that("tidy_importance is idempotent", {
  set.seed(123)
  skip_if_not(rlang::is_installed("vip"))
  train <- vip::gen_friedman(1000, seed = 101)
  pp <- ppr(y ~ ., data = train, nterms = 11)
  importance <- vip::vi_permute(
    pp,
    target = "y",
    metric = "rsquared",
    pred_wrapper = predict
  )

  expect_identical(
    tidy_importance.data.frame(tidy_importance.vi(importance)),
    tidy_importance.data.frame(
      tidy_importance.data.frame(tidy_importance.vi(importance))
    )
  )
})

test_that("expected failures", {
  #' @srrstats {G5.2} Testing each error message,
  #' @srrstats {G5.2a} with a unique message,
  #' @srrstats {G5.2b} against expected outcomes
  expect_snapshot(
    tidy_importance(list()),
    error = TRUE
  )

  expect_snapshot(
    tidy_importance(data.frame()),
    error = TRUE
  )
})
