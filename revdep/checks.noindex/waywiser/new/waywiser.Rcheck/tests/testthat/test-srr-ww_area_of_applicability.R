skip_if_not(getRversion() >= numeric_version("4.0.0"))
set.seed(123)
skip_if_not(rlang::is_installed("vip"))
train <- vip::gen_friedman(1000, seed = 101)
test <- train[701:1000, ]
train <- train[1:700, ]
comb_rset <- rsample::make_splits(train, test)
comb_rset <- rsample::manual_rset(list(comb_rset), "Fold1")
comb_rset_no_y <- rsample::make_splits(train[2:11], test[2:11])
comb_rset_no_y <- rsample::manual_rset(list(comb_rset_no_y), "Fold1")

pp <- ppr(y ~ ., data = train, nterms = 11)
importance <- vip::vi_permute(
  pp,
  target = "y",
  metric = "rsquared",
  pred_wrapper = predict
)

test_that("srr: expected failures for ww_area_of_applicability", {
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Predictors are numeric:
  train$x3 <- as.character(train$x3)
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, test, importance),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Predictors are numeric:
  expect_snapshot(
    ww_area_of_applicability(train, test, importance),
    error = TRUE
  )

  test$x3 <- as.character(test$x3)
  comb_rset_no_y <- rsample::make_splits(train[2:11], test[2:11])
  comb_rset_no_y <- rsample::manual_rset(list(comb_rset_no_y), "Fold1")
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Predictors are numeric:
  expect_snapshot(
    ww_area_of_applicability(comb_rset_no_y, importance = importance),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} Predictors are not lists:
  train$x3 <- lapply(as.numeric(train$x3), function(x) x)
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, test, importance),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} Predictors are not lists:
  expect_snapshot(
    ww_area_of_applicability(train, test, importance),
    error = TRUE
  )

  test$x3 <- lapply(as.numeric(test$x3), function(x) x)
  comb_rset_no_y <- rsample::make_splits(train[2:11], test[2:11])
  comb_rset_no_y <- rsample::manual_rset(list(comb_rset_no_y), "Fold1")
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} Predictors are not lists:
  expect_snapshot(
    ww_area_of_applicability(comb_rset_no_y, importance = importance),
    error = TRUE
  )

  train$x3 <- unlist(train$x3)
  test$x3 <- unlist(test$x3)
  comb_rset_no_y <- rsample::make_splits(train[2:11], test[2:11])
  comb_rset_no_y <- rsample::manual_rset(list(comb_rset_no_y), "Fold1")

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_area_of_applicability(y ~ ., head(train, 0), test, importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, head(test, 0), importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_area_of_applicability(head(train[2:11], 0), test[2:11], importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_area_of_applicability(train[2:11], head(test[2:11], 0), importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_area_of_applicability(head(as.matrix(train[2:11]), 0), as.matrix(test[2:11]), importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_area_of_applicability(as.matrix(train[2:11]), head(as.matrix(test[2:11]), 0), importance),
    error = TRUE
  )

  train_na <- train
  train_na[] <- NA_real_
  test_na <- test
  test_na[] <- NA_real_
  comb_rset_no_y_train_na <- rsample::make_splits(train_na[2:11], test[2:11])
  comb_rset_no_y_train_na <- rsample::manual_rset(list(comb_rset_no_y_train_na), "Fold1")
  comb_rset_no_y_test_na <- rsample::make_splits(train_na[2:11], test_na[2:11])
  comb_rset_no_y_test_na <- rsample::manual_rset(list(comb_rset_no_y_test_na), "Fold1")
  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train_na, test, importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, test_na, importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_area_of_applicability(train_na[2:11], test[2:11], importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_area_of_applicability(train[2:11], test_na[2:11], importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_area_of_applicability(as.matrix(train_na[2:11]), as.matrix(test[2:11]), importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test_na[2:11]), importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_area_of_applicability(comb_rset_no_y_train_na, importance = importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_area_of_applicability(comb_rset_no_y, comb_rset_no_y_test_na, importance),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-identical:
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, train, importance)
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-identical:
  expect_snapshot(
    ww_area_of_applicability(train[2:11], train[2:11], importance)
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-identical:
  expect_snapshot(
    ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(train[2:11]), importance)
  )

  comb_rset_no_y_identical <- rsample::make_splits(train[2:11], train[2:11])
  comb_rset_no_y_identical <- rsample::manual_rset(list(comb_rset_no_y_identical), "Fold1")
  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-identical:
  expect_snapshot(
    ww_area_of_applicability(comb_rset_no_y_identical, importance = importance)
  )
})

test_that("other generic srr standards", {
  skip_if_not_installed("withr")
  noised_train <- train + rnorm(
    nrow(train) * ncol(train),
    .Machine$double.eps,
    .Machine$double.eps
  )
  noised_rset <- rsample::make_splits(noised_train[2:11], test[2:11])
  noised_rset <- rsample::manual_rset(list(noised_rset), "Fold1")

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    ww_area_of_applicability(y ~ ., noised_train, test, importance),
    ww_area_of_applicability(y ~ ., train, test, importance)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    ww_area_of_applicability(noised_train[2:11], test[2:11], importance),
    ww_area_of_applicability(train[2:11], test[2:11], importance)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    ww_area_of_applicability(as.matrix(noised_train[2:11]), as.matrix(test[2:11]), importance),
    ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test[2:11]), importance)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    ww_area_of_applicability(noised_rset, importance = importance),
    ww_area_of_applicability(comb_rset_no_y, importance = importance)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_area_of_applicability(y ~ ., train, test, importance)
    ),
    withr::with_seed(
      1107,
      ww_area_of_applicability(y ~ ., train, test, importance)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_area_of_applicability(noised_train[2:11], test[2:11], importance)
    ),
    withr::with_seed(
      1107,
      ww_area_of_applicability(train[2:11], test[2:11], importance)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_area_of_applicability(as.matrix(noised_train[2:11]), as.matrix(test[2:11]), importance)
    ),
    withr::with_seed(
      1107,
      ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test[2:11]), importance)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_area_of_applicability(noised_rset, importance = importance)
    ),
    withr::with_seed(
      1107,
      ww_area_of_applicability(comb_rset_no_y, importance = importance)
    )
  )
})
