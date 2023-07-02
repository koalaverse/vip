skip_if_not(getRversion() >= numeric_version("4.0.0"))
set.seed(123)
skip_if_not(rlang::is_installed("vip"))
train <- vip::gen_friedman(1000, seed = 101)
test <- train[701:1000, ]
train <- train[1:700, ]

if (rlang::is_installed("rsample")) {
  comb_rset <- rsample::make_splits(train, test)
  comb_rset <- rsample::manual_rset(list(comb_rset), "Fold1")
  comb_rset_no_y <- rsample::make_splits(train[2:11], test[2:11])
  comb_rset_no_y <- rsample::manual_rset(list(comb_rset_no_y), "Fold1")
}

pp <- ppr(y ~ ., data = train, nterms = 11)
importance <- vip::vi_permute(
  pp,
  target = "y",
  metric = "rsquared",
  pred_wrapper = predict
)

test_that("`ww_area_of_applicability` is properly classed", {
  model <- ww_area_of_applicability(y ~ ., train, test, importance)
  expect_s3_class(model, "ww_area_of_applicability")
  expect_s3_class(model, "hardhat_model")
})


test_that("`ww_area_of_applicability` is not defined for vectors", {
  expect_snapshot_error(
    ww_area_of_applicability(mtcars$mpg)
  )
})

test_that("`ww_area_of_applicability` finds 0 distance between identical data", {
  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    suppressWarnings(
      ww_area_of_applicability(y ~ ., train, train, importance)$aoa_threshold
    ),
    0,
    tolerance = 1e-7
  )
})

test_that("`ww_area_of_applicability` works with or without a testing set", {
  expect_error(
    ww_area_of_applicability(y ~ ., train, test, importance),
    NA
  )

  expect_error(
    ww_area_of_applicability(y ~ ., train, importance = importance),
    NA
  )
})

test_that("`ww_area_of_applicability` methods are equivalent", {
  methods <- list(
    ww_area_of_applicability(y ~ ., train, test, importance),
    ww_area_of_applicability(train[2:11], test[2:11], importance),
    ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test[2:11]), importance)
  )

  expect_identical(
    head(methods[[1]], -1),
    head(methods[[2]], -1)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    predict(methods[[1]], test),
    predict(methods[[2]], test)
  )

  expect_identical(
    head(methods[[2]], -1),
    head(methods[[3]], -1)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    predict(methods[[2]], test),
    predict(methods[[3]], test)
  )

  # Comparing rset method to the others --
  # because here we calculate our training data on the entire thing
  # the training, means, sds slots are all different
  skip_if_not_installed("rsample")
  methods[[4]] <- ww_area_of_applicability(comb_rset_no_y, importance = importance)
  expect_equal(
    methods[[3]]$aoa_threshold,
    methods[[4]]$aoa_threshold
  )

  skip_if_not_installed("recipes")
  methods[[5]] <- ww_area_of_applicability(
    comb_rset,
    recipes::recipe(y ~ ., train),
    importance = importance
  )
  expect_identical(
    head(methods[[4]], -1),
    head(methods[[5]], -1)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    predict(methods[[4]], test),
    predict(methods[[5]], test)
  )
})

test_that("`ww_area_of_applicability` can handle different column orders", {
  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_area_of_applicability(train[2:11], test[2:11], importance)$aoa_threshold,
    ww_area_of_applicability(train[2:11], test[11:2], importance)$aoa_threshold
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_area_of_applicability(train[2:11], test[2:11], importance)$aoa_threshold,
    ww_area_of_applicability(train[11:2], test[2:11], importance)$aoa_threshold
  )
})

test_that("NAs are handled", {
  train[1, 2] <- NA
  test[1, 2] <- NA
  if (rlang::is_installed("rsample")) {
    comb_rset <- rsample::make_splits(train, test)
    comb_rset <- rsample::manual_rset(list(comb_rset), "Fold1")
    comb_rset_no_y <- rsample::make_splits(train[2:11], test[2:11])
    comb_rset_no_y <- rsample::manual_rset(list(comb_rset_no_y), "Fold1")
  }

  #' @srrstats {G2.14a} Users can error on NA:
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, test, importance),
    error = TRUE
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, test, importance, na_rm = TRUE)
  )

  #' @srrstats {G2.14a} Users can error on NA:
  expect_snapshot(
    ww_area_of_applicability(train[2:11], test[2:11], importance),
    error = TRUE
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_snapshot(
    ww_area_of_applicability(train[2:11], test[2:11], importance, na_rm = TRUE)
  )

  #' @srrstats {G2.14a} Users can error on NA:
  expect_snapshot(
    ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test[2:11]), importance),
    error = TRUE
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_snapshot(
    ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test[2:11]), importance, na_rm = TRUE)
  )

  skip_if_not_installed("rsample")
  #' @srrstats {G2.14a} Users can error on NA:
  expect_snapshot(
    ww_area_of_applicability(comb_rset_no_y, importance = importance),
    error = TRUE
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_snapshot(
    ww_area_of_applicability(comb_rset_no_y, importance = importance, na_rm = TRUE)
  )

  #' @srrstats {G2.14a} Users can error on NA:
  skip_if_not_installed("recipes")
  expect_snapshot(
    ww_area_of_applicability(
      comb_rset,
      recipes::recipe(y ~ ., train),
      importance = importance
    ),
    error = TRUE
  )
  #' @srrstats {G2.14b} Users can ignore NA:
  expect_snapshot(
    ww_area_of_applicability(
      comb_rset,
      recipes::recipe(y ~ ., train),
      importance = importance,
      na_rm = TRUE
    )
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_snapshot(
    predict(
      ww_area_of_applicability(y ~ ., train, test, importance, na_rm = TRUE),
      test
    )
  )
})

test_that("Expected errors", {
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, test[1:10], importance),
    error = TRUE
  )

  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, test, na_rm = c(TRUE, FALSE), importance),
    error = TRUE
  )

  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, test, head(importance, -1)),
    error = TRUE
  )

  expect_snapshot(
    ww_area_of_applicability(y ~ ., train[1:10], test[1:10], importance),
    error = TRUE
  )
})

skip_if_not(rlang::is_installed("vip"))
train <- vip::gen_friedman(1000, seed = 101)
test <- train[701:1000, ]
train <- train[1:700, ]

pp <- ppr(y ~ ., data = train, nterms = 11)
importance <- vip::vi_permute(
  pp,
  target = "y",
  metric = "rsquared",
  pred_wrapper = predict
)
aoa <- ww_area_of_applicability(y ~ ., train, test, importance)

test_that("normal use", {
  expect_snapshot(
    predict(aoa, test)
  )

  skip_on_os("mac")
  expect_snapshot(
    predict(aoa, train)
  )
})

test_that("`new_ww_area_of_applicability` arguments are assigned correctly", {
  x <- ww_area_of_applicability(y ~ ., train, test, importance)

  skip_on_os("mac")
  expect_equal(names(x), c("transformed_training", "sds", "means", "importance", "d_bar", "aoa_threshold", "blueprint"))
  expect_snapshot(x$transformed_training)
  expect_snapshot(x$sds)
  expect_snapshot(x$means)
  expect_snapshot(x$importance)
  expect_snapshot(x$d_bar)
  expect_snapshot(x$aoa_threshold)
  expect_snapshot(x$blueprint)
  expect_s3_class(x$blueprint, "hardhat_blueprint")
})

#' @srrstats {G5.4} Testing equivalence against CAST:
#' @srrstats {G5.4b} Testing equivalence against CAST and stored values:
#' @srrstats {G5.4c} Data is derived originally from CAST and associated paper
test_that("ww_area_of_applicability() is close-enough to CAST", {
  skip_on_cran()
  #' @srrstats {SP6.2} Testing with ~global data
  relevant_data <- head(as.data.frame(worldclim_simulation)[c(1:4, 6)], 1000)

  # Changes in CAST 0.7.1 mean that thresholds can't be compared against earlier versions
  if (rlang::is_installed("CAST", version = "0.7.1") &&
    rlang::is_installed("caret") &&
    rlang::is_installed("randomforest")) {
    withr::with_seed(
      123,
      model <- caret::train(
        relevant_data[1:4],
        relevant_data$response,
        method = "rf",
        importance = TRUE,
        trControl = caret::trainControl(method = "none", savePredictions = TRUE)
      )
    )

    AOA <- CAST::aoa(relevant_data, model = model)
    cast_threshold <- AOA$parameters$threshold[[1]]
    importance <- data.frame(
      term = rownames(caret::varImp(model)$importance),
      estimate = caret::varImp(model, scale = FALSE)$importance[[1]]
    )
  } else {
    cast_threshold <- 0.2184868
    importance <- data.frame(
      term = c("bio2", "bio10", "bio13", "bio19"),
      estimate = c(50.68727, 57.66859, 62.81009, 48.72391)
    )
  }

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_area_of_applicability(
      response ~ .,
      relevant_data,
      importance = importance
    )$aoa_threshold,
    cast_threshold,
    tolerance = 0.000001
  )
})

test_that("loaded data is equivalent", {
  importance <- data.frame(
    term = c("bio2", "bio10", "bio13", "bio19"),
    estimate = c(50.68727, 57.66859, 62.81009, 48.72391)
  )
  worldclim_loaded <- sf::st_read(
    system.file("worldclim_simulation.gpkg", package = "waywiser")
  )
  names(worldclim_loaded) <- c(
    head(names(worldclim_loaded), -1),
    "geometry"
  )
  attr(worldclim_loaded, "sf_column") <- "geometry"
  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP2.3} Testing with loaded data
  expect_equal(
    ww_area_of_applicability(
      response ~ bio2 + bio10 + bio13 + bio19,
      worldclim_loaded,
      importance = importance
    ),
    ww_area_of_applicability(
      response ~ bio2 + bio10 + bio13 + bio19,
      worldclim_simulation,
      importance = importance
    )
  )
})
