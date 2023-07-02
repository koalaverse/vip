test_that("pretrain accepts missing value in predictors and (unused) outcome", {

  data("attrition", package = "modeldata")
  ids <- sample(nrow(attrition), 256)

  x <- attrition[ids,-which(names(attrition) == "Attrition")]
  y <- attrition[ids,]$Attrition
  y_missing <- y
  y_missing[1] <- NA

  # numerical missing
  x_missing <- x
  x_missing[1,"Age"] <- NA

  expect_error(
    miss_pretrain <- tabnet_pretrain(x_missing, y, epochs = 1),
    regexp = NA
  )

  # categorical missing
  x_missing <- x
  x_missing[1,"BusinessTravel"] <- NA

  expect_error(
    miss_pretrain <- tabnet_pretrain(x_missing, y, epochs = 1),
    regexp = NA
  )

  # no error when missing in outcome
  expect_error(
    miss_pretrain <- tabnet_pretrain(x, y_missing, epochs = 1),
    regexp = NA
  )

})


test_that("fit accept missing value in predictor, not in outcome", {

  data("attrition", package = "modeldata")
  ids <- sample(nrow(attrition), 256)

  x <- attrition[ids,-which(names(attrition) == "Attrition")]
  y <- attrition[ids,]$Attrition
  y_missing <- y
  y_missing[1] <- NA

  # numerical missing
  x_missing <- x
  x_missing[1,"Age"] <- NA

  expect_error(
    miss_fit <- tabnet_fit(x_missing, y, epochs = 1),
    regexp = NA
  )

  # categorical missing
  x_missing <- x
  x_missing[1,"BusinessTravel"] <- NA

  expect_error(
    miss_fit <- tabnet_fit(x_missing, y, epochs = 1),
    regexp = NA
  )

  # missing in outcome
  expect_error(
    miss_fit <- tabnet_fit(x, y_missing, epochs = 1),
    regexp = "missing"
  )

})

test_that("predict data-frame accept missing value in predictor", {

  data("attrition", package = "modeldata")
  ids <- sample(nrow(attrition), 256)

  x <- attrition[ids,-which(names(attrition) == "Attrition")]
  y <- attrition[ids,]$Attrition
  #
  fit <- tabnet_fit(x, y, epochs = 1)

  # numerical missing
  x_missing <- x
  x_missing[1,"Age"] <- NA

  # predict with numerical missing
  expect_error(
    predict(fit, x_missing),
    regexp = NA
  )
  # categorical missing
  x_missing <- x
  x_missing[1,"BusinessTravel"] <- NA

  # predict with categorical missing
  expect_error(
    predict(fit, x_missing),
    regexp = NA
  )

})

test_that("inference works with missings in the response vector", {

  data("attrition", package = "modeldata")
  ids <- sample(nrow(attrition), 256)

  rec <- recipe(EnvironmentSatisfaction ~ ., data = attrition[ids, ]) %>%
    step_normalize(all_numeric(), -all_outcomes())
  fit <- tabnet_fit(rec, attrition, epochs = 1, valid_split = 0.25,
                    verbose = TRUE)
  # predict with empty vector
  attrition[["EnvironmentSatisfaction"]] <-NA
  expect_error(
    predict(fit, attrition),
    regexp = NA
  )

  # predict with wrong class
  attrition[["EnvironmentSatisfaction"]] <-NA_character_
  expect_error(
    predict(fit, attrition),
    regexp = NA
  )

  # predict with list column
  attrition[["EnvironmentSatisfaction"]] <- list(NA)
  expect_error(
    predict(fit, attrition),
    regexp = NA
  )

})

test_that("explain works with missings in predictors", {

  data("attrition", package = "modeldata")
  ids <- sample(nrow(attrition), 256)

  x <- attrition[ids,-which(names(attrition) == "Attrition")]
  y <- attrition[ids,]$Attrition
  #
  fit <- tabnet_fit(x, y, epochs = 1)

  # numerical missing
  x_missing <- x
  x_missing[1,"Age"] <- NA

  # explain with numerical missing
  expect_error(
    tabnet_explain(fit, x_missing),
    regexp = NA
  )
  # categorical missing
  x_missing <- x
  x_missing[1,"BusinessTravel"] <- NA

  # explain with categorical missing
  expect_error(
    tabnet_explain(fit, x_missing),
    regexp = NA
  )
})
