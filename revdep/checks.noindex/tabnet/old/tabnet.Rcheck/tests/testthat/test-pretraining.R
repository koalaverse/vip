test_that("transpose_metrics is not adding an unnamed entry on top of the list", {

  metrics <- list(loss = 1, loss = 2, loss = 3, loss = 4)

  expect_error(
    tabnet:::transpose_metrics(metrics),
    regexp = NA
  )

  expect_equal(
    tabnet:::transpose_metrics(metrics),
    list(loss = c(1, 2, 3, 4))
  )

})

test_that("Unsupervised training with default config, data.frame and formula", {

  expect_error(
    fit <- tabnet_pretrain(x, y, epochs = 1),
    regexp = NA
  )

  expect_error(
    fit <- tabnet_pretrain(Sale_Price ~ ., data = ames, epochs = 1),
    regexp = NA
  )

})

test_that("Unsupervised training with pretraining_ratio", {

  expect_error(
    pretrain <- tabnet_pretrain(attrix, attriy, epochs = 1, pretraining_ratio=0.2),
    regexp = NA
  )

})

test_that("Unsupervised training prevent predict with an explicit message", {

  pretrain <- tabnet_pretrain(attrix, attriy, epochs = 1, pretraining_ratio=0.2)

  expect_error(
    predict(pretrain, attrix, type = "prob"),
    regexp = "tabnet_pretrain"
  )

  expect_error(
    predict(pretrain, attrix),
    regexp = "tabnet_pretrain"
  )

})

test_that("errors when using an argument that do not exist", {

  expect_error(
    pretrain <- tabnet_pretrain(x, y, pretraining_ratiosas = 1-1e5),
    regexp = "unused argument"
  )

})

test_that("works with validation split", {

  expect_error(
    pretrain <- tabnet_pretrain(attrix, attriy, epochs = 1, valid_split = 0.2),
    regexp = NA
  )

  expect_error(
    pretrain <- tabnet_pretrain(attrix, attriy, epochs = 1, valid_split = 0.2, verbose = TRUE),
    regexp = NA
  )

})

test_that("works with categorical embedding dimension as list", {

  config <- tabnet_config(cat_emb_dim=c(1,1,2,2,1,1,1,2,1,1,1,2,2,2))

  expect_error(
    pretrain <- tabnet_pretrain(attrix, attriy, epochs = 1, valid_split = 0.2, config=config),
    regexp = NA
  )
})

test_that("explicit error message when categorical embedding dimension vector has wrong size", {

  config <- tabnet_config(cat_emb_dim=c(1,1,2,2))

  expect_error(
    pretrain <- tabnet_pretrain(attrix, attriy, epochs = 1, valid_split = 0.2, config=config),
    regexp = "number of categorical predictors"
  )
})

test_that("can train from a recipe", {

  rec <- recipe(Attrition ~ ., data = attrition) %>%
    step_normalize(all_numeric(), -all_outcomes())

  expect_error(
    pretrain <- tabnet_pretrain(rec, attrition, epochs = 1, verbose = TRUE),
    regexp = NA
  )

})

test_that("lr scheduler works", {

  expect_error(
    fit <- tabnet_pretrain(x, y, epochs = 3, lr_scheduler = "step",
                      lr_decay = 0.1, step_size = 1),
    regexp = NA
  )

  sc_fn <- function(optimizer) {
    torch::lr_step(optimizer, step_size = 1, gamma = 0.1)
  }

  expect_error(
    fit <- tabnet_pretrain(x, y, epochs = 3, lr_scheduler = sc_fn,
                      lr_decay = 0.1, step_size = 1),
    regexp = NA
  )

})

test_that("checkpoints works", {

  expect_error(
    pretrain <- tabnet_pretrain(x, y, epochs = 3, checkpoint_epochs = 1),
    regexp = NA
  )

  expect_length( pretrain$fit$checkpoints, 3  )

  # expect_equal(  pretrain$fit$checkpoints[[3]], pretrain$serialized_net )

})

test_that("print module works", {

  testthat::local_edition(3)
  testthat::skip_on_os("linux")
  testthat::skip_on_os("windows")

  expect_error(
    fit <- tabnet_pretrain(x, y, epochs = 1),
    regexp = NA
  )

  withr::with_options(new = c(cli.width = 50),
                      expect_snapshot_output(fit))

})

