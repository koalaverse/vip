if (torch::cuda_is_available()) {
  device <- "cuda"
} else {
  device <- "cpu"
}


test_that("resolve_data works through a dataloader", {
  data("ames", package = "modeldata")

  x <- ames[-which(names(ames) == "Sale_Price")]
  y <- ames[,"Sale_Price"]
  # dataset are R6 class and shall be instantiated
  train_ds <- torch::dataset(
    initialize = function() {},
    .getbatch = function(batch) {tabnet:::resolve_data(x[batch,], y[batch,])},
    .length = function() {nrow(x)}
  )()
  expect_no_error(
    train_ds$.getbatch(batch = 1:2)
  )
  # dataloader
  train_dl <- torch::dataloader(
    train_ds,
    batch_size = 2000 ,
    drop_last = TRUE,
    shuffle = FALSE #,
    # num_workers = 0L
  )
  expect_no_error(
    coro::loop(for (batch in train_dl) {
      expect_tensor_shape(batch$x, c(2000, 73))
      expect_true(batch$x$dtype == torch::torch_float())
      expect_tensor_shape(batch$x_na_mask, c(2000, 73))
      expect_true(batch$x_na_mask$dtype == torch::torch_bool())
      expect_tensor_shape(batch$y, c(2000, 1))
      expect_true(batch$y$dtype == torch::torch_float())
      expect_tensor_shape(batch$cat_idx, 40)
      expect_true(batch$cat_idx$dtype == torch::torch_long())
      expect_equal_to_r(batch$output_dim, 1L)
      expect_true(batch$cat_idx$dtype == torch::torch_long())
      expect_tensor_shape(batch$input_dim, 1)
      expect_true(batch$input_dim$dtype == torch::torch_long())
      expect_tensor_shape(batch$cat_dims, 40)
      expect_true(batch$cat_dims$dtype == torch::torch_long())

    })
  )

})

test_that("resolve_data works through a dataloader without nominal variables", {
  n <- 1000
  x <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n)
  )

  y <- x[,"x", drop = FALSE]
  # dataset are R6 class and shall be instanciated
  train_ds <- torch::dataset(
    initialize = function() {},
    .getbatch = function(batch) {tabnet:::resolve_data(x[batch,], y[batch,])},
    .length = function() {nrow(x)}
  )()
  expect_no_error(
    train_ds$.getbatch(batch = 1:2)
  )

  # dataloader
  train_dl <- torch::dataloader(
    train_ds,
    batch_size = 2000 ,
    drop_last = TRUE,
    shuffle = FALSE #,
    # num_workers = 0L
  )
  expect_no_error(
    coro::loop(for (batch in train_dl) {
      expect_tensor_shape(batch$x, c(2000, 3))
      expect_true(batch$x$dtype == torch::torch_float())
      expect_tensor_shape(batch$x_na_mask, c(2000, 3))
      expect_true(batch$x_na_mask$dtype == torch::torch_bool())
      expect_tensor_shape(batch$y, c(2000, 1))
      expect_true(batch$y$dtype == torch::torch_float())
      expect_tensor_shape(batch$cat_idx, 0)
      expect_true(batch$cat_idx$dtype == torch::torch_long())
      expect_equal_to_r(batch$output_dim, 1L)
      expect_true(batch$cat_idx$dtype == torch::torch_long())
      expect_tensor_shape(batch$input_dim, 1)
      expect_true(batch$input_dim$dtype == torch::torch_long())
      expect_tensor_shape(batch$cat_dims, 0)
      expect_true(batch$cat_dims$dtype == torch::torch_long())

    })
  )

})

test_that("resolve_data works for multioutput regression", {
  data("ames", package = "modeldata")

  x <- ames[-which(names(ames) %in% c("Sale_Price", "Lot_Area"))]
  y <- ames[,c("Sale_Price", "Lot_Area")]
  # dataset are R6 class and shall be instantiated
  train_ds <- torch::dataset(
    initialize = function() {},
    .getbatch = function(batch) {tabnet:::resolve_data(x[batch,], y[batch,])},
    .length = function() {nrow(x)}
  )()
  expect_error(
    train_ds$.getbatch(batch = 1:2),
    NA
  )
  # dataloader
  train_dl <- torch::dataloader(
    train_ds,
    batch_size = 2000 ,
    drop_last = TRUE,
    shuffle = FALSE #,
    # num_workers = 0L
  )
  expect_no_error(
    coro::loop(for (batch in train_dl) {
      expect_tensor_shape(batch$x, c(2000, 72))
      expect_true(batch$x$dtype == torch::torch_float())
      expect_tensor_shape(batch$x_na_mask, c(2000, 72))
      expect_true(batch$x_na_mask$dtype == torch::torch_bool())
      expect_tensor_shape(batch$y, c(2000, 2))
      expect_true(batch$y$dtype == torch::torch_float())
      expect_tensor_shape(batch$cat_idx, 40)
      expect_true(batch$cat_idx$dtype == torch::torch_long())
      expect_equal_to_r(batch$output_dim, 2L)
      expect_true(batch$cat_idx$dtype == torch::torch_long())
      expect_tensor_shape(batch$input_dim, 1)
      expect_true(batch$input_dim$dtype == torch::torch_long())
      expect_tensor_shape(batch$cat_dims, 40)
      expect_true(batch$cat_dims$dtype == torch::torch_long())

    })
  )

})
test_that("resolve_data works for multioutput classification", {

  x <- attrix[-which(names(attrix) == "JobSatisfaction")]
  y <- data.frame(y = attriy, z = attriy, sat = attrix$JobSatisfaction)
  # dataset are R6 class and shall be instantiated
  train_ds <- torch::dataset(
    initialize = function() {},
    .getbatch = function(batch) {tabnet:::resolve_data(x[batch,], y[batch,])},
    .length = function() {nrow(x)}
  )()
  expect_error(
    train_ds$.getbatch(batch = 1:2),
    NA
  )
  # dataloader
  train_dl <- torch::dataloader(
    train_ds,
    batch_size = 2000 ,
    drop_last = TRUE,
    shuffle = FALSE #,
    # num_workers = 0L
  )
  expect_no_error(
    coro::loop(for (batch in train_dl) {
      expect_tensor_shape(batch$x, c(2000, 72))
      expect_true(batch$x$dtype == torch::torch_float())
      expect_tensor_shape(batch$x_na_mask, c(2000, 72))
      expect_true(batch$x_na_mask$dtype == torch::torch_bool())
      expect_tensor_shape(batch$y, c(2000, 2))
      expect_true(batch$y$dtype == torch::torch_float())
      expect_tensor_shape(batch$cat_idx, 40)
      expect_true(batch$cat_idx$dtype == torch::torch_long())
      expect_equal_to_r(batch$output_dim, 2L)
      expect_true(batch$cat_idx$dtype == torch::torch_long())
      expect_tensor_shape(batch$input_dim, 1)
      expect_true(batch$input_dim$dtype == torch::torch_long())
      expect_tensor_shape(batch$cat_dims, 40)
      expect_true(batch$cat_dims$dtype == torch::torch_long())

    })
  )

})
