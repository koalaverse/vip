test_that("ww_multi_scale", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  ames_sf <- sf::st_as_sf(ames, coords = c("Longitude", "Latitude"), crs = 4326)
  ames_model <- lm(Sale_Price ~ Lot_Area, data = ames_sf)
  ames_sf$predictions <- predict(ames_model, ames_sf)

  made_w_grid_args <- ww_multi_scale(
    ames_sf,
    Sale_Price,
    predictions,
    n = list(
      c(20, 20),
      c(10, 10),
      c(1, 1)
    ),
    square = FALSE
  )

  embiggened_bbox <- expand_grid(sf::st_bbox(ames_sf))

  grids <- list(
    sf::st_make_grid(embiggened_bbox, n = c(20, 20), square = FALSE),
    sf::st_make_grid(embiggened_bbox, n = c(10, 10), square = FALSE),
    sf::st_make_grid(embiggened_bbox, n = c(1, 1), square = FALSE)
  )
  made_w_grids <- ww_multi_scale(ames_sf, Sale_Price, predictions, grids = grids)

  expect_identical(
    made_w_grid_args[1:3],
    made_w_grids[1:3]
  )
  expect_snapshot(made_w_grid_args)

  expect_snapshot(
    ww_multi_scale(
      ames_sf,
      Sale_Price,
      predictions,
      grids = grids[1],
      metrics = yardstick::rmse
    )
  )

  expect_snapshot(
    ww_multi_scale(
      ames_sf,
      Sale_Price,
      predictions,
      n = list(c(1, 1)),
      autoexpand_grid = FALSE
    )
  )

  expect_equal(
    ww_multi_scale(
      ames_sf,
      Sale_Price,
      predictions,
      n = list(c(1, 1)),
      metrics = list(yardstick::mae)
    )$.estimate,
    yardstick::mae_vec(mean(ames_sf$Sale_Price), mean(ames_sf$predictions))
  )

  expect_equal(
    ww_multi_scale(
      ames_sf,
      Sale_Price,
      predictions,
      n = list(c(1, 1)),
      metrics = list(yardstick::mae),
      aggregation_function = median
    )$.estimate,
    yardstick::mae_vec(median(ames_sf$Sale_Price), median(ames_sf$predictions))
  )

  expect_snapshot(
    ww_multi_scale(
      dplyr::group_by(ames_sf, Neighborhood),
      Sale_Price,
      predictions,
      n = list(
        c(10, 10),
        c(1, 1)
      ),
      square = FALSE
    )
  )
})

test_that("expected errors", {
  guerry_modeled <- guerry
  guerry_lm <- lm(Crm_prs ~ Litercy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  expect_snapshot(
    ww_multi_scale(
      guerry_modeled,
      Crm_prs,
      predictions,
      n = list(c(1, 1)),
      metrics = yardstick::rmse
    ),
    error = TRUE
  )

  expect_snapshot(
    ww_multi_scale(
      suppressWarnings(sf::st_centroid(guerry_modeled)),
      Crm_prs,
      predictions,
      n = list(c(1, 1)),
      na_rm = c(TRUE, FALSE),
      metrics = yardstick::rmse
    ),
    error = TRUE
  )

  expect_snapshot(
    ww_multi_scale(
      iris,
      Sepal.Length,
      Sepal.Width,
      n = list(c(1, 1)),
      metrics = yardstick::rmse
    ),
    error = TRUE
  )
})

test_that("srr: expected failures for ww_multi_scale", {
  worldclim_predicted <- worldclim_simulation
  worldclim_predicted$predicted <- predict(
    lm(response ~ bio2 * bio10 * bio13 * bio19, data = worldclim_simulation),
    worldclim_simulation
  )

  worldclim_predicted$predicted <- as.character(worldclim_predicted$predicted)
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, predicted, response, n = c(2, 4)),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, response, predicted, n = c(2, 4)),
    error = TRUE
  )

  worldclim_predicted$predicted <- lapply(
    as.numeric(worldclim_predicted$predicted),
    function(x) (x)
  )
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} List column inputs fail:
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, predicted, response, n = c(2, 4)),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} List column inputs fail:
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, response, predicted, n = c(2, 4)),
    error = TRUE
  )

  worldclim_predicted$predicted <- unlist(worldclim_predicted$predicted)
  #' @srrstats {G2.13} Missing data is properly handled
  #' @srrstats {G2.15} Missingness is checked
  #' @srrstats {G2.14} Users can specify behavior with NA results
  #' @srrstats {G2.16} NaN is properly handled
  #' Default removes NA:
  worldclim_predicted$response[4] <- NA_real_
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, predicted, response, n = 2)
  )

  # Default removes NA:
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, response, predicted, n = 2),
  )

  # Or return NA:
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, predicted, response, n = 2, na_rm = FALSE)
  )

  # Or return NA:
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, response, predicted, n = 2, na_rm = FALSE),
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_multi_scale(head(worldclim_predicted, 0), response, predicted, n = c(2, 4)),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_multi_scale(head(worldclim_predicted, 0), predicted, response, n = c(2, 4)),
    error = TRUE
  )

  worldclim_predicted$response <- NA_real_
  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, response, predicted, n = c(2, 4)),
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_multi_scale(worldclim_predicted, predicted, response, n = c(2, 4)),
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-identical:
  expect_snapshot(
    ww_multi_scale(worldclim_simulation, response, response, n = c(2, 4))
  )
})

test_that("other generic srr standards", {
  skip_if_not_installed("withr")
  worldclim_predicted <- worldclim_simulation
  worldclim_predicted$predicted <- predict(
    lm(response ~ bio2 * bio10 * bio13 * bio19, data = worldclim_simulation),
    worldclim_simulation
  )
  noised_worldclim <- worldclim_predicted + rnorm(
    nrow(worldclim_predicted) * ncol(worldclim_predicted),
    .Machine$double.eps,
    .Machine$double.eps
  )
  noised_worldclim <- sf::st_as_sf(
    noised_worldclim,
    crs = sf::st_crs(worldclim_predicted)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    withr::with_seed(
      123,
      ww_multi_scale(worldclim_predicted, response, predicted, n = c(2, 4))
    ),
    withr::with_seed(
      123,
      ww_multi_scale(noised_worldclim, response, predicted, n = c(2, 4))
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    withr::with_seed(
      123,
      ww_multi_scale(worldclim_predicted, predicted, response, n = c(2, 4))
    ),
    withr::with_seed(
      123,
      ww_multi_scale(noised_worldclim, predicted, response, n = c(2, 4))
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_multi_scale(worldclim_predicted, predicted, response, n = c(2, 4))
    ),
    withr::with_seed(
      1107,
      ww_multi_scale(worldclim_predicted, predicted, response, n = c(2, 4))
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_multi_scale(worldclim_predicted, response, predicted, n = c(2, 4))
    ),
    withr::with_seed(
      1107,
      ww_multi_scale(worldclim_predicted, response, predicted, n = c(2, 4))
    )
  )

  #' @srrstats {SP2.3} Testing that loading data is equivalent
  expect_equal(
    ww_multi_scale(
      sf::st_read(
        system.file("worldclim_simulation.gpkg", package = "waywiser")
      ),
      bio13,
      bio19,
      n = c(2, 4)
    )$.estimate,
    ww_multi_scale(worldclim_predicted, bio13, bio19, n = c(2, 4))$.estimate
  )

  #' @srrstats {SP4.1} CRS (and therefore units) is preserved
  expect_identical(
    sf::st_crs(
      ww_multi_scale(worldclim_simulation, bio13, bio19, n = c(2, 4))$.grid[[1]]
    ),
    sf::st_crs(
      worldclim_simulation
    )
  )

  guerry_modeled <- suppressWarnings(sf::st_centroid(guerry))
  guerry_modeled$predictions <- predict(
    lm(Crm_prs ~ Litercy, guerry),
    guerry
  )
  proj_grid <- sf::st_make_grid(guerry, n = 2)

  guerry_modeled_geo <- sf::st_transform(guerry_modeled, 4326)
  geo_grid <- sf::st_transform(proj_grid, 4326)

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP6.1} Testing with both projected and geographic CRS
  #' @srrstats {SP6.1b} Testing with both projected and geographic CRS
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    ww_multi_scale(
      guerry_modeled,
      predictions,
      Crm_prs,
      grids = list(proj_grid)
    )$.estimate,
    ww_multi_scale(
      guerry_modeled_geo,
      predictions,
      Crm_prs,
      grids = list(geo_grid)
    )$.estimate
  )
})

test_that("raster method works", {
  skip_if_not_installed("terra")
  skip_if_not_installed("exactextractr")
  skip_if_not_installed("withr")

  r1 <- matrix(nrow = 10, ncol = 10)
  r1[] <- 1
  r1 <- terra::rast(r1)

  r2 <- matrix(nrow = 10, ncol = 10)
  r2[] <- 2
  r2 <- terra::rast(r2)

  r3 <- c(r1, r2)

  expect_identical(
    ww_multi_scale(truth = r1, estimate = r2, metrics = yardstick::rmse, n = 1)$.estimate,
    1
  )

  expect_identical(
    ww_multi_scale(truth = r1, estimate = r1, metrics = yardstick::rmse, n = 1)$.estimate,
    0
  )

  expect_identical(
    ww_multi_scale(r3, truth = 1, estimate = 2, metrics = yardstick::rmse, n = 1)$.estimate,
    1
  )

  expect_identical(
    ww_multi_scale(r3, truth = 1, estimate = 1, metrics = yardstick::rmse, n = 1)$.estimate,
    0
  )

  # built-in functions
  expect_identical(
    ww_multi_scale(
      truth = r1,
      estimate = r2,
      metrics = yardstick::rmse,
      aggregation_function = "median",
      n = 1
    )$.estimate,
    1
  )

  expect_identical(
    ww_multi_scale(
      r3,
      truth = 1,
      estimate = 2,
      metrics = yardstick::rmse,
      aggregation_function = "median",
      n = 1
    )$.estimate,
    1
  )

  # R functions
  expect_identical(
    ww_multi_scale(
      truth = r1,
      estimate = r2,
      metrics = yardstick::rmse,
      aggregation_function = stats::weighted.mean,
      n = 1
    )$.estimate,
    1
  )

})

test_that("raster method is equivalent", {
  skip_if_not_installed("terra")
  skip_if_not_installed("exactextractr")
  skip_if_not_installed("withr")

  withr::with_seed(
    123,
    {
      x <- terra::rast(matrix(rnorm(100), 10))
      y <- terra::rast(matrix(rnorm(100), 10))
    }
  )

  z <- sf::st_as_sf(terra::as.points(c(x, y)))

  sf_method <- ww_multi_scale(z, lyr.1, lyr.1.1, n = 2)
  sf::st_geometry(sf_method$.grid[[1]]) <- NULL
  sf::st_geometry(sf_method$.grid[[2]]) <- NULL

  terra_method <- ww_multi_scale(truth = x, estimate = y, n = 2)
  sf::st_geometry(terra_method$.grid[[1]]) <- NULL
  sf::st_geometry(terra_method$.grid[[2]]) <- NULL

  terra_data_method <- ww_multi_scale(c(x, y), truth = 1, estimate = 2, n = 2)
  sf::st_geometry(terra_data_method$.grid[[1]]) <- NULL
  sf::st_geometry(terra_data_method$.grid[[2]]) <- NULL

  expect_identical(sf_method, terra_method, tolerance = 1e-6)
  expect_identical(sf_method, terra_data_method, tolerance = 1e-6)
})

test_that("raster method errors as expected", {
  skip_if_not_installed("terra")
  skip_if_not_installed("exactextractr")

  r1 <- matrix(nrow = 10, ncol = 10)

  expect_snapshot(
    ww_multi_scale(truth = 1),
    error = TRUE
  )
  expect_snapshot(
    ww_multi_scale(truth = c(terra::rast(r1), terra::rast(r1))),
    error = TRUE
  )

  expect_snapshot(
    ww_multi_scale(truth = terra::rast(r1), estimate = 1),
    error = TRUE
  )
  expect_snapshot(
    ww_multi_scale(truth = terra::rast(r1), estimate = c(terra::rast(r1), terra::rast(r1))),
    error = TRUE
  )
})
