# This file was generated, do not edit by hand
# Please edit inst/srr_template_spatial_yardstick.R instead

test_that("srr: expected failures for ww_global_moran_i", {
  worldclim_predicted <- worldclim_simulation
  worldclim_predicted$predicted <- predict(
    lm(response ~ bio2 * bio10 * bio13 * bio19, data = worldclim_simulation),
    worldclim_simulation
  )
  worldclim_weights <- ww_build_weights(worldclim_simulation)
  # Note that this test isn't applicable to data-frame input, which enforces
  # constant column lengths
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G2.0} Truth and estimate are equal in length:
  expect_snapshot(
    ww_global_moran_i_vec(
      worldclim_predicted$response,
      tail(worldclim_predicted$predicted, -1),
      worldclim_weights
    ),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G2.0} Truth and estimate are equal in length:
  expect_snapshot(
    ww_global_moran_i_vec(
      tail(worldclim_predicted$response, -1),
      worldclim_predicted$predicted,
      worldclim_weights
    ),
    error = TRUE
  )

  worldclim_predicted$predicted <- as.character(worldclim_predicted$predicted)
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_global_moran_i(worldclim_predicted, predicted, response),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_global_moran_i(worldclim_predicted, response, predicted),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_global_moran_i_vec(
      worldclim_predicted$response,
      worldclim_predicted$predicted,
      worldclim_weights
    ),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_global_moran_i_vec(
      worldclim_predicted$predicted,
      worldclim_predicted$response,
      worldclim_weights
    ),
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
    ww_global_moran_i(worldclim_predicted, response, predicted),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} List column inputs fail:
  expect_snapshot(
    ww_global_moran_i(worldclim_predicted, predicted, response),
    error = TRUE
  )

  worldclim_predicted$predicted <- unlist(worldclim_predicted$predicted)
  #' @srrstats {G2.13} Missing data is properly handled
  #' @srrstats {G2.15} Missingness is checked
  #' @srrstats {G2.14} Users can specify behavior with NA results
  #' @srrstats {G2.16} NaN is properly handled
  #' Users can error:
  worldclim_predicted$response[4] <- NA_real_
  expect_snapshot(
    ww_global_moran_i(worldclim_predicted, predicted, response)$.estimate,
    error = TRUE
  )

  #' Users can error:
  expect_snapshot(
    ww_global_moran_i(worldclim_predicted, response, predicted)$.estimate,
    error = TRUE
  )

  #' Users can error:
  expect_snapshot(
    ww_global_moran_i_vec(worldclim_predicted$predicted, worldclim_predicted$response, worldclim_weights),
    error = TRUE
  )

  #' Users can error:
  expect_snapshot(
    ww_global_moran_i_vec(worldclim_predicted$response, worldclim_predicted$predicted, worldclim_weights),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_global_moran_i_vec(numeric(), numeric(), structure(list(), class = "listw")),
    error = TRUE
  )

  empty_df <- tibble::tibble(x = numeric(), y = numeric())
  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_global_moran_i(head(worldclim_predicted, 0), response, predicted, structure(list(), class = "listw")),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_global_moran_i(head(worldclim_predicted, 0), predicted, response, structure(list(), class = "listw")),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_global_moran_i_vec(NA_real_, NA_real_, structure(list(neighbours = 1), class = "listw")),
    error = TRUE
  )

  worldclim_predicted$response <- NA_real_
  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_global_moran_i(worldclim_predicted, response, predicted)$.estimate,
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_global_moran_i(worldclim_predicted, predicted, response)$.estimate,
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-identical:
  expect_snapshot(
    ww_global_moran_i_vec(worldclim_simulation$response, worldclim_simulation$response, worldclim_weights)
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-identical:
  expect_snapshot(
    ww_global_moran_i(worldclim_simulation, response, response)
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
  worldclim_weights <- ww_build_weights(worldclim_simulation)
  noised_weights <- ww_build_weights(noised_worldclim)

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    withr::with_seed(
      123,
      ww_global_moran_i(worldclim_predicted, response, predicted)
    ),
    withr::with_seed(
      123,
      ww_global_moran_i(noised_worldclim, response, predicted)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    withr::with_seed(
      123,
      ww_global_moran_i(worldclim_predicted, predicted, response)
    ),
    withr::with_seed(
      123,
      ww_global_moran_i(noised_worldclim, predicted, response)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    withr::with_seed(
      123,
      ww_global_moran_i_vec(worldclim_predicted$predicted, worldclim_predicted$response, worldclim_weights)
    ),
    withr::with_seed(
      123,
      ww_global_moran_i_vec(noised_worldclim$predicted, noised_worldclim$response, noised_weights)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    withr::with_seed(
      123,
      ww_global_moran_i_vec(worldclim_predicted$response, worldclim_predicted$predicted, worldclim_weights)
    ),
    withr::with_seed(
      123,
      ww_global_moran_i_vec(noised_worldclim$response, noised_worldclim$predicted, noised_weights)
    )
  )

  skip_on_cran()

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    withr::with_seed(
      123,
      ww_global_moran_i(worldclim_predicted, predicted, response)
    ),
    withr::with_seed(
      1107,
      ww_global_moran_i(worldclim_predicted, predicted, response)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    withr::with_seed(
      123,
      ww_global_moran_i(worldclim_predicted, response, predicted)
    ),
    withr::with_seed(
      1107,
      ww_global_moran_i(worldclim_predicted, response, predicted)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    withr::with_seed(
      123,
      ww_global_moran_i_vec(worldclim_predicted$response, worldclim_predicted$predicted, worldclim_weights)
    ),
    withr::with_seed(
      1107,
      ww_global_moran_i_vec(worldclim_predicted$response, worldclim_predicted$predicted, worldclim_weights)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    withr::with_seed(
      123,
      ww_global_moran_i_vec(worldclim_predicted$predicted, worldclim_predicted$response, worldclim_weights)
    ),
    withr::with_seed(
      1107,
      ww_global_moran_i_vec(worldclim_predicted$predicted, worldclim_predicted$response, worldclim_weights)
    )
  )

  guerry_modeled <- guerry
  guerry_modeled$predictions <- predict(
    lm(Crm_prs ~ Litercy, guerry),
    guerry
  )
  guerry_modeled_geo <- sf::st_transform(guerry_modeled, 4326)
  guerry_weights <- ww_build_weights(guerry)
  guerry_weights_geo <- ww_build_weights(guerry_modeled_geo)

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP6.1} Testing with both projected and geographic CRS
  #' @srrstats {SP6.1b} Testing with both projected and geographic CRS
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    ww_global_moran_i(guerry_modeled, predictions, Crm_prs)$.estimate,
    ww_global_moran_i(guerry_modeled_geo, predictions, Crm_prs)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP6.1} Testing with both projected and geographic CRS
  #' @srrstats {SP6.1b} Testing with both projected and geographic CRS
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    ww_global_moran_i(guerry_modeled, Crm_prs, predictions)$.estimate,
    ww_global_moran_i(guerry_modeled_geo, Crm_prs, predictions)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP6.1} Testing with both projected and geographic CRS
  #' @srrstats {SP6.1b} Testing with both projected and geographic CRS
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    ww_global_moran_i_vec(guerry_modeled$Crm_prs, guerry_modeled$predictions, guerry_weights),
    ww_global_moran_i_vec(guerry_modeled_geo$Crm_prs, guerry_modeled_geo$predictions, guerry_weights_geo)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP6.1} Testing with both projected and geographic CRS
  #' @srrstats {SP6.1b} Testing with both projected and geographic CRS
  #' @srrstats {SP6.2} Testing with ~global data
  expect_equal(
    ww_global_moran_i_vec(guerry_modeled$predictions, guerry_modeled$Crm_prs, guerry_weights),
    ww_global_moran_i_vec(guerry_modeled_geo$predictions, guerry_modeled_geo$Crm_prs, guerry_weights_geo)
  )

  #' @srrstats {SP2.3} Testing with loaded sf objects:
  worldclim_loaded <- sf::read_sf(
    system.file("worldclim_simulation.gpkg", package = "waywiser")
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP2.3} Testing with loaded sf objects:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_snapshot(
    withr::with_seed(
      123,
      ww_global_moran_i(worldclim_loaded, bio13, bio19)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP2.3} Testing with loaded sf objects:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_snapshot(
    withr::with_seed(
      123,
      ww_global_moran_i(worldclim_loaded, bio13, bio19)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP2.3} Testing with loaded sf objects:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_snapshot(
    withr::with_seed(
      123,
      ww_global_moran_i_vec(worldclim_loaded$bio13, worldclim_loaded$bio19, worldclim_weights)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP2.3} Testing with loaded sf objects:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_snapshot(
    withr::with_seed(
      123,
      ww_global_moran_i_vec(worldclim_loaded$bio13, worldclim_loaded$bio19, worldclim_weights)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP2.3} Testing with loaded sf objects:
  #' @srrstats {SP6.2} Testing with ~global data
  expect_snapshot(
    withr::with_seed(
      123,
      ww_global_moran_i(worldclim_loaded, bio13, bio19)
    )
  )

  other_weights <- ww_build_weights(ww_make_point_neighbors(worldclim_loaded, k = 5))
  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP6.3} Testing alternative weights:
  expect_snapshot(
    withr::with_seed(
      123,
      ww_global_moran_i(worldclim_loaded, bio13, bio19, function(data) ww_build_weights(ww_make_point_neighbors(data, k = 5)))
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP6.3} Testing alternative weights:
  expect_snapshot(
    withr::with_seed(
      123,
      ww_global_moran_i_vec(worldclim_loaded$bio13, worldclim_loaded$bio19, other_weights)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {SP6.3} Testing alternative weights:
  expect_snapshot(
    withr::with_seed(
      123,
      ww_global_moran_i_vec(worldclim_loaded$bio13, worldclim_loaded$bio19, other_weights)
    )
  )

})
