test_that("ww_make_point_neighbors is stable", {
  guerry_pt <- sf::st_geometry(guerry)
  guerry_pt <- sf::st_centroid(guerry_pt)
  expect_snapshot(
    ww_make_point_neighbors(guerry_pt)
  )

  expect_snapshot(
    ww_make_point_neighbors(guerry_pt, k = 2)
  )
})

test_that("ww_make_polygon_neighbors is stable", {
  expect_snapshot(
    ww_make_polygon_neighbors(guerry)
  )

  expect_snapshot(
    ww_make_polygon_neighbors(guerry, queen = FALSE)
  )
})

test_that("ww_build_neighbors is stable", {
  expect_snapshot(
    ww_build_neighbors(guerry)
  )

  expect_identical(
    ww_build_neighbors(sf::st_sfc(), ww_build_neighbors(guerry)),
    ww_build_neighbors(guerry)
  )

  guerry_pt <- sf::st_geometry(guerry)
  guerry_pt <- sf::st_centroid(guerry_pt)
  expect_snapshot(
    ww_build_neighbors(guerry_pt)
  )

  expect_snapshot(
    ww_build_neighbors(sf::st_cast(guerry, "MULTILINESTRING")),
    error = TRUE
  )

  expect_snapshot(
    ww_build_neighbors(guerry, function(data) data),
    error = TRUE
  )
})

test_that("ww_build_weights is stable", {
  expect_snapshot(
    ww_build_weights(guerry)
  )

  expect_identical(
    ww_build_weights(guerry, ww_build_weights(guerry)),
    ww_build_weights(guerry)
  )

  guerry_pt <- sf::st_geometry(guerry)
  guerry_pt <- sf::st_centroid(guerry_pt)
  expect_snapshot(
    ww_build_weights(guerry_pt)
  )

  expect_snapshot(
    ww_build_weights(sf::st_cast(guerry, "MULTILINESTRING")),
    error = TRUE
  )

  expect_snapshot(
    ww_build_weights(guerry, function(data) data),
    error = TRUE
  )

  expect_snapshot(
    ww_build_weights(guerry, include_self = TRUE)
  )
})

test_that("expected_errors", {
  expect_snapshot(
    ww_build_neighbors(as.data.frame(guerry)),
    error = TRUE
  )

  expect_snapshot(
    ww_make_point_neighbors(as.data.frame(guerry)),
    error = TRUE
  )

  expect_snapshot(
    ww_make_point_neighbors(guerry, k = c(1, 5)),
    error = TRUE
  )
})
