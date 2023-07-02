test_that("Willmott's D estimates are the same across methods", {
  x <- c(6, 8, 9, 10, 11, 14)
  y <- c(2, 3, 5, 5, 6, 8)
  df <- data.frame(x = x, y = y)

  sim <- c(5, 7, 9, 2, 4.5, 6.7)
  obs <- c(4.7, 6, 10, 2.5, 4, 7)

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4} This comparison is made against a value calculated using hydroGOF::d()
  #' @srrstats {G5.4b} This comparison is made against a value calculated using hydroGOF::d()
  #' @srrstats {G5.4c} This comparison is made against a value calculated using hydroGOF::d()
  expect_equal(
    ww_willmott_d_vec(y, x),
    0.5137892
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4} This comparison is made against a value calculated using metrica::d1()
  #' @srrstats {G5.4b} This comparison is made against a value calculated using metrica::d1()
  #' @srrstats {G5.4c} This comparison is made against a value calculated using metrica::d1()
  expect_equal(
    ww_willmott_d1_vec(y, x),
    0.2434783,
    tolerance = 1e-6
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4} This comparison is made against the implementation at https://hydroerr.readthedocs.io/en/latest/api/HydroErr.HydroErr.dr.html
  #' @srrstats {G5.4b} This comparison is made against the implementation at https://hydroerr.readthedocs.io/en/latest/api/HydroErr.HydroErr.dr.html
  #' @srrstats {G5.4c} This comparison is made against the implementation at https://hydroerr.readthedocs.io/en/latest/api/HydroErr.HydroErr.dr.html
  expect_equal(
    ww_willmott_dr_vec(obs, sim),
    0.847457627118644
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4} These comparisons test that combining the systematic and unsystematic components of MSE sum to RMSE, implemented in yardstick
  expect_equal(
    ww_systematic_mse_vec(y, x) + ww_unsystematic_mse_vec(y, x),
    yardstick::rmse_vec(y, x)**2
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_systematic_rmse_vec(y, x)**2 + ww_unsystematic_rmse_vec(y, x)**2,
    yardstick::rmse_vec(y, x)**2
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_willmott_d_vec(x, y),
    ww_willmott_d(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_willmott_d1_vec(x, y),
    ww_willmott_d1(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_willmott_dr_vec(x, y),
    ww_willmott_dr(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_systematic_mse_vec(x, y),
    ww_systematic_mse(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_unsystematic_mse_vec(x, y),
    ww_unsystematic_mse(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_systematic_rmse_vec(x, y),
    ww_systematic_rmse(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  expect_equal(
    ww_unsystematic_rmse_vec(x, y),
    ww_unsystematic_rmse(df, x, y)$.estimate
  )
})
