test_that("agreement coefficients match Ji and Gallo", {
  #' @srrstats {G5.7} Algorithm performs as expected
  x <- c(6, 8, 9, 10, 11, 14)
  y <- c(2, 3, 5, 5, 6, 8)

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_agreement_coefficient_vec(x, y),
    0.475,
    tolerance = 0.01
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_systematic_agreement_coefficient_vec(x, y),
    0.478,
    tolerance = 0.01
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_unsystematic_agreement_coefficient_vec(x, y),
    0.996,
    tolerance = 0.01
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_systematic_mpd_vec(x, y),
    23.657,
    tolerance = 0.01
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_unsystematic_mpd_vec(x, y),
    0.177,
    tolerance = 0.01
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_systematic_rmpd_vec(x, y),
    4.864,
    tolerance = 0.01
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_unsystematic_rmpd_vec(x, y),
    0.420,
    tolerance = 0.01
  )
})

test_that("agreement coefficients are the same across methods", {
  x <- c(6, 8, 9, 10, 11, 14)
  y <- c(2, 3, 5, 5, 6, 8)
  df <- data.frame(x = x, y = y)

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_agreement_coefficient_vec(x, y),
    ww_agreement_coefficient(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_systematic_agreement_coefficient_vec(x, y),
    ww_systematic_agreement_coefficient(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_unsystematic_agreement_coefficient_vec(x, y),
    ww_unsystematic_agreement_coefficient(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_systematic_mpd_vec(x, y),
    ww_systematic_mpd(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_unsystematic_mpd_vec(x, y),
    ww_unsystematic_mpd(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_systematic_rmpd_vec(x, y),
    ww_systematic_rmpd(df, x, y)$.estimate
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.4c} Testing data is from the original paper.
  expect_equal(
    ww_unsystematic_rmpd_vec(x, y),
    ww_unsystematic_rmpd(df, x, y)$.estimate
  )
})
