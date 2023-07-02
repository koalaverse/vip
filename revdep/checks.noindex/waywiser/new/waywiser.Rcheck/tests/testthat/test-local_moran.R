set.seed(123)
test_that("Local Moran statistics are stable", {
  guerry_modeled <- guerry
  guerry_lm <- lm(Crm_prs ~ Litercy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  weights <- ww_build_weights(guerry)

  resid <- guerry_modeled$Crm_prs - guerry_modeled$predictions

  expect_snapshot({
    df_local_i <- ww_local_moran_i(guerry_modeled, Crm_prs, predictions)
    df_local_i[1:3]
  })

  expect_snapshot({
    df_local_i_p <- ww_local_moran_pvalue(guerry_modeled, Crm_prs, predictions)
    df_local_i_p[1:3]
  })

  expect_snapshot(
    (vec_local_i <- ww_local_moran_i_vec(guerry_modeled$Crm_prs, guerry_modeled$predictions, weights))
  )

  expect_snapshot(
    (vec_local_i_p <- ww_local_moran_pvalue_vec(guerry_modeled$Crm_prs, guerry_modeled$predictions, weights))
  )

  expect_identical(
    df_local_i$.estimate,
    as.vector(vec_local_i)
  )

  expect_identical(
    df_local_i_p$.estimate,
    as.vector(vec_local_i_p)
  )

  #' @srrstats {G5.4} Testing against spdep
  #' @srrstats {G5.5} Run with a consistent seed
  set.seed(123)
  spdep_output <- spdep::localmoran(resid, weights)

  expect_identical(
    vec_local_i,
    as.vector(spdep_output[, 1])
  )

  expect_identical(
    vec_local_i_p,
    as.vector(spdep_output[, 5])
  )
})
