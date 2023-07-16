test_that("Global Moran statistics are stable", {
  guerry_modeled <- guerry
  guerry_lm <- lm(Crm_prs ~ Litercy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  weights <- ww_build_weights(guerry)

  resid <- guerry_modeled$Crm_prs - guerry_modeled$predictions

  expect_snapshot({
    df_global_i <- ww_global_moran_i(guerry_modeled, Crm_prs, predictions)
    df_global_i[1:3]
  })

  expect_snapshot({
    df_global_i_p <- ww_global_moran_pvalue(guerry_modeled, Crm_prs, predictions)
    df_global_i_p[1:3]
  })

  expect_snapshot(
    (vec_global_i <- ww_global_moran_i_vec(guerry_modeled$Crm_prs, guerry_modeled$predictions, weights))
  )

  expect_snapshot(
    (vec_global_i_p <- ww_global_moran_pvalue_vec(guerry_modeled$Crm_prs, guerry_modeled$predictions, weights))
  )

  expect_identical(
    df_global_i$.estimate,
    vec_global_i
  )

  expect_identical(
    df_global_i_p$.estimate,
    vec_global_i_p
  )

  #' @srrstats {G5.4} Testing against spdep
  #' @srrstats {G5.5} Run with a consistent seed
  set.seed(123)
  spdep_output <- spdep::moran.test(resid, weights)

  expect_identical(
    vec_global_i,
    spdep_output$estimate[[1]]
  )

  expect_identical(
    vec_global_i_p,
    spdep_output$p.value
  )
})
