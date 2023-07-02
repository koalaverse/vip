test_that("Local geary statistics are stable", {
  set.seed(123)

  guerry_modeled <- guerry
  guerry_lm <- lm(Crm_prs ~ Litercy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  weights <- ww_build_weights(guerry)

  resid <- guerry_modeled$Crm_prs - guerry_modeled$predictions

  expect_snapshot({
    df_local_c <- ww_local_geary_c(guerry_modeled, Crm_prs, predictions)
    df_local_c[1:3]
  })

  set.seed(123)
  expect_snapshot({
    df_local_c_p <- ww_local_geary_pvalue(guerry_modeled, Crm_prs, predictions)
    df_local_c_p[1:3]
  })

  expect_snapshot(
    (vec_local_c <- ww_local_geary_c_vec(guerry_modeled$Crm_prs, guerry_modeled$predictions, weights))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_c_p <- ww_local_geary_pvalue_vec(guerry_modeled$Crm_prs, guerry_modeled$predictions, weights))
  )

  expect_identical(
    df_local_c$.estimate,
    vec_local_c
  )

  expect_identical(
    df_local_c_p$.estimate,
    vec_local_c_p
  )

  #' @srrstats {G5.4} Testing against spdep
  #' @srrstats {G5.5} Run with a consistent seed
  set.seed(123)
  spdep_output <- spdep::localC_perm(resid, weights)

  expect_identical(
    vec_local_c,
    as.vector(spdep_output)
  )

  expect_identical(
    vec_local_c_p,
    as.vector(attr(spdep_output, "pseudo-p")[, "Pr(z != E(Ci))"])
  )
})
