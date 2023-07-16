test_that("passing functions to build weights", {
  guerry_modeled <- guerry
  guerry_lm <- lm(Crm_prs ~ Litercy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  expect_snapshot({
    df_local_i <- ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions, wt = ww_build_weights)
    df_local_i[1:3]
  })
})

test_that("edge cases", {
  guerry_modeled <- guerry
  guerry_lm <- lm(Crm_prs ~ Litercy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  expect_snapshot(
    ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions, wt = list()),
    error = TRUE
  )

  crm <- guerry_modeled$Crm_prs
  prd <- guerry_modeled$predictions

  expect_snapshot(
    ww_local_getis_ord_g_vec(as.character(crm), prd, structure(list(), class = "listw")),
    error = TRUE
  )

  expect_snapshot(
    ww_local_getis_ord_g_vec(crm, as.character(prd), structure(list(), class = "listw")),
    error = TRUE
  )

  expect_snapshot(
    ww_local_getis_ord_g_vec(as.matrix(crm), prd, structure(list(), class = "listw")),
    error = TRUE
  )

  expect_snapshot(
    ww_local_getis_ord_g_vec(crm, as.matrix(prd), structure(list(), class = "listw")),
    error = TRUE
  )

  expect_snapshot(
    ww_local_getis_ord_g_vec(crm, numeric(), structure(list(), class = "listw")),
    error = TRUE
  )

  prd[4] <- NA
  expect_snapshot(
    ww_local_getis_ord_g_vec(
      crm,
      prd,
      structure(list(), class = "listw"),
      na_action = na.omit
    ),
    error = TRUE
  )

  expect_snapshot(
    withr::with_seed(
      123,
      ww_local_getis_ord_g_vec(
        crm,
        prd,
        structure(list(), class = "listw"),
        na_action = function(x) runif(sample(1:100, sample(1:100, 1)))
      )
    ),
    error = TRUE
  )
})
