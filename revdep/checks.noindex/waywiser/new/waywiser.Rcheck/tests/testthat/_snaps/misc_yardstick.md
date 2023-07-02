# passing functions to build weights

    Code
      df_local_i <- ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions, wt = ww_build_weights)
      df_local_i[1:3]
    Output
      # A tibble: 85 x 3
         .metric           .estimator .estimate
         <chr>             <chr>          <dbl>
       1 local_getis_ord_g standard       0.913
       2 local_getis_ord_g standard       2.49 
       3 local_getis_ord_g standard       2.15 
       4 local_getis_ord_g standard      -1.58 
       5 local_getis_ord_g standard      -1.19 
       6 local_getis_ord_g standard      -1.68 
       7 local_getis_ord_g standard       0.627
       8 local_getis_ord_g standard      -1.60 
       9 local_getis_ord_g standard       0.964
      10 local_getis_ord_g standard      -2.71 
      # i 75 more rows

# edge cases

    Code
      ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions, wt = list())
    Condition
      Error in `ww_local_getis_ord_g()`:
      ! `wt` must be a 'listw' object
      i You can create 'listw' objects using `ww_build_weights()`

---

    Code
      ww_local_getis_ord_g_vec(as.character(crm), prd, structure(list(), class = "listw"))
    Condition
      Error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_local_getis_ord_g_vec(crm, as.character(prd), structure(list(), class = "listw"))
    Condition
      Error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_local_getis_ord_g_vec(as.matrix(crm), prd, structure(list(), class = "listw"))
    Condition
      Error in `yardstick_vec()`:
      ! `truth` must be a numeric vector.

---

    Code
      ww_local_getis_ord_g_vec(crm, as.matrix(prd), structure(list(), class = "listw"))
    Condition
      Error in `yardstick_vec()`:
      ! `estimate` must be a numeric vector.

---

    Code
      ww_local_getis_ord_g_vec(crm, numeric(), structure(list(), class = "listw"))
    Condition
      Error in `yardstick_vec()`:
      ! Length of `truth` (85) and `estimate` (0) must match.

---

    Code
      ww_local_getis_ord_g_vec(crm, prd, structure(list(), class = "listw"),
      na_action = na.omit)
    Condition
      Error in `spatial_yardstick_vec()`:
      ! Missing values in data.
      i waywiser can't handle missing data for functions that use spatial weights.

---

    Code
      withr::with_seed(123, ww_local_getis_ord_g_vec(crm, prd, structure(list(),
      class = "listw"), na_action = function(x) runif(sample(1:100, sample(1:100, 1)))))
    Condition
      Error in `spatial_yardstick_vec()`:
      ! Missing values in data.
      i waywiser can't handle missing data for functions that use spatial weights.

