# Global Moran statistics are stable

    Code
      df_global_i <- ww_global_moran_i(guerry_modeled, Crm_prs, predictions)
      df_global_i[1:3]
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 global_moran_i standard       0.412

---

    Code
      df_global_i_p <- ww_global_moran_pvalue(guerry_modeled, Crm_prs, predictions)
      df_global_i_p[1:3]
    Output
      # A tibble: 1 x 3
        .metric             .estimator .estimate
        <chr>               <chr>          <dbl>
      1 global_moran_pvalue standard    7.23e-10

---

    Code
      (vec_global_i <- ww_global_moran_i_vec(guerry_modeled$Crm_prs, guerry_modeled$
        predictions, weights))
    Output
      [1] 0.4115652

---

    Code
      (vec_global_i_p <- ww_global_moran_pvalue_vec(guerry_modeled$Crm_prs,
      guerry_modeled$predictions, weights))
    Output
      [1] 7.234758e-10

