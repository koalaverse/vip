# Global Geary statistics are stable

    Code
      df_global_c <- ww_global_geary_c(guerry_modeled, Crm_prs, predictions)
      df_global_c[1:3]
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 global_geary_c standard       0.565

---

    Code
      df_global_c_p <- ww_global_geary_pvalue(guerry_modeled, Crm_prs, predictions)
      df_global_c_p[1:3]
    Output
      # A tibble: 1 x 3
        .metric             .estimator .estimate
        <chr>               <chr>          <dbl>
      1 global_geary_pvalue standard    7.55e-10

---

    Code
      (vec_global_c <- ww_global_geary_c_vec(guerry_modeled$Crm_prs, guerry_modeled$
        predictions, weights))
    Output
      [1] 0.5654044

---

    Code
      (vec_global_c_p <- ww_global_geary_pvalue_vec(guerry_modeled$Crm_prs,
      guerry_modeled$predictions, weights))
    Output
      [1] 7.548865e-10

