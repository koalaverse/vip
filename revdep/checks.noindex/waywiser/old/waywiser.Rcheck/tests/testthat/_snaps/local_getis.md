# Local Getis-Ord statistics are stable

    Code
      df_local_i <- ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions)
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

---

    Code
      df_local_i_p <- ww_local_getis_ord_g_pvalue(guerry_modeled, Crm_prs,
        predictions)
      df_local_i_p[1:3]
    Output
      # A tibble: 85 x 3
         .metric                  .estimator .estimate
         <chr>                    <chr>          <dbl>
       1 local_getis_ord_g_pvalue standard     0.339  
       2 local_getis_ord_g_pvalue standard     0.0175 
       3 local_getis_ord_g_pvalue standard     0.0470 
       4 local_getis_ord_g_pvalue standard     0.136  
       5 local_getis_ord_g_pvalue standard     0.253  
       6 local_getis_ord_g_pvalue standard     0.0943 
       7 local_getis_ord_g_pvalue standard     0.541  
       8 local_getis_ord_g_pvalue standard     0.132  
       9 local_getis_ord_g_pvalue standard     0.361  
      10 local_getis_ord_g_pvalue standard     0.00787
      # i 75 more rows

---

    Code
      (vec_local_i <- ww_local_getis_ord_g_vec(guerry_modeled$Crm_prs, guerry_modeled$
        predictions, weights))
    Output
       [1]  0.91288164  2.49305353  2.14692501 -1.57511235 -1.18988831 -1.67703329
       [7]  0.62706195 -1.60139345  0.96397088 -2.71475331 -3.05125861 -1.64429475
      [13]  1.15584938 -2.90161984 -0.63376101  0.61005430  2.29888368 -0.28799789
      [19]  1.53012357  1.22699107  0.59816133 -0.65331166  0.87501663 -1.75661589
      [25]  0.39100454  0.52017062  1.16424138 -3.20781683 -2.29583912 -0.98116177
      [31]  0.06685550 -2.13635236  2.30311768  0.63548278  1.99855789 -0.24366435
      [37]  1.55058791  0.17231008  1.52062197 -0.22999799 -1.29501163  1.08298736
      [43]  0.13063616 -1.21798455 -1.00549332 -2.29306942  0.89419782  1.19832028
      [49]  2.03154442  0.87398122  2.88484032 -0.76194353  1.52453019  1.36764146
      [55] -0.06618369  1.47010332  1.89277905  0.52529402  1.63007385  2.00852739
      [61]  1.14763247 -0.91644996 -0.49669001 -1.85515687 -1.00553207 -0.86081555
      [67]  1.63219209  1.04953518  2.51838763  1.71259713 -0.49967695  1.35387348
      [73] -0.33336379 -0.07498653  0.56058846  1.54116260 -1.91772217 -1.92601432
      [79] -1.62505600 -2.60384397  0.42262985  1.16021704  1.46229964  0.06056077
      [85]  1.08651166

---

    Code
      (vec_local_i_p <- ww_local_getis_ord_g_pvalue_vec(guerry_modeled$Crm_prs,
      guerry_modeled$predictions, weights))
    Output
       [1] 0.338551650 0.017476897 0.046989319 0.136306243 0.252759855 0.094280941
       [7] 0.540950240 0.131507964 0.360573261 0.007868825 0.002910297 0.099812583
      [13] 0.263145960 0.006073623 0.486840848 0.592079221 0.024897187 0.769018856
      [19] 0.147064254 0.236079422 0.566712648 0.491109230 0.430473715 0.079265433
      [25] 0.725081238 0.578327557 0.252363140 0.001922916 0.033158491 0.345911465
      [31] 0.980042792 0.026524059 0.020365959 0.457589915 0.049929480 0.842223381
      [37] 0.122974384 0.896634027 0.134780339 0.852933638 0.197999212 0.283637329
      [43] 0.908861719 0.251729216 0.309816435 0.029894833 0.388820107 0.244965500
      [49] 0.055290126 0.358699720 0.004749006 0.419371223 0.135285432 0.172833170
      [55] 0.889256614 0.166182979 0.052600987 0.625967137 0.123280301 0.042396581
      [61] 0.263286551 0.358596865 0.672797323 0.064221214 0.333500223 0.360499055
      [67] 0.106951191 0.292784000 0.020687637 0.123175709 0.582501078 0.195554540
      [73] 0.718377209 0.944328466 0.556059177 0.167383256 0.060941608 0.081390165
      [79] 0.130453836 0.012380565 0.707571437 0.277582701 0.190935930 0.922672415
      [85] 0.296789212

---

    Code
      df_local_i <- ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions,
        weights)
      df_local_i[1:3]
    Output
      # A tibble: 85 x 3
         .metric               .estimator .estimate
         <chr>                 <chr>          <dbl>
       1 local_getis_ord_gstar standard       1.35 
       2 local_getis_ord_gstar standard       2.64 
       3 local_getis_ord_gstar standard       2.33 
       4 local_getis_ord_gstar standard      -1.84 
       5 local_getis_ord_gstar standard      -1.19 
       6 local_getis_ord_gstar standard      -2.06 
       7 local_getis_ord_gstar standard       1.58 
       8 local_getis_ord_gstar standard      -2.32 
       9 local_getis_ord_gstar standard       0.880
      10 local_getis_ord_gstar standard      -2.74 
      # i 75 more rows

---

    Code
      df_local_i_p <- ww_local_getis_ord_g_pvalue(guerry_modeled, Crm_prs,
        predictions, weights)
      df_local_i_p[1:3]
    Output
      # A tibble: 85 x 3
         .metric                      .estimator .estimate
         <chr>                        <chr>          <dbl>
       1 local_getis_ord_gstar_pvalue standard     0.353  
       2 local_getis_ord_gstar_pvalue standard     0.0153 
       3 local_getis_ord_gstar_pvalue standard     0.0447 
       4 local_getis_ord_gstar_pvalue standard     0.124  
       5 local_getis_ord_gstar_pvalue standard     0.257  
       6 local_getis_ord_gstar_pvalue standard     0.104  
       7 local_getis_ord_gstar_pvalue standard     0.579  
       8 local_getis_ord_gstar_pvalue standard     0.100  
       9 local_getis_ord_gstar_pvalue standard     0.324  
      10 local_getis_ord_gstar_pvalue standard     0.00724
      # i 75 more rows

---

    Code
      (vec_local_i <- ww_local_getis_ord_g_vec(guerry_modeled$Crm_prs, guerry_modeled$
        predictions, weights))
    Output
       [1]  1.35371776  2.64470358  2.33101218 -1.83696218 -1.19214894 -2.06145107
       [7]  1.58230958 -2.31764702  0.88028873 -2.74035690 -3.39292895 -1.87812026
      [13]  0.85856419 -2.81583254 -0.31227470  0.49524512  2.24081985 -0.51676147
      [19]  2.00481255  1.63825389  1.39112875 -0.55103244  0.08003347 -1.97182396
      [25]  0.11106013  0.56430623  1.71460247 -3.33281642 -2.22026799 -0.98578659
      [31]  0.30960568 -2.34956428  2.25994512  1.20029086  1.79507717 -0.29168508
      [37]  1.78306056  0.01327693  1.48600121  0.13091399 -1.40966303  0.93339994
      [43]  0.02027691 -1.82812173 -0.79364697 -2.75575017  1.26343283  1.74008875
      [49]  1.69116164  1.14388681  3.09199200 -0.26702307  1.66914599  1.42530173
      [55] -0.65613591  1.62256416  2.11228076  0.90733590  1.93759338  1.91124325
      [61]  0.92968354 -1.01687632 -0.94802983 -2.54791742 -1.35286802 -1.66942606
      [67]  1.40993406  1.10799183  2.75546130  2.26572376 -0.86272825  1.07976560
      [73] -0.21434998 -0.43824366  0.43342398  2.13948993 -2.14931849 -2.06450852
      [79] -1.88711642 -2.74970443  0.42882579  0.82915623  1.17030359  0.01011938
      [85]  0.90033560

---

    Code
      (vec_local_i_p <- ww_local_getis_ord_g_pvalue_vec(guerry_modeled$Crm_prs,
      guerry_modeled$predictions, weights))
    Output
       [1] 0.353320976 0.015320807 0.044659841 0.124494604 0.257380879 0.104374603
       [7] 0.579386974 0.100499213 0.324430137 0.007238788 0.003024914 0.089786809
      [13] 0.245293341 0.001554081 0.515887921 0.580670550 0.028218825 0.782467003
      [19] 0.148454172 0.225639656 0.571064419 0.540603232 0.425964966 0.102290000
      [25] 0.750018977 0.653242771 0.239989113 0.001853200 0.019877161 0.378631358
      [31] 0.895027710 0.026860310 0.029574160 0.542892507 0.049082054 0.843613753
      [37] 0.119831188 0.922323909 0.121905018 0.802914179 0.238752158 0.286284115
      [43] 0.901178086 0.246053959 0.336073064 0.017506445 0.428596576 0.209104318
      [49] 0.049489955 0.405297312 0.004827649 0.463676567 0.145834651 0.166071593
      [55] 0.952182978 0.177860755 0.070212352 0.661584162 0.117573309 0.035904620
      [61] 0.249850749 0.301292619 0.623718225 0.073175337 0.343285322 0.455803772
      [67] 0.097245254 0.305824379 0.019794928 0.090672872 0.600765705 0.181230901
      [73] 0.778825479 0.987438813 0.557938792 0.135621556 0.055019155 0.049909299
      [79] 0.095463604 0.009723617 0.720748620 0.245515207 0.162155516 0.966175966
      [85] 0.314680103

