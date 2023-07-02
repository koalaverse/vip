# srr: ww_willmott_d1 errors if truth and estimate are different lengths

    Code
      ww_willmott_d1_vec(1:5, 1:4)
    Condition
      Error in `yardstick_vec()`:
      ! Length of `truth` (5) and `estimate` (4) must match.

---

    Code
      ww_willmott_d1_vec(1:4, 1:5)
    Condition
      Error in `yardstick_vec()`:
      ! Length of `truth` (4) and `estimate` (5) must match.

# srr: ww_willmott_d1 errors if truth and estimate aren't numeric

    Code
      ww_willmott_d1(char_df, x, y)
    Condition
      Error in `ww_willmott_d1()`:
      ! `estimate` must be numeric.

---

    Code
      ww_willmott_d1(char_df, y, x)
    Condition
      Error in `ww_willmott_d1()`:
      ! `truth` must be numeric.

---

    Code
      ww_willmott_d1_vec(as.character(1:5), 1:4)
    Condition
      Error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_willmott_d1_vec(1:5, as.character(1:4))
    Condition
      Error in `yardstick_vec()`:
      ! `estimate` must be numeric.

# srr: ww_willmott_d1 errors if truth and estimate are list columns

    Code
      ww_willmott_d1(list_df, x, y)
    Condition
      Error in `ww_willmott_d1()`:
      ! `estimate` must be numeric.

---

    Code
      ww_willmott_d1(list_df, y, x)
    Condition
      Error in `ww_willmott_d1()`:
      ! `truth` must be numeric.

# srr: ww_willmott_d1 removes NaN and NA when na_rm = TRUE

    Code
      round(ww_willmott_d1(missing_df, x, y)$.estimate, 15)
    Output
      [1] 1

---

    Code
      round(ww_willmott_d1(missing_df, y, x)$.estimate, 15)
    Output
      [1] 1

---

    Code
      round(ww_willmott_d1_vec(missing_df$y, missing_df$x), 15)
    Output
      [1] 1

---

    Code
      round(ww_willmott_d1_vec(missing_df$x, missing_df$y), 15)
    Output
      [1] 1

# srr: ww_willmott_d1 errors on zero-length data

    Code
      ww_willmott_d1_vec(numeric(), numeric())
    Condition
      Error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_willmott_d1(empty_df, x, y)
    Condition
      Error in `ww_willmott_d1()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_willmott_d1(empty_df, y, x)
    Condition
      Error in `ww_willmott_d1()`:
      ! 0 non-missing values were passed to `truth`.

# srr: ww_willmott_d1 errors on all-NA data

    Code
      ww_willmott_d1_vec(rep(NA_real_, 4), 4:1)
    Condition
      Error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_willmott_d1_vec(1:4, rep(NA_real_, 4))
    Condition
      Error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_willmott_d1(all_na, x, y)
    Condition
      Error in `ww_willmott_d1()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_willmott_d1(all_na, y, x)
    Condition
      Error in `ww_willmott_d1()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_willmott_d1_vec(1:4, 1:4)
    Output
      [1] 1

# srr: ww_willmott_d1 works with all identical data

    Code
      ww_willmott_d1(all_identical, x, y)
    Output
      # A tibble: 1 x 3
        .metric     .estimator .estimate
        <chr>       <chr>          <dbl>
      1 willmott_d1 standard           1

---

    Code
      ww_willmott_d1_vec(1:4, 1:4)
    Output
      [1] 1

---

    Code
      ww_willmott_d1(all_identical, x, y)
    Output
      # A tibble: 1 x 3
        .metric     .estimator .estimate
        <chr>       <chr>          <dbl>
      1 willmott_d1 standard           1

