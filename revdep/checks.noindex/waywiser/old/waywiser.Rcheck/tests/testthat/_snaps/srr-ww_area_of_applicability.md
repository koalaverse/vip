# srr: expected failures for ww_area_of_applicability

    Code
      ww_area_of_applicability(y ~ ., train, test, importance)
    Condition
      Error in `ww_area_of_applicability()`:
      ! All variables in `data` and `testing` must inherit either numeric or integer classes.

---

    Code
      ww_area_of_applicability(train, test, importance)
    Condition
      Error:
      ! All predictors must be numeric.

---

    Code
      ww_area_of_applicability(comb_rset_no_y, importance = importance)
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `purrr::map()`:
      ! All predictors must be numeric.

---

    Code
      ww_area_of_applicability(y ~ ., train, test, importance)
    Condition
      Error in `model.frame.default()`:
      ! invalid type (list) for variable 'x3'

---

    Code
      ww_area_of_applicability(train, test, importance)
    Condition
      Error:
      ! All predictors must be numeric.

---

    Code
      ww_area_of_applicability(comb_rset_no_y, importance = importance)
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `purrr::map()`:
      ! All predictors must be numeric.

---

    Code
      ww_area_of_applicability(y ~ ., head(train, 0), test, importance)
    Condition
      Error in `create_aoa()`:
      ! 0 rows were passed as training data.

---

    Code
      ww_area_of_applicability(y ~ ., train, head(test, 0), importance)
    Condition
      Error in `create_aoa()`:
      ! 0 rows were passed as testing data.

---

    Code
      ww_area_of_applicability(head(train[2:11], 0), test[2:11], importance)
    Condition
      Error in `create_aoa()`:
      ! 0 rows were passed as training data.

---

    Code
      ww_area_of_applicability(train[2:11], head(test[2:11], 0), importance)
    Condition
      Error in `create_aoa()`:
      ! 0 rows were passed as testing data.

---

    Code
      ww_area_of_applicability(head(as.matrix(train[2:11]), 0), as.matrix(test[2:11]),
      importance)
    Condition
      Error in `create_aoa()`:
      ! 0 rows were passed as training data.

---

    Code
      ww_area_of_applicability(as.matrix(train[2:11]), head(as.matrix(test[2:11]), 0),
      importance)
    Condition
      Error in `create_aoa()`:
      ! 0 rows were passed as testing data.

---

    Code
      ww_area_of_applicability(y ~ ., train_na, test, importance)
    Condition
      Error in `create_aoa()`:
      ! Missing values in training data.
      i Either process your data to fix NA values, or set `na_rm = TRUE`.

---

    Code
      ww_area_of_applicability(y ~ ., train, test_na, importance)
    Condition
      Error in `check_di_testing()`:
      ! Missing values in testing data.
      i Either process your data to fix NA values, or set `na_rm = TRUE`.

---

    Code
      ww_area_of_applicability(train_na[2:11], test[2:11], importance)
    Condition
      Error in `create_aoa()`:
      ! Missing values in training data.
      i Either process your data to fix NA values, or set `na_rm = TRUE`.

---

    Code
      ww_area_of_applicability(train[2:11], test_na[2:11], importance)
    Condition
      Error in `check_di_testing()`:
      ! Missing values in testing data.
      i Either process your data to fix NA values, or set `na_rm = TRUE`.

---

    Code
      ww_area_of_applicability(as.matrix(train_na[2:11]), as.matrix(test[2:11]),
      importance)
    Condition
      Error in `create_aoa()`:
      ! Missing values in training data.
      i Either process your data to fix NA values, or set `na_rm = TRUE`.

---

    Code
      ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test_na[2:11]),
      importance)
    Condition
      Error in `check_di_testing()`:
      ! Missing values in testing data.
      i Either process your data to fix NA values, or set `na_rm = TRUE`.

---

    Code
      ww_area_of_applicability(comb_rset_no_y_train_na, importance = importance)
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `create_aoa()`:
      ! Missing values in training data.
      i Either process your data to fix NA values, or set `na_rm = TRUE`.

---

    Code
      ww_area_of_applicability(comb_rset_no_y, comb_rset_no_y_test_na, importance)
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `purrr::map()`:
      ! All predictors must be numeric.

---

    Code
      ww_area_of_applicability(y ~ ., train, train, importance)
    Condition
      Warning:
      The AOA threshold was 0, which is usually unexpected.
      i Did you accidentally pass the same data as testing and training?
    Output
      # Predictors:
         10
      Area-of-applicability threshold:
         0

---

    Code
      ww_area_of_applicability(train[2:11], train[2:11], importance)
    Condition
      Warning:
      The AOA threshold was 0, which is usually unexpected.
      i Did you accidentally pass the same data as testing and training?
    Output
      # Predictors:
         10
      Area-of-applicability threshold:
         0

---

    Code
      ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(train[2:11]),
      importance)
    Condition
      Warning:
      The AOA threshold was 0, which is usually unexpected.
      i Did you accidentally pass the same data as testing and training?
    Output
      # Predictors:
         10
      Area-of-applicability threshold:
         0

---

    Code
      ww_area_of_applicability(comb_rset_no_y_identical, importance = importance)
    Condition
      Warning:
      The AOA threshold was 0, which is usually unexpected.
      i Did you accidentally pass the same data as testing and training?
    Output
      # Predictors:
         10
      Area-of-applicability threshold:
         0

