# expected failures

    Code
      tidy_importance(list())
    Condition
      Error in `tidy_importance()`:
      ! Can't construct a tidy importance table from an object of class list

---

    Code
      tidy_importance(data.frame())
    Condition
      Error:
      ! 'term' and 'estimate' must be columns in `importance`

