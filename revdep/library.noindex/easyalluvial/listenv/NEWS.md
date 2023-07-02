# Version 0.9.0 [2022-12-15]

## New Features

 * Add `aperm()` and `t()` for list-environment arrays.

 * `parse_env_subset()` gained argument `is_variable` to control
   whether or not the inferred element named should be checked if it
   is a valid variable name.

## Internationalization

 * All warning and error messages are now translatable.

## Deprecated and Defunct

 * `map()` is formally deprecated; use `mapping()` instead.

 * Dropped the defunct usage of `listenv(length = n)`, meaning it no
   longer gives an error but instead produces a `listenv` with an
   element named `length`.


# Version 0.8.0 [2019-12-05]

## Signficant Changes

 * S3 method `lengths()` for `listenv` is no longer exported.


## New Features

 * Made several error messages more informative.  The downside is that
   those messages are no longer translated (because they are no longer
   aligned with built-in R error messages which have translations).


## Bug Fixes

 * `parse_env_subset(x[1, names])` on a listenv `x` matrix would throw
   error `Error in if (any(i < 0)) { : missing value where TRUE/FALSE
   needed` if one of the elements in `names` specifies a non-existing
   column name.
 
 * `parse_env_subset(x[])` on a listenv `x` would throw an error on
    `Invalid subset: x[]`.
 
 * `parse_env_subset(x[names])` on a listenv `x` would throw an error
   on `length(x) = 2 > 1 in coercion to logical(1)` when
   `length(names) > 1` and `_R_CHECK_LENGTH_1_LOGIC2_=true`.
 
 * `parse_env_subset(x[1,idxs])` on a listenv `x` would throw an error
    on `length(x) = 2 > 1 in coercion to logical(1)` with
    `length(idxs) > 1` and `_R_CHECK_LENGTH_1_LOGIC2_=true`.
 
 * `parse_env_subset(x[[names]])` on a regular environment `x` with
   `length(names) > 1` would not throw an error, whereas `x[[names]]`
   would.
 
 * `parse_env_subset(x[[1]])` on a regular environment `x` would not
   throw an error, whereas `x[[1]]` would.
 
 
# Version 0.7.0 [2018-01-21]
 
## New Features
 
 * Now it is possible to set the dimension on an empty list
   environment without first resizing it with `length()`, e.g. `x <-
   listenv(); dim(x) <- c(2, 3)`.
 
 * Now it is possible to remove multiple elements by assigning NULL,
   e.g.  `x[c(2:3, 10)] <- NULL` and `x[, "B"] <- NULL`.
 
 * Added `lengths()` for list environments.  Requires R (>= 3.3.0).
 
 * `dim_na(x) <- dims`, where `dims` contain exactly one missing
   value, will set the "missing" dimension based on the length of `x`
   and the other dimensions specified, e.g. with `length(x) == 6`,
   `dim_na(x) <- c(2, NA)` will set `dim(x) <- c(2, 3)`.  This works
   for all types of object to which dimensions can be assigned - not
   only list environments.
 
 * Added `is.matrix()`, `is.array()`, `as.vector()`, and `as.matrix()`
   for list environments.
 
 
## Bug Fixes
 
 * `print()` on a named, empty list environment would output an empty
   string.
 
 * Removing an element from a list environment did not remove
   dimensions, e.g. `x$a <- NULL`.
 
 
## Deprecated and Defunct
 
 * Function `map()` has been renamed to `mapping()` and same for the
   corresponding replacement function.  The `map()` and `map<-()`
   functions will soon be deprecated and eventually defunct.
 
 * `x <- listenv(length = n)` is defunct; use `x <- listenv();
   length(x) <- n` instead.
 
 
# Version 0.6.0 [2015-12-27]
 
## New Features
 
 * Added support for multi-dimensional subsetting of list environments
   just as for list.
 
 
## Bug Fixes
 
 * `parse_env_subset(x[[idx]])` for list environment `x` and index
   `idx` claimed `x[[idx]]` existed as long as `idx` in
   `[1,length(x)]`, but it forgot to check if element really existed,
   which may not be true if `x` has been expanded.
 
 
# Version 0.5.0 [2015-10-30]
 
## New Features
 
 * Add support for assigning elements when creating list environment
   similarly how to lists work, e.g. `x <- listenv(a = 1, b = 2)`.
 
 * `length(x) <- n` now expand/truncate a list environment.
 
 * Added `unlist()` and `all.equal()` for list environments.
 
 
## Deprecated and Defunct
 
 * Deprecated `x <- listenv(length = n)`; use `x <- listenv();
   length(x) <- n` instead.
 
 
## Bug Fixes
 
 * `as.listenv(x)` would drop NULL elements in `x`.
 
 * `x[idxs]`, `x[name] <- y`, and `x$<name> <- y` would introduce NA
   names for non-named list environments.
 
 
# Version 0.4.0 [2015-08-08]
 
## New Features
 
 * Added `as.listenv()`.
 
 * CONSISTENCY: Assigning NULL now removes element just as lists,
    e.g. `x$a <- NULL`. To assign value NULL, do `x['a'] <-
    list(NULL)`.
 
 * Added support for subsetting with `[()`, which returns another list
   environment, e.g. `x[2:3]`, `x[-1]`, and `x[c(TRUE, FALSE)]`.
 
 * Added `[<-` assignment, e.g. `x['a'] <- 1` and `x[2:3] <- c(3,8)`.
 
 * CLEANUP: Dropped stray debug code.
 
 
# Version 0.3.0 [2015-05-23]
 
## Code Refactorization
 
 * Package no longer depends on other packages.
 
 
# Version 0.2.4 [2015-05-22]
 
## New Features
 
 * Added helper function `parse_env_subset()`.
 
 
# Version 0.2.3 [2015-05-21]
 
## New Features
 
 * `print()` on `listenv` handles empty and no-named `listenv`:s
   better.
 
 
# Version 0.2.2 [2015-05-20]
 
## New Features
 
 * Now `listenv(length = ...)` always allocates internal variables.
 
 
# Version 0.2.1 [2015-05-19]
 
## New Features
 
 * `get_variable()` gained argument `mustExist`.
 
 
# Version 0.2.0 [2015-05-19]
 
## Signficant Changes
 
 * Moved list environments from an in-house package to its own package.
 
 
# Version 0.1.4 [2015-05-02]
 
## New Features
 
 * Added `print()` for `listenv`:s.
 
 
## Code Refactorization
 
 * Using `tempvar()` of **R.utils**.


# Version 0.1.0 [2015-02-07]

 * Created.
 
