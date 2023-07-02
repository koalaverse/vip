# Version 0.16.2 [2022-11-21]

## Documentation

 * Drop duplicated arguments from `help("walkAST")`.
 

# Version 0.16.1 [2022-08-28]

## Bug Fixes

 * `packagesOf()` for `Globals` failed to return the package of the
   globals if the global doesn't have a closure, e.g. `base::pi`
   and `data.table::.N`.


# Version 0.16.0 [2022-08-05]

## New Features

 * Add `[[<-` and `[<-` for `Globals`, to complement `$<-`.

## Reproducibility

 * All functions modifying a `Globals` object guarantee that the
   `where` and the `class` attributes are always the last two
   attributes and in that order.

## Bug Fixes

 * `c()` for `Globals` would lose the `where` environment for any
   functions appended.
  

# Version 0.15.1 [2022-06-24]

## Bug Fixes

 * `cleanup()` assumed it was safe to call `env$.packageName` on each
   scanned environment, but that might not be true. A classed
   environment could be such that `$()` gives an error, rather than
   returning something.


# Version 0.15.0 [2022-05-08]

## New Features

 * `globalsOf()` gained argument `locals`, which controls whether
   globals that exist in "local" environments of a function should be
   considered or not, e.g. in `f <- local({ a <- 1; function() a })`,
   should `a` be considered a global of `f()` or not.  For backward
   compatibility reasons, the default is `locals = TRUE`, but this
   might become `locals = FALSE` in a later release.

 * Any `globals.*` options specific to this packages can now be set
   via environment variables `R_GLOBALS_*` when the package is loaded.
   For example, `R_GLOBALS_DEBUG=true` sets option `globals.debug =
   TRUE`.

## Bug Fixes

 * `as.Globals(list(a = NULL))` and `c(Globals(), list(a = NULL))`
   would include the calling environment instead of an empty
   environment as part of the `where` attribute.
   

# Version 0.14.0 [2020-11-22]

## New Features

 * Now `findGlobals(function(x) x <- x)` identifies `x` as a global
   variable.

 * Now `findGlobals(function(x) x[1] <- 0)` identifies `x` as a global
   variable.  Same for other variants like `x[[1]] <- 0` and `x$a <- 0`.

 * Now `findGlobals(function(z) x <- z$x)` identifies `x` as a global
   variable.

 * Now `findGlobals(quote({ f <- function(x) x; x }))` identifies `x`
   as a global variable.  Previously, the `x` of the function would
   hide the global `x`.


# Version 0.13.1 [2020-10-11]

## Bug Fixes

 * `globalsOf()` could produce "Error in vapply(where, FUN = envname,
   FUN.VALUE = NA_character_, USE.NAMES = FALSE) : values must be
   length 1, but FUN(X[[2]]) result is length 10".  This would happen
   if for instance argument `envir` has attributes set.

 * `findGlobals()` works around a bug in `stats:::[.formula` of R (<
   4.1.0) that revealed itself when scanning formulas with NULL
   components.

 * `findGlobals()` would not pass down argument `dotdotdot` when
   recursively parsing assignments.
   
 * `findGlobals()` could return `...` as a global also when used in
   formulas.  Now it respects argument `dotdotdot = "ignore"` and
   parses formulas accordingly, otherwise formulas will be parsed
   using `dotdotdot = "return"`.
   

# Version 0.13.0 [2020-09-16]

## Significant Changes

 * `findGlobals(expr)` now also scans any attributes of `expr` for
   globals, e.g.  `purrr::partial()` puts the original function in
   attribute `body`.  Argument `attributes` controls which attributes,
   if any, should be scanned.  Default is to scan all attributes.

 * `findGlobals()`, `globalsOf()`, and `globalsByName()` now
   recognizes and returns values for `..1`, `..2`, etc. like they do
   for `...`.

 * `cleanup()` now also drop exported and non-exported
   `NativeSymbolInfo` objects.

## New Features

 * `cleanup()` gained support for dropping `NativeSymbolInfo` objects.

## Bug Fixes

 * `findGlobals()` did not pass down argument `method` in recursive
   calls.

 * `findGlobals(expr)` would fail to identify globals in anonymous
   function calls, e.g. `expr <- as.call(list(function(...) NOT_FOUND,
   quote(FOUND)))`.

 * Calls like `findGlobals(~ NULL)` with NULLs on the right-hand side
   could throw "Error in if (length(ans) == 0L ||
   as.character(ans[[1L]])[1L] == "~") { : missing value where
   TRUE/FALSE needed". Solved by working around what looks like a bug
   in the **stats** package causing subsetting on formulas with NULLs to
   fail.

 * `cleanup(..., drop = c(..., "base-packages"))` for `Globals` would
   drop base R objects with names not exported by the corresponding
   base R package.  Similarly, `drop = c(..., "primitive")` would drop
   primitive R objects with names not exported by any base R package.

 * `findGlobals()`, `globalsOf()`, and `globalsByName()` did not
   handle `..1`, `..2`, etc.

 * `findGlobals()` and `globalsOf()` produces warnings on
   '<anonymous>: ... may be used in an incorrect context' when
   formulas had `...`, `..1`, `..2`, etc.

 * `findGlobals(function() NULL, substitute = TRUE, trace = TRUE)`
   would throw "Error in environment(w$enterLocal) : object 'w' not
   found".


# Version 0.12.5 [2019-12-07]

## Bug Fixes

 * `findGlobals(function() { a; a <- a + 1 })` would fail to identify
   `a` as a global variable whereas it was properly identified with `{
   a <- a + 1; a }`.
 

# Version 0.12.4 [2018-10-11]
 
## Bug Fixes

 * `globalsOf()` could produce "Error in vapply(where, FUN = envname,
   FUN.VALUE = NA_character_, USE.NAMES = FALSE) : values must be
   length 1, but FUN(X[[...]]) result is length ...".  This was
   because the internal `envname(env)` did not always handle when
   `class(env) != "environment"`.
   

# Version 0.12.3 [2018-09-16]

## New Features

 * `findGlobals()`, `globalsOf()`, and `packagesOf()` no longer
   return elements sorted by name.
    
## Bug Fixes
  
 * globals::`findGlobals()` would not identify `a` as a global in
   expressions of type `a[1] = ...` and `names(a) = ...` although it
   did for `a[1] <- ...` and `names(a) <- ...`.


# Version 0.12.2 [2018-08-25]

## Performance

 * `cleanup()` for `Globals` should now be much faster. Previously,
   it could be very slow the first time it was called in a fresh R
   session, especially if the user had a large number of packages
   installed and/or the package libraries were on slow drives.

## Documentation

 * Added help for `globals::findGlobals()`.
  
## Bug Fixes

 * `globals::findGlobals(x)`, where `x` is a list, iterated over `x`
   incorrectly assuming no method dispatching on `x` would take
   place. For instance, if `x` contained an `fst::fst_table` object,
   then "Error in .subset2(x, i, exact = exact) : subscript out of
   bounds" would be produced.
   
 * globals::`findGlobals()` could produce a "Warning in is.na(x):
   is.na() applied to non-(list or vector) of type 'NULL'" in R (<
   3.5.0).


# Version 0.12.1 [2018-06-24]

## Performance

 * globals::`findGlobals()` is now significantly faster for elements
   that are long lists with many elements of basic data types.  This
   is because elements of such basic data type cannot contain globals
   and can therefore be skipped early in the search for globals.
    

# Version 0.12.0 [2018-06-12]

## New Features

 * Now globals::`findGlobals()` identifies `a` as a global also when
   it is part of LHS expressions of type `a[1] <- ...` and `names(a)
   <- ...`.

## Bug Fixes

 * globals::`findGlobals()` incorrectly identified `a` as a global in
   expression of type `a <- pkg::a`.

 * If `...` was passed to `globalsByName(names)`, an error would be
   produced unless it was the last entry in `names`.


# Version 0.11.0 [2018-01-09]

## New Features

 * Now `findGlobals()` identifies `x` as a global variable in 
   `x <- x + 1` and likewise for `x + 1 -> x`.  Note that ditto
   using `<<-` and `->>` was already identifying `x` as a global.

## Bug Fixes

 * `findGlobals(..., trace = TRUE)` now outputs only to standard
   error.  Previously, some of the output went to standard output.


# Version 0.10.3 [2017-10-12]

## Bug Fixes

 * `globalsOf(..., recursive = TRUE)` would result in "Error in
   match.fun(FUN) : node stack overflow" if one of the globals
   identified was a function that called itself recursively (either
   directly or indirectly).


# Version 0.10.2 [2017-08-08]

## Bug Fixes

 * `walkAST()` could produce error "Cannot walk expression. Unknown
   object type '...'" for objects of type `environment`.


# Version 0.10.1 [2017-07-01]

## Bug Fixes

 * `walkAST()` could produce error "Cannot walk expression. Unknown
   object type '...'" for objects of type `list`, `expression` and
   `S4`.


# Version 0.10.0 [2017-04-16]

## New Features

 * Globals that are part of a formula are now identified.

 * `findGlobals(..., trace = TRUE)` will now show low-level parse
   information as the abstract syntax tree (AST) is walked.

SOFTWARE QUALITY:

 * Enabled more internal sanity checks.
  
## Bug Fixes

 * `walkAST()` could produce error "Cannot walk expression. Unknown
   object type 'nnn'" for expressions of type `builtin`, `closure`
   and `special`.
    

# Version 0.9.0 [2017-03-09]

## New Features

 * Added option `globals.debug`, which when TRUE enables debugging output.
  
## Bug Fixes

 * `globalsOf(..., recursive = TRUE)` would in some cases scan an
    incorrect subset of already identified globals.

 * `globalsOf(..., recursive = TRUE)` failed to skip objects part of
   package namespaces that where defined via a `local()` statement.


# Version 0.8.0 [2017-01-14]

## New Features

 * `globalsOf()` identifies also globals in locally defined
   functions.  This can be disabled with argument `recursive =
   FALSE`.

 * `findGlobals()` now takes both closures (functions) and
   expressions.
  

# Version 0.7.2 [2016-12-28]

## Bug Fixes

 * `c(x, list())` where `x` is a `Globals` object would give an error
   reporting that the list does not have named elements.
  
  
# Version 0.7.1 [2016-10-13]

## New Features

 * `Globals()` and `as.Globals()` now accepts an empty list as input
   as well.

## Bug Fixes

 * `walkAST(quote( function(x=NULL) 0 ))` would give a sanity check
   error due to the NULL argument.  Thank you GitHub user 'billy34'
   for reporting on this.
  
  
# Version 0.7.0 [2016-09-08]

## New Features

 * Added `walkAST()`, which can be used to tweak expressions.

 * Added `globalsByName()` for locating and retrieving a set of known
   global variables.

 * Added `c()`, `$<-()`, `names()`, `unique()` for `Globals` objects.

 * Improved `as.Globals()` for lists.
  
  
# Version 0.6.1 [2016-01-31]

## New Features

 * Now the error message of `globalsOf(..., mustExist = TRUE)` when
   it fails to locate a global also gives information on the
   expression that is problematic.

## Bug Fixes

 * `cleanup()` for `Globals` did not cleanup functions in core package
   environments named `package:<name>`.
  
  
# Version 0.6.0 [2015-12-12]

## New Features

 * `findGlobals()` is updated to handle the case where a local
   variable is overwriting a global one with the same name, e.g. `{ a
   <- b; b <- 1 }`.  Now `b` is correctly identified as a global
   object.  Previously it would have been missed.  For backward
   compatibility, the previous behavior can be obtained using
   argument `method = "conservative"`.
  
  
# Version 0.5.0 [2015-10-13]

## New Features

 * `globalsOf()` now returns attribute `where` specifying where each
   global object is located.

## Bug Fixes

 * `cleanup()` now only drops objects that are *located* in one of
   the "base" packages; previously it would also drop copies of such
   objects, e.g.  `FUN <- base::sample`.
 
  
# Version 0.4.1 [2015-10-05]

## Bug Fixes

 * `globalsOf()` failed to return global variables with value NULL.
   They were identified but silently dropped.
  
  
# Version 0.4.0 [2015-09-12]

## New Features

 * `findGlobals()` and `globalsOf()` gained argument `dotdotdot`.

  
# Version 0.3.1 [2015-06-10]

 * More test coverage.
  
  
# Version 0.3.0 [2015-06-08]

## New Features

 * Renamed `getGlobals()` to `globalsOf()`.
  
  
# Version 0.2.3 [2015-06-08]

## New Features

 * Added `[()` for `Globals`.

 * `findGlobals()` and `getGlobals()` gained argument `substitute`.

 * Added `cleanup(..., method = "internals")`.
  
  
# Version 0.2.2 [2015-05-20]

## New Features

 * Added `Globals` class with methods `cleanup()` and `packagesOf()`.
  
 * Added `as.Globals()` to coerce lists to `Globals` objects.
 
  
# Version 0.2.1 [2015-05-20]

## New Features

 * `getGlobals()` gained argument `mustExist` for controlling whether
   to give an error when the corresponding object for an identified
   global cannot be found or to silently drop the missing global.

 * `findGlobals()` and `getGlobals()` gained argument `method` for
   controlling whether a `"conservative"` or a `"liberal"` algorithm
   for identifying true globals should be used.
  
  
# Version 0.2.0 [2015-05-19]

 * Moved "globals" functions from an in-house package to this package.
  
  
# Version 0.1.0 [2015-02-07]

 * Created.
