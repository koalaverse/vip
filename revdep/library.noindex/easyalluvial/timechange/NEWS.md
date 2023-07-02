Version 0.2.0
=============

## New Features

 - [#22](https://github.com/vspinu/timechange/issues/22) Implement absolute units `aminute` and `ahour`
 - [#23](https://github.com/vspinu/timechange/issues/23) Implement new `roll_dst` parameters `xfirst` and `xlast` to allow for directional crossing of the DST gap
 - Rounding gain new `origin` parameter with respect to which to round with "absolute" units.

## Bug Fixes

 - [#16](https://github.com/vspinu/timechange/issues/16) Rounding unit parser is now conformant to R numeric parser
 - [#23](https://github.com/vspinu/timechange/pull/24) Respect `tzone` attributes of Date objects.

## Internals

 - [#17](https://github.com/vspinu/timechange/issues/17) Simplified and refactored unit parser.

Version 0.1.1
=============

## Changes

 - Follow vctrs replication rules
 - Change arguments of `time_add()` and `time_subtract()` to singulars
 - Build on top of cpp11 instead of Rcpp

Version 0.1.0
=============

## New Features:

 - Refactor `roll_month` and `roll_dst` parameterisation
   + more intuitive names
   + full control over the behavior of repeated and skipped DST intervals
 - `time_update()` gains new argument `exact = FALSE` in order to enforce very strict updating rules

Version 0.0.2
=============

## New Features:

 - New function `time_get()` for extraction of date-time components

## Bug Fixes:

 - [#8](https://github.com/vspinu/timechange/issues/8) Correctly handle infinite date-times

## Changes:

 - Rename global option "week_start" -> "timechange.week_start"

Version 0.0.1
=============

Initial Release
