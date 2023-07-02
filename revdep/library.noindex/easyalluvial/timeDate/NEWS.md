# timeDate 4022.108

- added the 2023 UK Bank holiday for the coronation of King Charles III.

- `axis.timeDate` was not handling properly the case when `x` was missing,
  leading to errors from R-devel check (2023-01-07 r83578). Fix suggested by Uwe
  Liege.

- refactored file NAMESPACE to facilitate maintenance (that revealed the
  two omissions listed below).

- `CAFamilyDay` (Canada Family Day) was missing from the list returned by
  `listHolidays()`. It was missing only from that list, `holidayTSX()` was
  including it when applicable.

- `JPVernalEquinox` was missing from the list returned by `listHolidays()`.

- the financial centers are now updated to reflect changes in time zones in
  recent years. The list returned by `listFinCenter()` is synchronised with
  current time zone names. Previous names supported by timeDate are available as
  aliases.
  
- import selectively from 'stats' and 'utils'.


# timeDate 4021.107

- London financial centre holidays - fixed and/or included non-standard holidays
  (e.g., Early May Bank holiday was moved in 2020 to VE day; Spring Bank holiday
  was moved in Queen's Jubilee years; state funeral of the Queen).  Millenium
  day now is included in the result of `holidayLONDON(1999)`.  The London
  holidays should now be complete up to the time of writing this (1 Oct 2022).

- renamed `GBMayDay` to `GBEarlyMayBankHoliday` and `GBBankHoliday` to
  `GBSpringBankHoliday`. The old names are somewhat ambiguous and strongly
  discouraged but still available. `listHolidays()` gives the new names.

- the generic `timeDate()` gets argument '...' to allow methods for it to have
  additional arguments (e.g., for DST gaps).

- the 'character' method for `timeDate()` gets a new argument `dst_gap` to
  control what to do with non-existent DST times at the requested `FinCenter`
  with options to add/subtract ("+", "-") the DST shift or set them to `NA`.

- `timeDate()` was not handling correctly some times just after the switch
  to/from DST. This was visible mostly for time zones away from GMT and GMT+1.

- In `timeSequence()`, if any of the generated times would fall in DST gaps,
  they are moved by "+1 hour", corresponding to `dst_gap = "+"` in `timeDate`.
  This is consistent with `seq` for other time objects.  Currently there is no
  option to change this behaviour of `timeSequence`.

  Previously `timeSequence` was moving DST gaps down by 1 hour (for by =
  'DSTday' and similar). This was not consistent similar time functions in R and
  was actually due to a bug (or unfinished DST handling) in `timeDate`, see
  remarks for `timeDate` above.
  
- `timeSequence()` now throws error if argument `from` is in a DST gap. It seems
  desirable to have a default action for this case. Rolling the faulty time by
  an hour in the case of 'DSTday' may be suitable in most cases but for other
  values of `by` it might be totally wrong. 

- updated the DST rules.

- internally, refactored the way the DST rules are generated (not visible to
  users).

- `rulesFinCenter()` now looks for a financial center starting from the
  namespace of `timeDate`. Previously it was starting from the environment of
  the caller which could result in using an unrelated object or, if `timeDate`
  was loaded but not attached, not finding it.

  
# timeDate 4021.106

- fix `whichFormat()` to accommodate a change in R-devel after which
  `as.character(Sys.time())` contains fractional seconds. (`format(Sys.time())`
  doesn't; before this change in R-devel both were dropping the fractional
  seconds). (fixed by Martin Maechler, see timeDate rev 6286)


# timeDate 4021.105

- the list returned by `holidaysNYSE()` was missing the special closing days of
  the New York stock exchange (NYSE). Now it should be complete (though there
  may be ommissions after 2011). This fixes issue #1356 reported by Corwin
  Joy. Thanks to him and Ian E for the insigthful discussion and useful links.

  See also below. Contributions for the other exchanges and corrections are
  welcome.

- `holidaysNYSE()` gets a new argument, `type`, to select what type of the
  exchange's closing days to return. The default is to return all days in the
  requested years when NYSE was closed for whatever reason. Use `type = "standard"`
  and `type = special` to get the standard holidays and the special closings,
  respectively.

  Returning any closing day by default might be considered a breaking
  change. However, not returning all closing days was perceived as erroneous by
  users (eg issue #1356). In fact, the package itself calculates business days
  by dropping weekends and days returned by `holidayXXXX`.

  Note that `holiday()` returns the actual dates of the public holidays, while
  the corresponding days returned by `holidayXXXX` are the resulting non-weekend
  closing days, if any.

- `holidayTSX()` now correctly calculates Christmas and Boxing day closures when
   Christmas is on Monday.  Fixes part (2) of issue #1288 reported by Stefan
   Wilhelm (part (1) was fixed in a previous release). The fix is really a patch
   for the specific issue, maybe the same should be done when Christmas is on
   Sunday, for example. Information/contribution on Canadian holidays is
   welcome.

- now `holiday()` accepts also a function or a list of functions for argument
  'Holiday'.

- `timeNthNdayInMonth` could return a value in the following month. Now
  fixed. This is bug #1463 reported with a fix by Manny C. Note that the bug was
  not present for dates in the first day of a month.

- `timeLastNdayInMonth` could return a value in the following month,
  e.g. '1996-06-04' for the last Tuesday in May 1996. Now fixed. The check of
  this function was prompted by the bug report for #1463 (see above) for
  `timeNthNdayInMonth` but the error was different.

- the `data.frame` methods for `kurtosis()` and `skewness()` now set attribute
  `method` as for the other methods and as documented.

- removed `.holidayList()` which had been replaced by `listHolidays()` a long
  time ago and was not exported in recent versions of `timeDate`.

- updated documentation files.

## Deprecation notes

- the `timeDate` method for `cut` has been discouraged in the sources for a long
  time with a recommendation to use `window` instead (just replace `cut(x,
  from = xx , to = yy)` with `window(x, start = xx, end = yy)`. The `cut` method
  will be deprecated in the next release and later removed or replaced by a
  method that is consistent with the methods for `cut` in base R.
  

# timeDate 4021.104

- new maintainer: Georgi N. Boshnakov.

- updated DESCRIPTION with links and moved all `Depends:` to `Imports:`.

- removed the line `LazyData: yes` from DESCRIPTION to fix the NOTE on CRAN.

- added the new US holiday, Juneteenth National Independence Day. Fixes #6755 by
  Ian E (ene100).

- `holidayTSX()` now includes the Labour Day. Fixes part (1) of issue #1288
  reported by Stefan Wilhelm.

- created a first version of `_pkgdown.yml` for more organised view of the large
  number of objects in the package. Unpack the tarball and run
  `pkgdown::build_site()` to build the site locally. Don't know if this could
  work directly off the R-forge repository.
  

# timeDate 3043.102 and older versions

  See file `ChangeLog` for changes before 4021.104.
