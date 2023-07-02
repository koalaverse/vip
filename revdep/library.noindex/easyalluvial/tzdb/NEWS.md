# tzdb 0.4.0

* Updated the embedded date library
  (SHA cc4685a21e4a4fdae707ad1233c61bbaff241f93) (#30).

* Updated the time zone database to 2023c (#29).

* R >=3.5.0 is now required. This is consistent with the standards of the
  tidyverse.

# tzdb 0.3.0

* Updated the embedded date library
  (SHA 9ea5654c1206e19245dc21d8a2c433e090c8c3f5) (#22).

* Updated the time zone database to 2022a (#21).

* R >=3.4.0 is now required. This is consistent with the standards of the
  tidyverse.

* cpp11 >=0.4.2 is now required to ensure that a fix related to unwind
  protection is included.

# tzdb 0.2.0

* Updated the time zone database to 2021e (#12).

* Updated the embedded date library (SHA d9049ee6976f45eff434c4971baa78ff807562c4).

* Fixed a Windows issue where the time zone database couldn't be found if the
  path to it contained Unicode characters (#10).

# tzdb 0.1.2

* Updated the embedded date library.

# tzdb 0.1.1

* tzdb now provides C++ headers and callables for working with the 'date'
  library. 'date' provides comprehensive support for working with dates and
  date-times, which this package exposes to make it easier for other R packages
  to utilize.

# tzdb 0.1.0

* Added a `NEWS.md` file to track changes to the package.
