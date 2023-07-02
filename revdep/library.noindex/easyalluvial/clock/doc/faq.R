## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(clock)
library(magrittr)

## -----------------------------------------------------------------------------
x <- year_month_day(2019, 1, 5)

add_months(x, 1)

## ---- error=TRUE--------------------------------------------------------------
add_days(x, 1)

## -----------------------------------------------------------------------------
x %>%
  as_naive_time() %>%
  add_days(1) %>%
  as_year_month_day()

## -----------------------------------------------------------------------------
odd_dates <- year_month_day(2019, 2, 28:31)
odd_dates

## -----------------------------------------------------------------------------
odd_dates %>%
  invalid_resolve(invalid = "next")

odd_dates %>%
  invalid_resolve(invalid = "next") %>%
  as_naive_time() %>%
  add_days(2)

odd_dates %>%
  invalid_resolve(invalid = "overflow")

odd_dates %>%
  invalid_resolve(invalid = "overflow") %>%
  as_naive_time() %>%
  add_days(2)

## -----------------------------------------------------------------------------
x <- zoned_time_parse_complete("1970-04-26T01:30:00-05:00[America/New_York]")
x

## ---- error=TRUE--------------------------------------------------------------
add_days(x, 1)

add_seconds(x, 1)

## -----------------------------------------------------------------------------
x

# The printed time with no time zone info
as_naive_time(x)

# The equivalent time in UTC
as_sys_time(x)

zoned_time_zone(x)

## -----------------------------------------------------------------------------
x %>%
  as_naive_time() %>%
  add_seconds(1) %>%
  as_zoned_time(zoned_time_zone(x))

x %>%
  as_sys_time() %>%
  add_seconds(1) %>%
  as_zoned_time(zoned_time_zone(x))

## ---- error=TRUE--------------------------------------------------------------
# There is a DST gap 1 second after 01:59:59,
# which jumps us straight to 03:00:00,
# skipping the 2 o'clock hour entirely

x %>%
  as_naive_time() %>%
  add_minutes(30) %>%
  as_zoned_time(zoned_time_zone(x))

x %>%
  as_sys_time() %>%
  add_minutes(30) %>%
  as_zoned_time(zoned_time_zone(x))

## -----------------------------------------------------------------------------
x

x %>%
  as_naive_time()

## -----------------------------------------------------------------------------
x %>%
  as_naive_time() %>%
  add_minutes(30)

## -----------------------------------------------------------------------------
x %>%
  as_naive_time() %>%
  add_minutes(30) %>%
  as_zoned_time(zoned_time_zone(x), nonexistent = "roll-forward")

## -----------------------------------------------------------------------------
old <- options(digits.secs = 6, digits = 22)

## -----------------------------------------------------------------------------
x <- as.POSIXct("2019-01-01 01:00:00.2", "America/New_York")
x

## -----------------------------------------------------------------------------
as_naive_time(x)

## -----------------------------------------------------------------------------
y <- as.POSIXct(
  c("2019-01-01 01:00:00.1", "2019-01-01 01:00:00.3"), 
  "America/New_York"
)

# Oh dear!
y

## -----------------------------------------------------------------------------
unclass(y)

## -----------------------------------------------------------------------------
new_utc <- function(x) {
  class(x) <- c("POSIXct", "POSIXt")
  attr(x, "tzone") <- "UTC"
  x
}

year_2050 <- 2524608000
five_microseconds <- 0.000005

new_utc(year_2050)

# Oh no!
new_utc(year_2050 + five_microseconds)

# Represented internally as:
year_2050 + five_microseconds

## -----------------------------------------------------------------------------
naive_time_parse(
  c("2019-01-01T01:00:00.1", "2019-01-01T01:00:00.3"), 
  precision = "millisecond"
) %>%
  as_zoned_time("America/New_York")

## -----------------------------------------------------------------------------
# Reset old options
options(old)

## -----------------------------------------------------------------------------
x <- as.Date("2019-01-01")
x

withr::with_timezone("America/New_York", {
  print(as.POSIXct(x))
})

## -----------------------------------------------------------------------------
as_zoned_time(x, "UTC")

as_zoned_time(x, "America/New_York")

as_zoned_time(x, "Europe/London")

## -----------------------------------------------------------------------------
x <- as.POSIXct("2019-01-01 23:00:00", "America/New_York")

as.Date(x, tz = date_time_zone(x))

## -----------------------------------------------------------------------------
utc <- date_time_set_zone(x, "UTC")
utc

as.Date(utc, tz = date_time_zone(utc))

## ---- warning=TRUE------------------------------------------------------------
raw <- c(
  "2015-12-31T23:59:59", 
  "2015-12-31T23:59:60", # A real leap second!
  "2016-01-01T00:00:00"
)

x <- sys_time_parse(raw)

x

## -----------------------------------------------------------------------------
# Reported as exactly 1 second apart.
# In real life these are 2 seconds apart because of the leap second.
x[[3]] - x[[1]]

## -----------------------------------------------------------------------------
# This returns a POSIXlt, which can handle the special 60s field
x <- strptime(raw, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
x

# On conversion to POSIXct, it "rolls" forward
as.POSIXct(x)

## -----------------------------------------------------------------------------
# 2016-12-31 wasn't a leap second date, but it still tries to parse this fake time
strptime("2016-12-31T23:59:60", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

## ---- eval=FALSE--------------------------------------------------------------
#  library(data.table)
#  
#  data.table(x = year_month_day(2019, 1, 1))
#  #> Error in dimnames(x) <- dn :
#  #>   length of 'dimnames' [1] not equal to array extent

## -----------------------------------------------------------------------------
ymdh <- year_month_day(2019, 1, 1:2, 1)

unclass(ymdh)

unclass(as_naive_time(ymdh))

## -----------------------------------------------------------------------------
x <- as.POSIXct("2019-01-01", "America/New_York")

# POSIXct is implemented as a double
unclass(x)

# POSIXlt is a record type
unclass(as.POSIXlt(x))

## ---- eval=FALSE--------------------------------------------------------------
#  data.table(x = as.POSIXlt("2019-01-01", "America/New_York"))
#  #>             x
#  #> 1: 2019-01-01
#  #> Warning message:
#  #> In as.data.table.list(x, keep.rownames = keep.rownames, check.names = check.names,  :
#  #>   POSIXlt column type detected and converted to POSIXct. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.

