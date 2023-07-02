## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(clock)
library(magrittr)

## -----------------------------------------------------------------------------
date_build(2019, 2, 1:5)

## ---- error=TRUE--------------------------------------------------------------
date_build(2019, 1:12, 31)

## -----------------------------------------------------------------------------
date_build(2019, 1:12, 31, invalid = "previous")

## -----------------------------------------------------------------------------
date_build(2019, 1:12, "last")

## -----------------------------------------------------------------------------
date_time_build(2019, 1:5, 1, 2, 30, zone = "America/New_York")

## ---- error=TRUE--------------------------------------------------------------
date_time_build(2019:2021, 3, 8, 2, 30, zone = "America/New_York")

## -----------------------------------------------------------------------------
zone <- "America/New_York"

date_time_build(2019:2021, 3, 8, 2, 30, zone = zone, nonexistent = "roll-forward")
date_time_build(2019:2021, 3, 8, 2, 30, zone = zone, nonexistent = "roll-backward")

## -----------------------------------------------------------------------------
date_parse("2019-01-05")

## -----------------------------------------------------------------------------
date_parse("January 5, 2020", format = "%B %d, %Y")

## -----------------------------------------------------------------------------
date_parse(
  "juillet 10, 2021", 
  format = "%B %d, %Y", 
  locale = clock_locale("fr")
)

## -----------------------------------------------------------------------------
x <- c("2020/1/5", "10-03-05", "2020/2/2")
formats <- c("%Y/%m/%d", "%y-%m-%d")

date_parse(x, format = formats)

## -----------------------------------------------------------------------------
date_time_parse("2020-01-01 01:02:03", "America/New_York")

## -----------------------------------------------------------------------------
before <- date_time_parse("2020-11-01 00:59:59", "America/New_York")

# First 1 o'clock
before + 1

# Second 1 o'clock
before + 1 + 3600

## ---- error=TRUE--------------------------------------------------------------
date_time_parse("2020-11-01 01:30:00", "America/New_York")

## -----------------------------------------------------------------------------
zone <- "America/New_York"

date_time_parse("2020-11-01 01:30:00", zone, ambiguous = "earliest")
date_time_parse("2020-11-01 01:30:00", zone, ambiguous = "latest")

## -----------------------------------------------------------------------------
x <- "2020-01-01T01:02:03-05:00[America/New_York]"

date_time_parse_complete(x)

## -----------------------------------------------------------------------------
x <- "2020-01-01 01:02:03 EST"

date_time_parse_abbrev(x, "America/New_York")

## -----------------------------------------------------------------------------
x <- c(
  "1970-10-25 01:30:00 EDT",
  "1970-10-25 01:30:00 EST"
)

date_time_parse_abbrev(x, "America/New_York")

## -----------------------------------------------------------------------------
x <- "1970-01-01 02:30:30 IST"

# IST = India Standard Time
date_time_parse_abbrev(x, "Asia/Kolkata")

# IST = Israel Standard Time
date_time_parse_abbrev(x, "Asia/Jerusalem")

## -----------------------------------------------------------------------------
x <- "2020-01-01T01:02:03Z"

date_time_parse_RFC_3339(x)

## -----------------------------------------------------------------------------
x <- "2020-01-01T01:02:03-0500"

date_time_parse_RFC_3339(x, offset = "%z")

x <- "2020-01-01T01:02:03-05:00"

date_time_parse_RFC_3339(x, offset = "%Ez")

## -----------------------------------------------------------------------------
x <- seq(date_build(2019, 1, 20), date_build(2019, 2, 5), by = 1)
x

# Grouping by 5 days of the current month
date_group(x, "day", n = 5)

## -----------------------------------------------------------------------------
date_group(x, "month")

## -----------------------------------------------------------------------------
x <- seq(
  date_time_build(2019, 1, 1, 1, 55, zone = "UTC"),
  date_time_build(2019, 1, 1, 2, 15, zone = "UTC"),
  by = 120
)
x

date_group(x, "minute", n = 5)

## -----------------------------------------------------------------------------
unclass(date_build(2020, 1, 1))

## -----------------------------------------------------------------------------
x <- seq(date_build(1970, 01, 01), date_build(1970, 05, 10), by = 20)

date_floor(x, "day", n = 60)
date_ceiling(x, "day", n = 60)

## -----------------------------------------------------------------------------
as_weekday(date_floor(x, "week", n = 14))

## -----------------------------------------------------------------------------
sunday <- date_build(1970, 01, 04)

date_floor(x, "week", n = 14, origin = sunday)

as_weekday(date_floor(x, "week", n = 14, origin = sunday))

## -----------------------------------------------------------------------------
x <- date_build(2020, 1, 1:2)

# Wednesday / Thursday
as_weekday(x)

# `clock_weekdays` is a helper that returns the code corresponding to
# the requested day of the week
clock_weekdays$tuesday

tuesday <- weekday(clock_weekdays$tuesday)
tuesday

date_shift(x, target = tuesday)

## -----------------------------------------------------------------------------
x <- seq(date_build(1970, 01, 01), date_build(1970, 01, "last"), by = 3)

date_shift(x, tuesday, which = "previous")

## -----------------------------------------------------------------------------
x <- date_build(2020, 1, 1)

add_years(x, 1:5)

## ---- error=TRUE--------------------------------------------------------------
x <- date_build(2020, 1, 31)

add_months(x, 1)

## -----------------------------------------------------------------------------
# The previous valid moment in time
add_months(x, 1, invalid = "previous")

# The next valid moment in time
add_months(x, 1, invalid = "next")

# Overflow the days. There were 29 days in February, 2020, but we
# specified 31. So this overflows 2 days past day 29.
add_months(x, 1, invalid = "overflow")

# If you don't consider it to be a valid date
add_months(x, 1, invalid = "NA")

## -----------------------------------------------------------------------------
ymd <- as_year_month_day(x) + duration_months(1)
ymd

## -----------------------------------------------------------------------------
# Adding 1 more month makes it valid again
ymd + duration_months(1)

## -----------------------------------------------------------------------------
x <- date_time_build(2020, 1, 1, 2, 30, zone = "America/New_York")

x %>%
  add_days(1) %>%
  add_hours(2:5)

## ---- error=TRUE--------------------------------------------------------------
x <- date_time_build(1970, 04, 25, 02, 30, 00, zone = "America/New_York")
x

# Daylight saving time gap on the 26th between 01:59:59 -> 03:00:00
x %>% add_days(1)

## -----------------------------------------------------------------------------
# Roll forward to the next valid moment in time
x %>% add_days(1, nonexistent = "roll-forward")

# Roll backward to the previous valid moment in time
x %>% add_days(1, nonexistent = "roll-backward")

# Shift forward by adding the size of the DST gap
# (this often keeps the time of day,
# but doesn't guaratee that relative ordering in `x` is maintained
# so I don't recommend it)
x %>% add_days(1, nonexistent = "shift-forward")

# Replace nonexistent times with an NA
x %>% add_days(1, nonexistent = "NA")

## -----------------------------------------------------------------------------
x <- date_build(2019, 5, 6)

get_year(x)
get_month(x)
get_day(x)

x %>%
  set_day(22) %>%
  set_month(10)

## ---- error=TRUE--------------------------------------------------------------
x %>%
  set_day(31) %>%
  set_month(4)

x %>%
  set_day(31) %>%
  set_month(4, invalid = "previous")

## -----------------------------------------------------------------------------
x <- date_time_build(2020, 1, 2, 3, zone = "America/New_York")
x

x %>%
  set_minute(5) %>%
  set_second(10)

