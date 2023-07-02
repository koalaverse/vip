## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(clock)
library(magrittr)

## ---- eval=FALSE--------------------------------------------------------------
#  zoned_time_now("")
#  #> <zoned_time<nanosecond><America/New_York (current)>[1]>
#  #> [1] "2021-02-10T15:54:29.875011000-05:00"

## ---- eval=FALSE--------------------------------------------------------------
#  zoned_time_now("Asia/Shanghai")
#  #> <zoned_time<nanosecond><Asia/Shanghai>[1]>
#  #> [1] "2021-02-11T04:54:29.875011000+08:00"

## -----------------------------------------------------------------------------
my_time <- year_month_day(2019, 1, 30, 9) %>%
  as_naive_time() %>%
  as_zoned_time("America/New_York")

my_time

their_time <- zoned_time_set_zone(my_time, "Asia/Shanghai")

their_time

## -----------------------------------------------------------------------------
my_time <- as.POSIXct("2019-01-30 09:00:00", "America/New_York")

date_time_set_zone(my_time, "Asia/Shanghai")

## -----------------------------------------------------------------------------
my_time <- year_month_day(2019, 1, 30, 9) %>%
  as_naive_time() %>%
  as_zoned_time("America/New_York")

my_time

# Drop the time zone information, retaining the printed time
my_time %>%
  as_naive_time()

# Add the correct time zone name back on,
# again retaining the printed time
their_9am <- my_time %>%
  as_naive_time() %>%
  as_zoned_time("Asia/Shanghai")

their_9am

## -----------------------------------------------------------------------------
zoned_time_set_zone(their_9am, "America/New_York")

## -----------------------------------------------------------------------------
my_time <- as.POSIXct("2019-01-30 09:00:00", "America/New_York")

my_time %>%
  as_naive_time() %>%
  as.POSIXct("Asia/Shanghai") %>%
  date_time_set_zone("America/New_York")

## -----------------------------------------------------------------------------
days <- as_naive_time(year_month_day(2019, c(1, 2), 1))

# A Tuesday and a Friday
as_weekday(days)

monday <- weekday(clock_weekdays$monday)

time_point_shift(days, monday)

as_weekday(time_point_shift(days, monday))

## -----------------------------------------------------------------------------
time_point_shift(days, monday, which = "previous")

## -----------------------------------------------------------------------------
tuesday <- weekday(clock_weekdays$tuesday)

time_point_shift(days, tuesday)
time_point_shift(days, tuesday, boundary = "advance")

## -----------------------------------------------------------------------------
next_weekday <- function(x, target) {
  x + (target - as_weekday(x))
}

next_weekday(days, monday)

as_weekday(next_weekday(days, monday))

## -----------------------------------------------------------------------------
monday - as_weekday(days)

## -----------------------------------------------------------------------------
days + (monday - as_weekday(days))

## -----------------------------------------------------------------------------
next_weekday2 <- function(x, target) {
  x <- x + duration_days(1L)
  x + (target - as_weekday(x))
}

a_monday <- as_naive_time(year_month_day(2018, 12, 31))
as_weekday(a_monday)

next_weekday2(a_monday, monday)

## -----------------------------------------------------------------------------
monday <- weekday(clock_weekdays$monday)

x <- as.Date(c("2019-01-01", "2019-02-01"))

date_shift(x, monday)

# With a date-time
y <- as.POSIXct(
  c("2019-01-01 02:30:30", "2019-02-01 05:20:22"), 
  "America/New_York"
)

date_shift(y, monday)

## -----------------------------------------------------------------------------
ym <- seq(year_month_day(2019, 1), by = 2, length.out = 10)
ym

## -----------------------------------------------------------------------------
yq <- seq(year_quarter_day(2019, 1), by = 2, length.out = 10)

## -----------------------------------------------------------------------------
set_day(ym, "last")

set_day(yq, "last")

## -----------------------------------------------------------------------------
from <- as_naive_time(year_month_day(2019, 1, 1))
to <- as_naive_time(year_month_day(2019, 5, 15))

seq(from, to, by = 20)

## -----------------------------------------------------------------------------
from <- as_naive_time(year_month_day(2019, 1, 1, 2, 30, 00))
to <- as_naive_time(year_month_day(2019, 1, 1, 12, 30, 00))

seq(from, to, by = duration_minutes(90))

## -----------------------------------------------------------------------------
date_seq(date_build(2019, 1), by = 2, total_size = 10)

## -----------------------------------------------------------------------------
date_seq(date_build(2019, 1), by = duration_months(2), total_size = 10)

## ---- error=TRUE--------------------------------------------------------------
date_seq(
  date_build(2019, 1, 1),
  to = date_build(2019, 10, 2),
  by = duration_months(2)
)

## ---- error=TRUE--------------------------------------------------------------
jan31 <- date_build(2019, 1, 31)
dec31 <- date_build(2019, 12, 31)

date_seq(jan31, to = dec31, by = duration_months(1))

## -----------------------------------------------------------------------------
date_seq(jan31, to = dec31, by = duration_months(1), invalid = "previous")

## -----------------------------------------------------------------------------
seq(jan31, to = dec31, by = "1 month")

## -----------------------------------------------------------------------------
from <- as_naive_time(year_month_day(2019, 1, 1))
to <- as_naive_time(year_month_day(2019, 12, 31))

x <- seq(from, to, by = duration_days(20))

x

## -----------------------------------------------------------------------------
ymd <- as_year_month_day(x)

head(ymd)

calendar_group(ymd, "month")

## -----------------------------------------------------------------------------
yqd <- as_year_quarter_day(x)

head(yqd)

calendar_group(yqd, "quarter")

## -----------------------------------------------------------------------------
calendar_group(ymd, "month", n = 2)

calendar_group(yqd, "quarter", n = 2)

## -----------------------------------------------------------------------------
x <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = 20)

date_group(x, "month")

## -----------------------------------------------------------------------------
x %>%
  as_year_quarter_day() %>%
  calendar_group("quarter") %>%
  set_day(1) %>%
  as.Date()

## -----------------------------------------------------------------------------
x %>%
  as_year_quarter_day(start = clock_months$june) %>%
  calendar_group("quarter") %>%
  set_day(1) %>%
  as.Date()

## -----------------------------------------------------------------------------
from <- as_naive_time(year_month_day(2019, 1, 1))
to <- as_naive_time(year_month_day(2019, 12, 31))

x <- seq(from, to, by = duration_days(20))

## -----------------------------------------------------------------------------
time_point_floor(x, "day", n = 60)

## -----------------------------------------------------------------------------
unclass(x[1])

## -----------------------------------------------------------------------------
x <- seq(as_naive_time(year_month_day(2019, 1, 1)), by = 3, length.out = 10)
x

thursdays <- time_point_floor(x, "day", n = 14)
thursdays

as_weekday(thursdays)

## -----------------------------------------------------------------------------
origin <- as_naive_time(year_month_day(2018, 12, 31))
as_weekday(origin)

mondays <- time_point_floor(x, "day", n = 14, origin = origin)
mondays

as_weekday(mondays)

## -----------------------------------------------------------------------------
x <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = 20)

date_floor(x, "day", n = 60)

## -----------------------------------------------------------------------------
x <- seq(as.Date("2019-01-01"), by = 3, length.out = 10)

origin <- as.Date("2018-12-31")

date_floor(x, "week", n = 2, origin = origin)

## -----------------------------------------------------------------------------
x <- year_month_day(2019, clock_months$july, 4)

yd <- as_year_day(x)
yd

get_day(yd)

## -----------------------------------------------------------------------------
x <- as.Date("2019-07-04")

x %>%
  as_year_day() %>%
  get_day()

## -----------------------------------------------------------------------------
x <- year_month_day(1980, 12, 14:16)
today <- year_month_day(2005, 12, 15)

# Note that the month and day of the month are taken into account!
# (Time of day would also be taken into account if there was any.)
calendar_count_between(x, today, "year")

## -----------------------------------------------------------------------------
x <- date_build(1980, 12, 14:16)
today <- date_build(2005, 12, 15)

date_count_between(x, today, "year")

## -----------------------------------------------------------------------------
x <- year_month_day(2019, 11, 28)

# lubridate::week(as.Date(x))
# [1] 48

x_start <- calendar_start(x, "year")
x_start

time_point_count_between(
  as_naive_time(x_start),
  as_naive_time(x),
  "week"
) + 1L

## -----------------------------------------------------------------------------
doy <- get_day(as_year_day(x))
doy

(doy - 1L) %/% 7L + 1L

## -----------------------------------------------------------------------------
x <- date_build(2019, 11, 28)

date_count_between(date_start(x, "year"), x, "week") + 1L

## -----------------------------------------------------------------------------
x <- year_month_day(2013, 10, 15)
y <- year_month_day(2016, 10, 13)

## -----------------------------------------------------------------------------
calendar_narrow(y, "month") - calendar_narrow(x, "month")

## -----------------------------------------------------------------------------
calendar_count_between(x, y, "month")

## -----------------------------------------------------------------------------
x_close <- add_months(x, calendar_count_between(x, y, "month"))
x_close

x_close_st <- as_sys_time(x_close)
y_st <- as_sys_time(y)

time_point_count_between(x_close_st, y_st, "day")

## -----------------------------------------------------------------------------
# Days between x and y
days <- as_sys_time(y) - as_sys_time(x)
days

# In units of seconds
days <- duration_cast(days, "second")
days <- as.numeric(days)
days

# Average number of seconds in 1 proleptic Gregorian month
avg_sec_in_month <- duration_cast(duration_months(1), "second")
avg_sec_in_month <- as.numeric(avg_sec_in_month)

days / avg_sec_in_month

## -----------------------------------------------------------------------------
x <- date_build(2013, 10, 15)
y <- date_build(2016, 10, 13)

## -----------------------------------------------------------------------------
date_count_between(date_start(x, "month"), date_start(y, "month"), "month")

## -----------------------------------------------------------------------------
date_count_between(x, y, "month")

## -----------------------------------------------------------------------------
x <- date_build(2019:2026)
y <- as_year_week_day(x, start = clock_weekdays$monday)

data.frame(x = x, y = y)

## -----------------------------------------------------------------------------
get_year(y)
get_week(y)

# Last week in the ISO year
set_week(y, "last")

## -----------------------------------------------------------------------------
calendar_narrow(y, "week")

## -----------------------------------------------------------------------------
x <- date_build(2019:2026)
iso <- as_year_week_day(x, start = clock_weekdays$monday)
epi <- as_year_week_day(x, start = clock_weekdays$sunday)

data.frame(x = x, iso = iso, epi = epi)

## -----------------------------------------------------------------------------
get_year(epi)
get_week(epi)

## -----------------------------------------------------------------------------
x <- "2020-10-25 01:30:00 IST"

zoned_time_parse_abbrev(x, "Asia/Kolkata")
zoned_time_parse_abbrev(x, "Asia/Jerusalem")

## -----------------------------------------------------------------------------
x <- naive_time_parse(x, format = "%Y-%m-%d %H:%M:%S IST")
x

## -----------------------------------------------------------------------------
naive_find_by_abbrev <- function(x, abbrev) {
  if (!is_naive_time(x)) {
    abort("`x` must be a naive-time.")
  }
  if (length(x) != 1L) {
    abort("`x` must be length 1.")
  }
  if (!rlang::is_string(abbrev)) {
    abort("`abbrev` must be a single string.")
  }
  
  zones <- tzdb_names()
  info <- naive_time_info(x, zones)
  info$zones <- zones
  
  c(
    compute_uniques(x, info, abbrev),
    compute_ambiguous(x, info, abbrev)
  )
}

compute_uniques <- function(x, info, abbrev) {
  info <- info[info$type == "unique",]
  
  # If the abbreviation of the unique time matches the input `abbrev`,
  # then that candidate zone should be in the output
  matches <- info$first$abbreviation == abbrev
  zones <- info$zones[matches]
  
  lapply(zones, as_zoned_time, x = x)
}

compute_ambiguous <- function(x, info, abbrev) {
  info <- info[info$type == "ambiguous",]

  # Of the two possible times,
  # does the abbreviation of the earliest match the input `abbrev`?
  matches <- info$first$abbreviation == abbrev
  zones <- info$zones[matches]
  
  earliest <- lapply(zones, as_zoned_time, x = x, ambiguous = "earliest")
  
  # Of the two possible times,
  # does the abbreviation of the latest match the input `abbrev`?
  matches <- info$second$abbreviation == abbrev
  zones <- info$zones[matches]
  
  latest <- lapply(zones, as_zoned_time, x = x, ambiguous = "latest")
  
  c(earliest, latest)
}

## -----------------------------------------------------------------------------
candidates <- naive_find_by_abbrev(x, "IST")
candidates

## -----------------------------------------------------------------------------
as_zoned_time(x, "Asia/Kolkata")
as_zoned_time(x, "Europe/Dublin", ambiguous = "earliest")
as_zoned_time(x, "Asia/Jerusalem", ambiguous = "latest")

## -----------------------------------------------------------------------------
x <- zoned_time_parse_complete("2019-01-01T00:00:00-05:00[America/New_York]")

info <- zoned_time_info(x)

# Beginning of the current DST range
info$begin

# Beginning of the next DST range
info$end

## -----------------------------------------------------------------------------
# Last moment in time in the current DST range
info$end %>%
  as_sys_time() %>%
  add_seconds(-1) %>%
  as_zoned_time(zoned_time_zone(x))

## -----------------------------------------------------------------------------
x <- date_time_parse("2019-01-01 00:00:00", zone = "America/New_York")

date_time_info(x)

