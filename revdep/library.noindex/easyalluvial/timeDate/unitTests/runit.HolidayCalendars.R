
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# Rmetrics is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:           HOLIDAY CALENDAR FUNCTIONS:
#  holiday             Returns a holiday date of G7 and CH
#  holidayNYSE         Returns 'timeDate' object for full-day NYSE holidays
#  holidayZURICH       Returns 'timeDate' object for ZURICH holidays
################################################################################


test.holiday =
function()
{
    # easter -
    # easter(year = currentYear, shift = 0)
    # Dates for Easter and Good Friday from 2000 until 2010:
    timeDate:::.easter()
    Easter = timeDate:::.easter(2000:2010)
    Easter
    checkTrue(inherits(Easter, "timeDate"))

    GoodFriday = timeDate:::.easter(2000:2010, -2)
    GoodFriday
    checkIdentical(
        target = Easter,
        current = GoodFriday + 2*24*3600)

    HD = holiday(2000:2010, "Easter")
    HD
    checkTrue(inherits(HD, "timeDate"))

    HD = holiday(2000:2010, "GoodFriday")
    HD
    checkTrue(inherits(HD, "timeDate"))

    # holidays -
    Easter(2000:2010)
    GoodFriday(2000:2010)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.holidayNYSE <- function() {

    test <- function() {
        # Holiday NYSE -
        HD <- holidayNYSE(getRmetricsOptions("currentYear"))
        print(HD)
        checkTrue(inherits(HD, "timeDate"))

        # After July 3, 1959, move Saturday holidays to Friday
        # ... except if at the end of monthly/yearly accounting period
        # this is the last business day of a month.
        publishedHolidays <- c(# holidays listed in http://www.nyse.com/events
                               "2007-01-01", # New Year's Day0
                               "2007-01-15", # Martin Luther King, Jr. Days
                               "2007-02-19", # Washington's
                                             # Birthday/Presidents' Days
                               "2007-04-06", # Good Friday
                               "2007-05-28", # Memorial Day
                               "2007-07-04", # Independence Days
                               "2007-09-03", # Labor Days
                               "2007-11-22", # Thanksgiving Days
                               "2007-12-25", # Christmas

                               # holidays published on http://nyse.com/holidays
                               "2008-01-01", # New Year's Days
                               "2008-01-21", # Martin Luther King, Jr. Day
                               "2008-02-18", # Washington's
                                             # Birthday/Presidents' Days
                               "2008-03-21", # Good Friday
                               "2008-05-26", # Memorial Day
                               "2008-07-04", # Independence Days
                               "2008-09-01", # Labor Day
                               "2008-11-27", # Thanksgiving Days
                               "2008-12-25", # Christmas

                               "2009-01-01", # New Year's Day
                               "2009-01-19", # Martin Luther King, Jr. Days
                               "2009-02-16", # Washington's
                                             # Birthday/Presidents' Day
                               "2009-04-10", # Good Friday
                               "2009-05-25", # Memorial Day
                               "2009-07-03", # Independence Day (observed)
                               "2009-09-07", # Labor Day
                               "2009-11-26", # Thanksgiving Day
                               "2009-12-25", # Christmas+

                               "2010-01-01", # New Year's Day
                               "2010-01-18", # Martin Luther King, Jr. Days
                               "2010-02-15", # Washington's
                                             # Birthday/Presidents' Day
                               "2010-04-02", # Good Friday
                               "2010-05-31", # Memorial Day
                               "2010-07-05", # Independence Day (observed)
                               "2010-09-06", # Labor Day
                               "2010-11-25", # Thanksgiving Day
                               "2010-12-24", # Christmas+

                                             # New Year's Day in 2011 falls
                               "2011-01-17", # Martin Luther King, Jr. Days
                               "2011-02-21", # Washington's
                                             # Birthday/Presidents' Day
                               "2011-04-22", # Good Friday
                               "2011-05-30", # Memorial Day
                               "2011-07-04", # Independence Day (observed)
                               "2011-09-05", # Labor Day
                               "2011-11-24", # Thanksgiving Day
                               "2011-12-26") # Christmas+

        publishedHolidays <- timeDate(publishedHolidays, zone="NewYork",
                                      FinCenter="NewYork")
        ## GNB: for v. 4021.105 use argument type for old behaviour
        ##  [2007-01-02] - mourning for G.R. Ford, not included in holidayNYSE()
        checkTrue(all.equal(publishedHolidays, holidayNYSE(2007:2011, type = "standard")))
    }

    # check if there is any problem with timezone and days
    old <- setRmetricsOptions(myFinCenter = "Zurich")
    on.exit(setRmetricsOptions(old))
    test()

    # change tzone
    setRmetricsOptions(myFinCenter = "GMT")
    test()
}


# ------------------------------------------------------------------------------

test.holidayZURICH <- function() {

    # Holiday Zurich -
    holidayZURICH(getRmetricsOptions("currentYear"))

    # Return Value:
    return()
}

# ------------------------------------------------------------------------------

test.holidayNERC <- function() {

    test <- function() {
        # Holiday NERC -
        HD <- holidayNERC(getRmetricsOptions("currentYear"))
        print(HD)
        checkTrue(inherits(HD, "timeDate"))

        #  http://www.nerc.com/docs/oc/rs/OffPeakDays.csv

        publishedHolidays <- c("1/1/2009",   # New Year's Day
                               "5/25/2009",  # Memorial Day
                               "7/4/2009",   # Independence
                               "9/7/2009",   # Labor Days
                               "11/26/2009", # Thanksgiving Day
                               "12/25/2009", # Christmas Day
                               "1/1/2010",   # New Years
                               "5/31/2010",  # Memorial Day
                               "7/5/2010",   # Independence Day
                               "9/6/2010",   # Labor Day
                               "11/25/2010", # Thanksgiving
                               "12/25/2010", # Christmas
                               "1/1/2011",   # New Years
                               "5/30/2011",  # Memorial Day
                               "7/4/2011",   # Independence Day
                               "9/5/2011",   # Labor Day
                               "11/24/2011", # Thanksgiving
                               "12/26/2011", # Christmas
                               "1/2/2012",   # New Year's Day
                               "5/28/2012",  # Memorial Day
                               "7/4/2012",   # Independence
                               "9/3/2012",   # Labor Day
                               "11/22/2012", # Thanksgiving Day
                               "12/25/2012", # Christmas Day
                               "1/1/2013",   # New Year's Day
                               "5/27/2013",  # Memorial Day
                               "7/4/2013",   # Independence
                               "9/2/2013",   # Labor Day
                               "11/28/2013", # Thanksgiving Day
                               "12/25/2013", # Christmas Day
                               "1/1/2014",   # New Year's Day
                               "5/26/2014",  # Memorial Day
                               "7/4/2014",   # Independence
                               "9/1/2014",   # Labor Day
                               "11/27/2014", # Thanksgiving Day
                               "12/25/2014", # Christmas Day
                               "1/1/2015",   # New Year's Day
                               "5/25/2015",  # Memorial Day
                               "7/4/2015",   # Independence
                               "9/7/2015",   # Labor Day
                               "11/26/2015", # Thanksgiving Day
                               "12/25/2015") # Christmas Day

        publishedHolidays <- timeDate(publishedHolidays, zone="Eastern",
                                      FinCenter="Eastern")
        checkTrue(all.equal(publishedHolidays, holidayNERC(2009:2015)))
    }

    # check if there is any problem with timezone and days
    old <- setRmetricsOptions(myFinCenter = "Zurich")
    on.exit(setRmetricsOptions(old))
    test()

    # change tzone
    setRmetricsOptions(myFinCenter = "GMT")
    test()
}

## GNB: test fixes on 2022-10-01
test.holidayLONDON <- function() {

    test <- function() {
        # Holiday LONDON -
        HD <- holidayLONDON(getRmetricsOptions("currentYear"))
        print(HD)
        checkTrue(inherits(HD, "timeDate"))

        ## https://www.gov.uk/bank-holidays#england-and-wales
        ## (downloaded on 2022-10-01, this is updated regularly
        
        ph2017 <- c("2017-01-02", "2017-04-14", "2017-04-17", "2017-05-01",
                    "2017-05-29", "2017-08-28", "2017-12-25", "2017-12-26" )
        
        ph2018 <- c("2018-01-01", "2018-03-30", "2018-04-02", "2018-05-07",
                    "2018-05-28", "2018-08-27", "2018-12-25", "2018-12-26" )

        ph2019 <- c("2019-01-01", "2019-04-19", "2019-04-22", "2019-05-06",
                    "2019-05-27", "2019-08-26", "2019-12-25", "2019-12-26" )

        ph2020 <- c("2020-01-01", "2020-04-10", "2020-04-13", "2020-05-08",
                    "2020-05-25", "2020-08-31", "2020-12-25", "2020-12-28" )

        ph2021 <- c("2021-01-01", "2021-04-02", "2021-04-05", "2021-05-03",
                    "2021-05-31", "2021-08-30", "2021-12-27", "2021-12-28" )

        ## Queen's Platinum Jubilee; Quinn's death
        ph2022 <- c("2022-01-03", "2022-04-15", "2022-04-18", "2022-05-02",
                    "2022-06-02", "2022-06-03", "2022-08-29", "2022-09-19",
                    "2022-12-26", "2022-12-27" ) 

        ## Queen's Jubilee
        ph2002 <- c("2002-01-01", "2002-03-29", "2002-04-01", "2002-05-06",
                    "2002-06-03", "2002-06-04", "2002-08-26", "2002-12-25", "2002-12-26")

        ## Queen's Diamond Jubilee
        ph2012 <- c("2012-01-02", "2012-04-06", "2012-04-09", "2012-05-07",
                    "2012-06-04", "2012-06-05", "2012-08-27", "2012-12-25", "2012-12-26")

        checkTrue(all.equal(ph2017, format(holidayLONDON(2017))))
        checkTrue(all.equal(ph2018, format(holidayLONDON(2018))))
        checkTrue(all.equal(ph2019, format(holidayLONDON(2019))))
        checkTrue(all.equal(ph2020, format(holidayLONDON(2020))))
        checkTrue(all.equal(ph2021, format(holidayLONDON(2021))))
        checkTrue(all.equal(ph2022, format(holidayLONDON(2022))))

        checkTrue(all.equal(ph2002, format(holidayLONDON(2002))))
        checkTrue(all.equal(ph2012, format(holidayLONDON(2012))))

        
        checkTrue(format(GBMayDay(1995)) == "1995-05-08") # VE day that year
        checkTrue(format(GBMayDay(2020)) == "2020-05-08") # VE day that year

        checkTrue(format(GBBankHoliday(2002)) == "2002-06-03")
        checkTrue(format(GBBankHoliday(2012)) == "2012-06-04")
        checkTrue(format(GBBankHoliday(2022)) == "2022-06-02")

        ## millenium day was not included before v4021.107
        checkTrue(length(holidayLONDON(1999)) == 9)
    }

    test()
}





################################################################################

