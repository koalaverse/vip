
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
# FUNCTION:             DESCRIPTION:
#  ...                   Holiday Functions
# FUNCTION:             DESCRIPTION:
#  listHolidays          Prints all public and ecclestical holidays
#  Easter                Returns date of easter or related feasts
################################################################################


test.holiday =
function()
{
    # Holidays:
    holidays = as.vector(listHolidays())
    for (holiday in holidays) {
        Holiday = match.fun(holiday)
        cat(as.character(Holiday(getRmetricsOptions("currentYear"))), holiday, "\n")
    }

    # GB Holidays:
    listHolidays("GB")

    ## GNB: check that holiday() accepts also a function or a list of functions
    checkIdentical(holiday(2022, "GoodFriday"), holiday(2022, GoodFriday))
    checkIdentical(holiday(2022, c("GoodFriday", "Easter")),                  
                   holiday(2022, c(GoodFriday, Easter)))

    ## GNB: test fix for issue #1288
    ex_stephan <- c("2011-01-03", "2011-02-21", "2011-04-22", "2011-05-23", "2011-07-01",
                    "2011-08-01", "2011-09-05","2011-10-10", "2011-12-26", "2011-12-27")
    tdStefan <-  timeDate(ex_stephan, zone = "Toronto", FinCenter = "Toronto")
    
    checkIdentical(holidayTSX(2011), tdStefan)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.easter =
function()
{
    # Easter:
    Easter()

    # Old Function stoll available for compatibility
    timeDate:::.easter()

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.listHolidays =
function()
{
    # Holiday List:
    listHolidays()

    # Return Value:
    return()
}


################################################################################

