
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# It is distributed in the hope that it will be useful,
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


test.zurich =
function()
{
    # DST Rules for Zurich:
    head(Zurich())
    tail(Zurich())

    # Return Value:
    return()
}


if ((any(Sys.info()["user"] %in% c("yankee", "chalabi")) && !try(system("zdump")))) {
    test.DST <- function()
    {
        # works only if OS is well configured !!!

        finCenter <- listFinCenter()

        for (k in seq_along(finCenter)) {

            zdump <-
                try(system(paste("zdump ", finCenter[k], sep=" "), intern=TRUE))
            zdump <- strsplit(zdump, " +" )
            zdump <- unlist(zdump)


            dts <- paste(zdump[c(3, 4, 6)], collapse = " ")
            tms <- zdump[5]
            timeSys <- timeDate(paste(dts, tms), format =  "%b %d %Y %H:%M:%S",
                                zone = finCenter[k], FinCenter = finCenter[k])


            timeTest <- Sys.timeDate(finCenter[k])

            # round and compare
            cat("\nSimple DST test for", finCenter[k], "\n")
            cat("System\t\t", as.character(timeSys), "\n")
            cat("timeDate\t", as.character(timeTest), "\n")
            checkTrue(abs(as.numeric(timeSys - timeTest)) < 5)
        }
    }

}


# ------------------------------------------------------------------------------

test.dst1.print <-
    function()
{

    from <- '2008-03-30'
    to <- '2008-03-31'

    # make sure that DST is the same as with POSIXct
    tseq1 <- timeSequence( from = from, to = to,
                          by = "hour", zone = "GMT", FinCenter = "GMT")
    tseq1

    tseq1Test <- c(
                   "2008-03-30 00:00:00", "2008-03-30 01:00:00",
                   "2008-03-30 02:00:00", "2008-03-30 03:00:00",
                   "2008-03-30 04:00:00", "2008-03-30 05:00:00",
                   "2008-03-30 06:00:00", "2008-03-30 07:00:00",
                   "2008-03-30 08:00:00", "2008-03-30 09:00:00",
                   "2008-03-30 10:00:00", "2008-03-30 11:00:00",
                   "2008-03-30 12:00:00", "2008-03-30 13:00:00",
                   "2008-03-30 14:00:00", "2008-03-30 15:00:00",
                   "2008-03-30 16:00:00", "2008-03-30 17:00:00",
                   "2008-03-30 18:00:00", "2008-03-30 19:00:00",
                   "2008-03-30 20:00:00", "2008-03-30 21:00:00",
                   "2008-03-30 22:00:00", "2008-03-30 23:00:00",
                   "2008-03-31 00:00:00")

    checkIdentical(tseq1Test, format(tseq1))

    # make sure that tseq1@Data is a continuous time
    checkIdentical(tseq1Test, format(tseq1@Data))

    # make sure that DST is the same as with POSIXct
    tseq2 <- timeSequence( from = from, to = to,
                          by = "hour", zone = "GMT", FinCenter = "Zurich")
    tseq2

    ## make sure that tseq1@Data is a continuous time
    checkIdentical(tseq1Test, format(tseq2@Data))

    # test taken from format(tseq2@Data, tz = "Europe/Zurich")
    tseq2Test <- c(
        ## GNB: there is no "2008-03-30 01:00:00" in FinCenter "Zurich".
        ##      was: "2008-03-30 01:00:00", "2008-03-30 03:00:00",
        ## 
                   "2008-03-30 02:00:00", "2008-03-30 03:00:00",
                   "2008-03-30 04:00:00", "2008-03-30 05:00:00",
                   "2008-03-30 06:00:00", "2008-03-30 07:00:00",
                   "2008-03-30 08:00:00", "2008-03-30 09:00:00",
                   "2008-03-30 10:00:00", "2008-03-30 11:00:00",
                   "2008-03-30 12:00:00", "2008-03-30 13:00:00",
                   "2008-03-30 14:00:00", "2008-03-30 15:00:00",
                   "2008-03-30 16:00:00", "2008-03-30 17:00:00",
                   "2008-03-30 18:00:00", "2008-03-30 19:00:00",
                   "2008-03-30 20:00:00", "2008-03-30 21:00:00",
                   "2008-03-30 22:00:00", "2008-03-30 23:00:00",
                   "2008-03-31 00:00:00", "2008-03-31 01:00:00",
                   "2008-03-31 02:00:00")

    ##
    ## GNB:
    ## TODO: by = "hour" works with numerics, directly creates POSIXct object,
    ##       and doesn't call timeDate (it creates the object with new("timeDate", ...)
    ##       That needs a separate fix to move the nonexistent "2008-03-30 01:00:00" in
    ##       Zurich. See also the remarks for 'tseq2Test'
    ## TODO: temporarilly, don't compare the first elements
    ##
    checkIdentical(tseq2Test, format(tseq2))
    
    ## @Data slot should be the same for both object
    checkIdentical(tseq1@Data, tseq2@Data)

    # should be of length length(tseq2) - 1
    tseq3 <- timeSequence( from = from, to = to,
                          by = "hour", zone = "Zurich", FinCenter = "Zurich")
    tseq3

    # test taken from format(tseq3@Data, tz = "Europe/Zurich")
    tseq3Test <- c(
        ## GNB: "2008-03-30 01:00:00" doesn't exist in "Zurich"
        ##      was: "2008-03-30 00:00:00", "2008-03-30 01:00:00",
                   "2008-03-30 00:00:00", "2008-03-30 02:00:00",
                   "2008-03-30 03:00:00", "2008-03-30 04:00:00",
                   "2008-03-30 05:00:00", "2008-03-30 06:00:00",
                   "2008-03-30 07:00:00", "2008-03-30 08:00:00",
                   "2008-03-30 09:00:00", "2008-03-30 10:00:00",
                   "2008-03-30 11:00:00", "2008-03-30 12:00:00",
                   "2008-03-30 13:00:00", "2008-03-30 14:00:00",
                   "2008-03-30 15:00:00", "2008-03-30 16:00:00",
                   "2008-03-30 17:00:00", "2008-03-30 18:00:00",
                   "2008-03-30 19:00:00", "2008-03-30 20:00:00",
                   "2008-03-30 21:00:00", "2008-03-30 22:00:00",
                   "2008-03-30 23:00:00", "2008-03-31 00:00:00")

    checkIdentical(tseq3Test, format(tseq3))


    ## more by GNB

    ## Sofia GMT+2
    around_midnight_char <- c("1983-03-26 23:00:00",
                           "1983-03-27 00:00:00", # change to DST; doesn't exist in Sofia DST
                           "1983-03-27 01:00:00",
                           "1983-03-27 02:00:00",
                           "1983-03-27 03:00:00")

    Sofia_to_DST_test <- around_midnight_char
    Sofia_to_DST_test[2] <- "1983-03-27 01:00:00" # gap to gap + 1 hour
    
    Sofia_to_DST <- timeDate(around_midnight_char, zone = "Sofia", FinCenter = "Sofia")

    checkIdentical(format(Sofia_to_DST), Sofia_to_DST_test)

    ## London
    London_to_DST_test <- around_midnight_char
    London_to_DST_test[3] <- "1983-03-27 02:00:00" # gap to gap + 1 hour
    
    London_to_DST <- timeDate(around_midnight_char, zone = "London", FinCenter = "London")

    checkIdentical(format(London_to_DST), London_to_DST_test)

    ## Zurich
    Zurich_to_DST_test <- around_midnight_char
    Zurich_to_DST_test[3] <- "1983-03-27 02:00:00" # gap to gap + 1 hour
    
    Zurich_to_DST <- timeDate(around_midnight_char, zone = "Zurich", FinCenter = "Zurich")

    checkIdentical(format(Zurich_to_DST), Zurich_to_DST_test)


    

    
    ## more by GNB -  TODO: consolidate and turn into proper tests

    ## London
    timeSequence(from = "2004-03-26 01:00:00", to = "2004-04-01 01:00:00",
                 by = "DSTday", zone = "London", FinCenter = "London")

    timeSequence(from = "2004-03-26 01:00:00", to = "2004-04-01 01:00:00",
                 by = "DSTday", zone = "London", FinCenter = "London")@Data

    timeDate("2004-03-26 01:00:00", zone = "London", FinCenter = "London")
    timeDate("2004-03-26 01:00:00", zone = "London", FinCenter = "London")@Data

    timeDate("2004-04-01 01:00:00", zone = "London", FinCenter = "London")
    timeDate("2004-04-01 01:00:00", zone = "London", FinCenter = "London")@Data

    
    timeDate(c("2004-03-28 00:00:00", "2004-03-28 01:00:00", "2004-03-28 02:00:00",
               "2004-03-28 03:00:00", "2004-03-28 04:00:00"),
             zone = "London", FinCenter = "London")
    
    timeDate(c("2004-03-28 00:00:00", "2004-03-28 01:00:00", "2004-03-28 02:00:00",
               "2004-03-28 03:00:00", "2004-03-28 04:00:00"),
             zone = "London", FinCenter = "London")@Data


    timeDate(c("2004-03-26 00:00:00", "2004-03-28 01:00:00", "2004-03-28 02:00:00",
               "2004-03-28 03:00:00", "2004-03-28 04:00:00"),
             zone = "London", FinCenter = "London")

    ## Zurich
    timeSequence(from = "2004-03-26 02:00:00", to = "2004-04-01 02:00:00",
                 by = "DSTday", zone = "Zurich", FinCenter = "Zurich")
    
    timeSequence(from = "2004-03-26 02:00:00", to = "2004-04-01 02:00:00",
                 by = "DSTday", zone = "Zurich", FinCenter = "Zurich")@Data

    timeDate(c("2004-03-26 00:00:00", "2004-03-28 01:00:00", "2004-03-28 02:00:00",
               "2004-03-28 03:00:00", "2004-03-28 04:00:00"),
             zone = "Zurich", FinCenter = "Zurich")

    
    ## London BDST during war    
    timeDate(c("1941-05-04 00:00:00", "1941-05-04 01:00:00", "1941-05-04 02:00:00",
               "1941-05-04 03:00:00", "1941-05-04 04:00:00"),
             zone = "London", FinCenter = "London")@Data
    
    timeDate(c("1941-05-04 00:00:00", "1941-05-04 01:00:00", "1941-05-04 02:00:00",
               "1941-05-04 03:00:00", "1941-05-04 04:00:00"),
             zone = "London", FinCenter = "London")

    timeSequence(from = "1941-05-02 02:00:00", to = "1941-05-06 02:00:00",
                 by = "DSTday", zone = "London", FinCenter = "London")
    
}

# ------------------------------------------------------------------------------

test.dst2.print <-
    function()
{

    from = '2008-10-26'
    to = '2008-10-27'

    # make sure that DST is the same as with POSIXct
    tseq1 <- timeSequence( from = from, to = to,
                          by = "hour", zone = "GMT", FinCenter = "GMT")
    tseq1

    tseq1Test <- c(
                   "2008-10-26 00:00:00", "2008-10-26 01:00:00",
                   "2008-10-26 02:00:00", "2008-10-26 03:00:00",
                   "2008-10-26 04:00:00", "2008-10-26 05:00:00",
                   "2008-10-26 06:00:00", "2008-10-26 07:00:00",
                   "2008-10-26 08:00:00", "2008-10-26 09:00:00",
                   "2008-10-26 10:00:00", "2008-10-26 11:00:00",
                   "2008-10-26 12:00:00", "2008-10-26 13:00:00",
                   "2008-10-26 14:00:00", "2008-10-26 15:00:00",
                   "2008-10-26 16:00:00", "2008-10-26 17:00:00",
                   "2008-10-26 18:00:00", "2008-10-26 19:00:00",
                   "2008-10-26 20:00:00", "2008-10-26 21:00:00",
                   "2008-10-26 22:00:00", "2008-10-26 23:00:00",
                   "2008-10-27 00:00:00")

    checkIdentical(tseq1Test, format(tseq1))

    # make sure that tseq1@Data is a continuous time
    checkIdentical(tseq1Test, format(tseq1@Data))

    # make sure that DST is the same as with POSIXct
    tseq2 <- timeSequence( from = from, to = to,
                          by = "hour", zone = "GMT", FinCenter = "Zurich")
    tseq2

    # make sure that tseq2@Data is a also continuous time
    checkIdentical(tseq1Test, format(tseq2@Data))

    # test taken from format(tseq2@Data, tz = "Europe/Zurich")
    tseq2Test <- c(
                   "2008-10-26 02:00:00", "2008-10-26 02:00:00",
                   "2008-10-26 03:00:00", "2008-10-26 04:00:00",
                   "2008-10-26 05:00:00", "2008-10-26 06:00:00",
                   "2008-10-26 07:00:00", "2008-10-26 08:00:00",
                   "2008-10-26 09:00:00", "2008-10-26 10:00:00",
                   "2008-10-26 11:00:00", "2008-10-26 12:00:00",
                   "2008-10-26 13:00:00", "2008-10-26 14:00:00",
                   "2008-10-26 15:00:00", "2008-10-26 16:00:00",
                   "2008-10-26 17:00:00", "2008-10-26 18:00:00",
                   "2008-10-26 19:00:00", "2008-10-26 20:00:00",
                   "2008-10-26 21:00:00", "2008-10-26 22:00:00",
                   "2008-10-26 23:00:00", "2008-10-27 00:00:00",
                   "2008-10-27 01:00:00")

    # make sure that DST is the same as with POSIXct
    checkIdentical(tseq2Test, format(tseq2))

    # @Data slot should be the same for both object
    checkIdentical(tseq1@Data, tseq2@Data)

    # # should be of length length(tseq2) - 1
    tseq3 <- timeSequence( from = from, to = to,
                          by = "hour", zone = "Zurich", FinCenter = "Zurich")
    tseq3

    # test taken from format(tseq3@Data, tz = "Europe/Zurich")
    tseq3Test <- c(
                   "2008-10-26 00:00:00", "2008-10-26 01:00:00",
                   "2008-10-26 02:00:00", "2008-10-26 02:00:00",
                   "2008-10-26 03:00:00", "2008-10-26 04:00:00",
                   "2008-10-26 05:00:00", "2008-10-26 06:00:00",
                   "2008-10-26 07:00:00", "2008-10-26 08:00:00",
                   "2008-10-26 09:00:00", "2008-10-26 10:00:00",
                   "2008-10-26 11:00:00", "2008-10-26 12:00:00",
                   "2008-10-26 13:00:00", "2008-10-26 14:00:00",
                   "2008-10-26 15:00:00", "2008-10-26 16:00:00",
                   "2008-10-26 17:00:00", "2008-10-26 18:00:00",
                   "2008-10-26 19:00:00", "2008-10-26 20:00:00",
                   "2008-10-26 21:00:00", "2008-10-26 22:00:00",
                   "2008-10-26 23:00:00", "2008-10-27 00:00:00")

    checkIdentical(tseq3Test, format(tseq3))

}


################################################################################
