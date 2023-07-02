# test.prolog.R

# A safe version of sprintf.
# Like sprintf except that %s on NULL prints "NULL" rather than
# preventing the entire string from being printed
#
# e.g. sprintf("abc %s def", NULL) returns an empty string -- a silent failure!
# but   sprint("abc %s def", NULL) returns "abc NULL def"
#
# e.g. sprintf("abc %d def", NULL) returns an empty string!
# but   sprint("abc %d def", NULL) causes an error msg (not a silent failure)
sprint <- function(fmt, ...)
{
    dots <- list(...)
    dots <- lapply(dots, function(e) if(is.null(e)) "NULL" else e)
    do.call(sprintf, c(fmt, dots))
}
printf <- function(fmt, ...) cat(sprint(fmt, ...), sep="")
cat0 <- function(...) cat(..., sep="")
strip.space <- function(s) gsub("[ \t\n]", "", s)
# test that we got an error as expected from a try() call
expect.err <- function(object, expected.msg="")
{
    if(class(object)[1] != "try-error")
        stop("Did not get expected error: ", expected.msg)
    else {
        msg <- attr(object, "condition")$message[1]
        if(length(grep(expected.msg, msg, fixed=TRUE)))
            cat0("Got expected error from ",
                 deparse(substitute(object)), "\n")
        else
            stop(sprint("Expected: %s\n  Got:      %s",
                        expected.msg, substr(msg[1], 1, 1000)))
    }
}
empty.plot <- function()
{
    plot(0, 0, col=0, bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
}
options(warn=1) # print warnings as they occur
if(!interactive())
    postscript(paper="letter")
org.par <- par(no.readonly=TRUE)
set.seed(2020)
