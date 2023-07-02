# test.expand.bpairs.R:

source("test.prolog.R")
library(earth)
options(warn=1) # print warnings as they occur

# 5 cases (11 obs)
sex <- factor(c("m","f","f","f","f"))
pclass <- factor(c("1st", "2nd", "3rd", "3rd", "3rd"))
x.short <- data.frame(dose=1L:5L,
                      numericx=c(1.1,1.2,1.3,1.4,1.5),
                      logicalx=c(TRUE,FALSE,TRUE,FALSE,TRUE),
                      sex=sex,
                      pclass=pclass)
y.short <- data.frame(success=c(1,2,3,0,1),
                      fail   =c(1,1,1,0,0))
short <- data.frame(x.short, y.short)
x.short.unsorted <- x.short[nrow(x.short):1, ]
y.short.unsorted <- y.short[nrow(y.short):1, ]
short.unsorted <- data.frame(x.short.unsorted, y.short.unsorted)
long <- data.frame(
            success =c(    F,    T,    F,    T,    T,    F,    T,    T,    T,    F,    T),
            dose    =c(   1L,   1L,   2L,   2L,   2L,   3L,   3L,   3L,   3L,   4L,   5L),
            numericx=c(  1.1,  1.1,  1.2,  1.2,  1.2,  1.3,  1.3,  1.3,  1.3,  1.4,  1.5),
            logicalx=c(    T,    T,    F,    F,    F,    T,    T,    T,    T,    F,    T),
            sex     =factor(c(  "m",  "m",  "f",  "f",  "f",  "f",  "f",  "f",  "f",  "f",  "f")),
            pclass  =factor(c("1st","1st","2nd","2nd","2nd","3rd","3rd","3rd","3rd","3rd","3rd")))
bpairs.index <- c(1L, 3L, 6L, 10L, 11L)
ynames <- c("success", "fail")

check.expanded.bpairs <- function(long.expanded, long.ref, bpairs.index.ref, ynames.ref)
{
    stopifnot(rownames(long.expanded)[1] == "row1.1") # basic sanity check
    # delete attributes so can check identical
    stripped.long.expanded <- long.expanded
    rownames(stripped.long.expanded) <- 1:nrow(long.expanded)
    attr(stripped.long.expanded, "bpairs.index") <- NULL
    attr(stripped.long.expanded, "ynames") <- NULL
    if(!identical(stripped.long.expanded, long.ref)) {
        printf("\n---print.default(stripped.long.expanded)------\n")
        print.default(stripped.long.expanded)
        printf("\n---print.default(long.ref)--------------------\n")
        print.default(long.ref)
        printf("\n----------------------------------------------\n")

        stop("!identical(stripped.long.expanded, long.ref), see above prints")
    }
    stopifnot(identical(attr(long.expanded, "bpairs.index"), bpairs.index.ref))
    stopifnot(identical(attr(long.expanded, "ynames"), ynames.ref))
}
cat("expand.bpairs(x.short, y.short)\n")
long.default <- expand.bpairs(x.short, y.short)
check.expanded.bpairs(long.default, long, bpairs.index, ynames)

long.default.sort <- expand.bpairs(x.short.unsorted, y.short.unsorted, sort=TRUE)
attr(long.default.sort, "row.names") <- NULL
attr(long.default.sort, "ynames") <- NULL
long1 <- long
rownames(long1) <- NULL
attr(long1, "row.names") <- NULL
attr(long1, "bpairs.index") <- NULL
stopifnot(all.equal(long.default.sort, long1))

# single predictor "dose"
cat("expand.bpairs(expand.bpairs(short$dose, y.short)\n")
long.default.dose <- expand.bpairs(short$dose, y.short)
colnames(long.default.dose)[2] <- "dose" # needed for check because above produces column name "x"
check.expanded.bpairs(long.default.dose, long[,c("success", "dose")], bpairs.index, ynames)

# use a two element numeric vector to specify the y columns
cat("expand.bpairs(short.data.frame, c(6,7))\n")
short.data.frame <- data.frame(x.short, y.short)
long.colindex <- expand.bpairs(short.data.frame, c(6,7))
check.expanded.bpairs(long.colindex, long, bpairs.index, ynames)

# use a two element numeric vector to specify the y columns, single predictor "dose"
cat("expand.bpairs(short.data.frame.dose, c(2,3))\n")
short.data.frame.dose <- data.frame(dose=x.short$dose, y.short)
long.default.dose <- expand.bpairs(short.data.frame.dose, c(2,3))
check.expanded.bpairs(long.default.dose, long[,c("success", "dose")], bpairs.index, ynames)

# use a two element character vector to specify the y columns
cat("expand.bpairs(short.data.frame, c(\"success\",\"fail\"))\n")
short.data.frame <- data.frame(x.short, y.short)
long.charindex <- expand.bpairs(short.data.frame, c("success", "fail"))
check.expanded.bpairs(long.charindex, long, bpairs.index, ynames)

# use a two element character vector to specify the y columns, single predictor "dose"
cat("expand.bpairs(short.data.frame.dose, c(2,3))\n")
short.data.frame.dose <- data.frame(dose=x.short$dose, y.short)
long.default.charindex.dose <- expand.bpairs(short.data.frame.dose, c(2,3))
check.expanded.bpairs(long.default.charindex.dose, long[,c("success", "dose")], bpairs.index, ynames)

expect.err(try(expand.bpairs()), "expand.bpairs: no y argument")
expect.err(try(expand.bpairs(short.data.frame.dose)), "expand.bpairs: no y argument")
expect.err(try(expand.bpairs(short.data.frame.dose, c(2,3), nonesuch=99)), "expand.bpairs.default: unrecognized argument 'nonesuch'")
expect.err(try(expand.bpairs(short.data.frame, c(5,6))), "short.data.frame[,c(5,6)] is not a two-column matrix of binomial pairs")
expect.err(try(expand.bpairs(short.data.frame, 1)), "expand.bpairs: bad y argument '1'")
expect.err(try(expand.bpairs(short.data.frame, c(1,2,3))), "bad y argument 'c(1, 2, 3)'")
expect.err(try(expand.bpairs(short.data.frame, c(1,2))), "expand.bpairs: short.data.frame[,c(1,2)] is not a two-column matrix of binomial pairs")
expect.err(try(expand.bpairs(short.data.frame, c(99,100))), "'ycolumns' is out of range, allowed values are 1 to 7")
expect.err(try(expand.bpairs(short.data.frame, c("success99", "fail"))), "undefined columns selected")
expect.err(try(expand.bpairs(short.data.frame, c("nonesuch", "fail"))), "undefined columns selected")
expect.err(try(expand.bpairs(short.data.frame, "nonesuch")), "bad y argument 'nonesuch'")
expect.err(try(expand.bpairs(short.data.frame, nonesuch)), "object 'nonesuch' not found")
options(warn=2) # treat warnings as errors
expect.err(try(expand.bpairs(short.data.frame, c("nonesuch", "fail"))),   "\"nonesuch\" in ycolumns does not match any names")
expect.err(try(expand.bpairs(short.data.frame, c("fail", "nonesuch99"))), "\"nonesuch99\" in ycolumns does not match any names")
expect.err(try(expand.bpairs(short.data.frame, c("", "fail"))), "ycolumns[1] is an empty string \"\"")
expect.err(try(expand.bpairs(short.data.frame, c("success", ""))), "ycolumns[2] is an empty string \"\"")
options(warn=1) # print warnings as they occur
try(expand.bpairs(short.data.frame, c("success", ""))) # check error messages that are issued after the warning

# formula
cat("expand.bpairs(success.fail~., data=x.short)\n")
success.fail <- cbind(success=short$success, fail=short$fail)
long.formula.matrix <- expand.bpairs(success.fail~., data=x.short)
check.expanded.bpairs(long.formula.matrix, long, bpairs.index, ynames)

cat("expand.bpairs(success+fail~., data=x.short)\n")
xy.short <- data.frame(y.short, x.short)
long.formula <- expand.bpairs(success+fail~., data=xy.short)
check.expanded.bpairs(long.formula, long, bpairs.index, ynames)

long.formula.sort <- expand.bpairs(x.short, y.short, sort=TRUE)
long.formula.sort <- expand.bpairs(x.short.unsorted, y.short.unsorted, sort=TRUE)
attr(long.formula.sort, "row.names") <- NULL
attr(long.formula.sort, "ynames") <- NULL
long1 <- long
rownames(long1) <- NULL
attr(long1, "row.names") <- NULL
attr(long1, "bpairs.index") <- NULL
stopifnot(all.equal(long.formula.sort, long1))

expand.bpairs(success+fail+fail~., data=xy.short) # ok, duplicated name gets dropped
expect.err(try(expand.bpairs(success~., data=xy.short)), "expand.bpairs: 'success' does not have two columns")
expect.err(try(expand.bpairs(success+success~., data=xy.short)), "expand.bpairs: 'success + success' does not have two columns")

cat("expand.bpairs(success.fail~., data=x.short)\n")
success.fail <- cbind(success=short$success, fail=short$fail)
long.formula.matrix <- expand.bpairs(success.fail~., data=x.short)
check.expanded.bpairs(long.formula.matrix, long, bpairs.index, ynames)

# TODO it's a pity the following doesn't work (issue is in model.frame.default)
cat("expand.bpairs(data.frame(success.fail)~., data=x.short)\n")
expect.err(try(expand.bpairs(data.frame(success.fail)~., data=x.short)), "invalid type (list) for variable 'data.frame(success.fail)'")

# formula, single predictor "dose"
cat("expand.bpairs(expand.bpairs(success+fail~dose, data=xy.short)\n")
long.formula.dose <- expand.bpairs(success+fail~dose, data=xy.short)
check.expanded.bpairs(long.formula.dose, long[,c("success", "dose")], bpairs.index, ynames)

trues <- xy.short$success
falses <- xy.short$fail
cat("expand.bpairs(expand.bpairs(trues+falses~dose, data=x.short)\n")
long.formula.dose <- expand.bpairs(trues+falses~~dose, data=xy.short)
stopifnot(identical(colnames(long.formula.dose), c("trues", "dose")))
colnames(long.formula.dose) <- c("success", "dose")
attr(long.formula.dose, "ynames") <- c("success", "fail")
check.expanded.bpairs(long.formula.dose, long[,c("success", "dose")], bpairs.index, ynames)

cat("expand.bpairs(expand.bpairs(trues+falses~., data=x.short)\n")
long.formula <- expand.bpairs(trues+falses~., data=xy.short)
stopifnot(identical(colnames(long.formula), c("trues", "success", "fail", "dose", "numericx", "logicalx", "sex", "pclass")))

cat("expand.bpairs(expand.bpairs(success.fail~dose, data=x.short)\n")
long.formula.dose <- expand.bpairs(success.fail~dose, data=x.short)
check.expanded.bpairs(long.formula.dose, long[,c("success", "dose")], bpairs.index, ynames)

cat("expand.bpairs(expand.bpairs(success.fail~dose, data=xy.short)\n")
long.formula.dose <- expand.bpairs(success.fail~dose, data=xy.short)
check.expanded.bpairs(long.formula.dose, long[,c("success", "dose")], bpairs.index, ynames)

x.short.na <- x.short
x.short.na$dose[3] <- NA
long.na <- long
long.na$dose[6:9] <- NA
# formula with NAs in data
cat("expand.bpairs(success.fail~., data=x.short.na)\n")
long.formula.na <- expand.bpairs(success.fail~., data=x.short.na)
check.expanded.bpairs(long.formula.na, long.na, bpairs.index, ynames)

# formula with NAs in data, single predictor "dose"
cat("expand.bpairs(success.fail~dose., data=x.short.na)\n")
long.formula.dose.na <- expand.bpairs(success.fail~dose, data=x.short.na)
check.expanded.bpairs(long.formula.dose.na, long.na[,c("success", "dose")], bpairs.index, ynames)

expect.err(try(expand.bpairs(nonesuch~., data=x.short)), "object 'nonesuch' not found")
expect.err(try(expand.bpairs(dose~., data=x.short)), "'dose' does not have two columns")
expect.err(try(expand.bpairs(dose~success.fail, data=x.short)), "'dose' does not have two columns")

# # # check Warning: dropping column 'success' from x because it matches a column name in y
# # TODO Removed because we (intentionally) no longer give a warning
long.formula <- expand.bpairs(success.fail~., data=xy.short)
check.expanded.bpairs(long.formula, long, bpairs.index, ynames)
# options(warn=2)
# # expect.err(try(expand.bpairs(success.fail~., data=xy.short)), "(converted from warning) dropping column 'success' from x because it matches a column name in y")
# # options(warn=1)
long.formula.dose <- expand.bpairs(success.fail~dose, data=xy.short)
check.expanded.bpairs(long.formula.dose, long[,c("success", "dose")], bpairs.index, ynames)

old.success.fail <- success.fail
success.fail <- 99
expect.err(try(expand.bpairs(success.fail~., data=xy.short)), "variable lengths differ (found for 'success')")
success.fail <- old.success.fail

# example with short data as a matrix (not a data.frame)
short <- matrix(c( 5, 2, 2, 9, 5, 9,
                 20,20,30,20,20,30), ncol=2)
colnames(short) <- c("dose", "temp")
success.fail <- matrix(c(1,2,0,2,2,0,
                         3,3,1,0,1,0), ncol=2)
long <- matrix(c(
        0,    5,   20,
        0,    5,   20,
        0,    5,   20,
        1,    5,   20,

        0,    2,   20,
        0,    2,   20,
        0,    2,   20,
        1,    2,   20,
        1,    2,   20,

        0,    2,   30,

        1,    9,   20,
        1,    9,   20,

        0,    5,   20,
        1,    5,   20,
        1,    5,   20,

        0,    9,   30),  # both rows zero in short data, so treat as a "false",
    ncol=3, byrow=TRUE)
colnames(long) <- c("V1", "dose", "temp")
long <- as.data.frame(long)
bpairs.index <- c(1L, 5L, 10L, 11L, 13L, 16L)
ynames <- c("V1", "V2")

long.default <- expand.bpairs(short, success.fail)
long.default$V1 <- as.numeric(long.default$V1) # convert TRUE/FALSE to 0/1
check.expanded.bpairs(long.default, long, bpairs.index, ynames)
# man page for expand.bpairs
example(expand.bpairs)
# man page for expand.bpairs, do it manually and check
survived <- c(3,2,1,1)
died     <- c(0,1,2,2)
dose <- c(10,10,20,20)
sex  <- factor(c("male", "female", "male", "female"))
short.data <- data.frame(survived, died, dose, sex)
long.data <- expand.bpairs(survived + died ~ ., short.data) # returns long form of the data
print(long.data)
stopifnot(identical(expand.bpairs(data=short.data, y=cbind(survived, died)), long.data)) # equivalent
stopifnot(identical(expand.bpairs(short.data, c(1,2)), long.data))                    # equivalent
stopifnot(identical(expand.bpairs(short.data, c("survived", "died")), long.data))     # equivalent
pairs(short.data, main="short.data")
pairs(long.data, main="long.data")

# test without column names
short.unsorted.nocolnames <- short.unsorted
colnames(short.unsorted.nocolnames) <- NULL
temp <- expand.bpairs(short.unsorted, 6:7)
temp.nocolnames <- expand.bpairs(short.unsorted.nocolnames, 6:7)
stopifnot(all.equal(colnames(temp.nocolnames), c("true", "X1", "X2", "X3", "X4", "X5")))
colnames(temp.nocolnames) <- colnames(temp)
attr(temp, "ynames") <- NULL
stopifnot(identical(temp.nocolnames, temp))

source("test.epilog.R")
