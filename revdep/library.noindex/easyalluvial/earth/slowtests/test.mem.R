# test.mem.R: test earth C code memory usage under both normal and error conditions
#
# TODO With some versions of R, test.mem gives different results per run.
#      First seen Sep 2020, R 4.0.3.

source("test.prolog.R")
library(earth)

# the data we will build the models on
ncases <- 10
x <- matrix(1:ncases, ncol=1)
colnames(x) <- "x"
max <- max(x)
y <- sin(3 * x / max(x))
colnames(y) <- "y"

nmodels <- 5
nlm            <- double(length=nmodels) # mem used for each lm model
nstandardearth <- double(length=nmodels) # mem used for each earth model
ngoodallowed   <- double(length=nmodels)
nbadallowed    <- double(length=nmodels)
nbadendspan    <- double(length=nmodels)

max.mem.change <- function(mem.start, gc.start)
{
    mem <- memory.size() # MBytes (on non windows platforms, will always be Inf)
    gc <- gc(full=TRUE) # returns cells left after garbage collection
    # max(abs(mem - mem.start),
    #     abs(gc[1,1] - gc.start[1,1]),  # Ncells
    #     abs(gc[2,1] - gc.start[2,1]))  # Vcells
    mem <- abs(mem - mem.start)
    ncells <- abs(gc[1,1] - gc.start[1,1])
    vcells <- abs(gc[2,1] - gc.start[2,1])
    printf("mem %g ncells %g vcells %g\n", mem, ncells, vcells)
    max(mem, ncells, vcells)
}
plotmem <- function(nlm, nstandardearth, ngoodallowed, nbadallowed, nbadendspan)
{
    min <- min(nlm, nstandardearth, ngoodallowed, nbadallowed, nbadendspan)
    max <- max(nlm, nstandardearth, ngoodallowed, nbadallowed, nbadendspan)
    min <- min - 1
    max <- max + 3
    yjitter <- (max - min) / 130 # minimize overplotting

    # in the graphs, lines should be horizontal (at least after the first iter)
    # if a line increases after the first iter, it means that memory is not being released
    plot( 1:nmodels, nlm, type="l", main="memory used by each model",
                          xlab="nmodels", ylab="memory change", ylim=c(min, max))
    lines(1:nmodels, nstandardearth + 1 * yjitter, col=2)
    lines(1:nmodels, ngoodallowed   + 2 * yjitter, col=3)
    lines(1:nmodels, nbadallowed    + 3 * yjitter, col=1, lty=2)
    lines(1:nmodels, nbadendspan    + 4 * yjitter, col=2, lty=2)

    legend(x="topright", bg="white",
           legend=c("lm", "standardearth", "goodallowed", "badallowed", "badendspan"),
           lty=c(1,1,1,2,2),
           col=c(1,2,3,1,2))
}
good.allowedfunc  <- function(degree, pred, parents, namesx, first)
{
    pred != 999
}
bad.allowedfunc  <- function(degree, pred, parents, namesx, first)
{
    # this stop is silent because call earth using try(..., silent=TRUE)
    stop("early exit from bad.allowedfunc")
}
cat("initial redundant run of lm\n") # else initial nlm very large
                                     # (probably because some function is allocating a static buffer)
print(summary(lm(y~x)))
for(i in 0:nmodels) {
    try(lm(y~x), silent=FALSE)
    gc <- gc(full=TRUE)
    if(i <= 0) {
        mem.start <- memory.size()
        gc.start <- gc(full=TRUE)
    } else
        nlm[i] <- max.mem.change(mem.start, gc.start)
}
cat("actual run of lm\n")
# We use 0:nmodels, because we build the first model at iter 0,
# but don't save results from iter 0 (i.e. we the ignore first model).
# This is because the first model sometimes leaves some memory allocated (why?).
print(summary(lm(y~x)))
for(i in 0:nmodels) {
    try(lm(y~x), silent=FALSE)
    gc <- gc(full=TRUE)
    if(i <= 0) {
        mem.start <- memory.size()
        gc.start <- gc(full=TRUE)
    } else
        nlm[i] <- max.mem.change(mem.start, gc.start)
}
# standard earth model
cat("earth(y~x)\n")
print(summary(earth(y~x)))
for(i in 0:nmodels) {
    try(earth(y~x), silent=FALSE)
    gc <- gc(full=TRUE)
    if(i <= 0) {
        mem.start <- memory.size()
        gc.start <- gc(full=TRUE)
    } else
        nstandardearth[i] <- max.mem.change(mem.start, gc.start)
}
# earth model with an allowed func
cat("earth(y~x, allowed = good.allowedfunc)\n")
print(summary(earth(y~x, allowed = good.allowedfunc)))
for(i in 0:nmodels) {
    try(earth(y~x, allowed = good.allowedfunc), silent=FALSE)
    gc <- gc(full=TRUE)
    if(i <= 0) {
        mem.start <- memory.size()
        gc.start <- gc(full=TRUE)
    } else
        ngoodallowed[i] <- max.mem.change(mem.start, gc.start)
}
# try earth model with an allowed func which causes an error
cat("earth(y~x, allowed = bad.allowedfunc)\n")
expect.err(try(earth(y~x, allowed = bad.allowedfunc), silent=FALSE), "early exit from bad.allowedfunc")
for(i in 0:nmodels) {
    try(earth(y~x, allowed = bad.allowedfunc), silent=TRUE)
    gc <- gc(full=TRUE)
    if(i <= 0) {
        mem.start <- memory.size()
        gc.start <- gc(full=TRUE)
    } else
        nbadallowed[i] <- max.mem.change(mem.start, gc.start)
}

# try earth model with an arg that causes error in ForwardPass in earth.c
cat("earth(y~x, Adjust.endspan = -999\n")
expect.err(try(earth(y~x, Adjust.endspan = -999), silent=FALSE), "Adjust.endspan is -999 but should be between 0 and 10")
for(i in 0:nmodels) {
    try(earth(y~x, Adjust.endspan = -999), silent=TRUE)
    gc <- gc(full=TRUE)
    if(i <= 0) {
        mem.start <- memory.size()
        gc.start <- gc(full=TRUE)
    } else
        nbadendspan[i] <- max.mem.change(mem.start, gc.start)
}

cat("nlm           "); print(nlm)
cat("nstandardearth"); print(nstandardearth)
cat("ngoodallowed  "); print(ngoodallowed)
cat("nbadallowed   "); print(nbadallowed)
cat("nbadendspan   "); print(nbadendspan)

# printf("\n                             Min       1stQ     Median       Mean      3rdQ        Max\n")
# printf("lm                    %s\n", paste0(sprintf("% 10.3f", summary(nlm)), collapse=" "))
# printf("standardearth         %s\n", paste0(sprintf("% 10.3f", summary(nstandardearth)), collapse=" "))
# printf("goodallowed           %s\n", paste0(sprintf("% 10.3f", summary(ngoodallowed)), collapse=" "))
# printf("badallowed            %s\n", paste0(sprintf("% 10.3f", summary(nbadallowed)), collapse=" "))
# printf("badendspan            %s\n", paste0(sprintf("% 10.3f", summary(nbadendspan)), collapse=" "))

# plot the data we are modeling
plot(1:nrow(x), y, type="b", pch=20, xlab="x", main="the data we are modeling")

# plot memory used for each model
plotmem(nlm, nstandardearth, ngoodallowed, nbadallowed, nbadendspan)

source("test.epilog.R")
