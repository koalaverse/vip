# test.big: test earth with a biggish model

source("test.prolog.R")
library(earth)
options(digits=3)

p <- 100
n <- 20000 # big enough to cross ten-thousand-cases barrier in plotres and plotmo

# p <- 100; n <- 10e6 # earth 4.4.0: Error in forward.pass: Out of memory (could not allocate 15 GB)
                      # ok with nk=21, 42 minutes to build model
                      # earth 4.6.3: Stopped after two hours, much memory paging

# p <- 100; n <- 9e6  # windows 64 bit system, 2.9 GHz i7, 32 gig ram, SSD drive:
                      # earth 4.4.0: Out of memory (could not allocate 15 GB)
                      # earth 4.6.3: ok (earth now uses .Call instead of .C to invoke ForwardPassR)
                      #              55 mins to build model

# p <- 100; n <- 8e6  # 51 minutes to build model, additional 1.5 minutes for plotmo and plotres

# p <- 2; n <- 60e6   # ok

# p <- 2; n <- 80e6   # ok (but not enough memory to get leverages)
#                     # 18 minutes to build model, additional 8 minutes for plotmo and plotres
#
# p <- 2; n <- 100e6  # earth 4.6.3: thrashes, interupted after a few hours
#                     # earth 4.4.0 Error in leaps.setup: Reached total allocation of 32673Mb
#                     # ok with nk=11, not so much thrashing, 10 minutes

cat("creating x\n")
ran <- function() runif(n, min=-1, max=1)
x <- matrix(ran(), ncol=1)
if(p >= 2)
    x <- cbind(x, ran())
if(p >= 3)
    x <- cbind(x, ran())
if(p >= 4) {
    # xran saves time generating x, ok because func uses only columns x1, x2, and x3
    xran <- ran()
    x <- cbind(x, matrix(xran, nrow=n, ncol=p-3))
}
colnames(x) <- paste("x", 1:ncol(x), sep="")
func <- function(x) # additive, no interactions
{
    y <- sin(4 * x[,1])
    if(p > 1)
        y <- y + x[,2]
    if(p > 2)
        y <- y + 2 * x[,3]^2 - 1
    y
}
cat("creating y\n")
y <- func(x)
cat("testing memory handling when an error (Adjust.endspan = -999)\n")
dummy.allowed <- function(degree, pred, parents) TRUE
expect.err(try(earth(x, y, trace=1.5, allowed=dummy.allowed, Adjust.endspan = -999)), "Adjust.endspan is -999 but should be between 0 and 10")
cat("calling earth\n")
start.time <- proc.time()
a <- earth(x, y, degree=1, trace=1.5)
if(interactive())
    printf("n %g p %g: earth time %.3f seconds (%.3f minutes)\n",
        n, p,
        (proc.time() - start.time)[3],
        (proc.time() - start.time)[3] / 60)
cat("print(summary(a1)):\n")
print(summary(a))
invisible(gc())
cat("calling plotmo\n")
plotmo(a, trace=-1)
invisible(gc())
cat("calling plotres\n")
set.seed(2015) # TODO this is necessary, why?
plot(a, trace=1)
if(interactive()) {
    printf("n %g p %g: total time %.3f seconds (%.3f minutes)\n",
         n, p,
        (proc.time() - start.time)[3],
        (proc.time() - start.time)[3] / 60)
    x <- y <- 0 # free memory by reducing size of large matrices
    gc()        # release memory back to operating system
}
source("test.epilog.R")
