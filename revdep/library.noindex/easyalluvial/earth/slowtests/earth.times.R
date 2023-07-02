# earth.times.R

library(earth)
library(mda)   # for Hastie Tibs version of mars
set.seed(2018) # for reproducibility

printf <- function(format, ...) cat(sprintf(format, ...)) # like c printf

# generate robot arm data from Friedman's Fast MARS paper
robotarm <- function(n=1000, p=20)
{
    robotarm1 <- function(x) {
      x. <- with(x, l1 * cos(theta1) - l2 * cos(theta1 + theta2) * cos(phi))
      y  <- with(x, l1 * sin(theta1) - l2 * sin(theta1 + theta2) * cos(phi))
      z  <- with(x, l2 * sin(theta2) * sin(phi))
      sqrt(x.^2 + y^2 + z^2)
    }
    l1     <- runif(n, 0, 1)
    l2     <- runif(n, 0, 1)
    theta1 <- runif(n, 0, 2 * pi)
    theta2 <- runif(n, 0, 2 * pi)
    phi    <- runif(n, -pi/2, pi/2)
    x <- cbind(l1, l2, theta1, theta2, phi)
    for (i in 1:(p-5)) # p-5 dummy variables, so p vars in total
       x <- cbind(x, runif(n, 0, 1))
    x <- data.frame(x)
    y <- robotarm1(x)
    list(x=x, y=y)
}
spacer <- function()
{
    cat("                        |",
        "                                                           |\n", sep="")
}
test <- function(x, y, nk, degree, niter) {
    earth.time <- system.time(for (i in 1:niter)
        earth <- earth(x, y, nk=nk, degree=degree, minspan=0))
    gcv.null <- earth$gcv.per.subset[1]
    grsq <- 1 - earth$gcv/gcv.null

    mars.time <- system.time(for (i in 1:niter)
         mars <- mars(x, y, degree=degree,  nk=nk))
    mars.grsq <- 1 - mars$gcv/gcv.null

    no.fastmars.time <- system.time(for (i in 1:niter)
        no.fastmars <- earth(x, y, nk=nk, degree=degree, minspan=0, fast.k=0))

    no.betacache.time <- system.time(for (i in 1:niter)
        no.betacache <- earth(x, y, nk=nk, degree=degree, minspan=0, Use.beta.cache=FALSE))

    minspan1.time <- system.time(for (i in 1:niter)
        minspan1 <- earth(x, y, nk=nk, degree=degree, minspan=1))

    # dummy func to estimate time taken by an "allowed" function
    allowed.func <- function(degree, pred, parents)
    {
        if (degree > 0 && (parents[1] == 999 || pred == 999))
            return(FALSE) # never get here
        TRUE
    }
    allowed.time <- system.time(for (i in 1:niter)
      allowed <- earth(x, y, nk=nk, degree=degree, minspan=0, allowed=allowed.func))

    niter.weights <- 3 # weights code is very slow
    weights.time <- system.time(for (i in 1: niter.weights)
        weights <- earth(x, y, nk=nk, degree=degree, Force.weights=TRUE))

    # Force.weights=TRUE with minspan=1 is extremely slow
    # if(nrow(x) < 1000)
    #     weights.minspan1.time <- system.time(for (i in 1: niter.weights)
    #         weights.minspan1 <- earth(x, y, nk=nk, degree=degree, Force.weights=TRUE, minspan=1))
    # else { # too slow, skip
    #     weights.minspan1.time <- NA
    #     weights.minspan1 <- list(grsq=NA)
    # }

    cv5.time <- system.time(for (i in 1:niter)
      cv5 <- earth(x, y, nk=nk, degree=degree, minspan=0, nfold=5))

    pmethcv.time <- system.time(for (i in 1:niter)
      pmethcv <- earth(x, y, nk=nk, degree=degree, minspan=0, nfold=5, pmethod="cv"))

    format <- paste(
       # nk degree  nterms time     mars  nofast nobeta minspan1 allowed weights   cv5 pmethcv
        "%2d    %3d   %4.0d %6.3f | %4.1f  %5.1f  %5.1f    %5.1f   %5.1f %7.0f %5.1f   %5.1f ",
      #  grsq  mars      nofast  minspan1  weights pmethcv
        "|  %4.2f %4.2f   %4.2f     %4.2f    %4.2f   %4.2f\n",
        sep="")

    printf(format,
        nk, degree, length(earth$selected.terms), earth.time[1] / niter,
        mars.time[1]            / earth.time[1],
        no.fastmars.time[1]     / earth.time[1],
        no.betacache.time[1]    / earth.time[1],
        minspan1.time[1]        / earth.time[1],
        allowed.time[1]         / earth.time[1],
        (weights.time[1]        / earth.time[1]) * (niter / niter.weights),
        cv5.time[1]             / earth.time[1],
        pmethcv.time[1]         / earth.time[1],
        grsq, mars.grsq, no.fastmars$grsq,
        minspan1$grsq, weights$grsq, pmethcv$grsq)
}
print.header <- function ()
{
    printf("nk degree  earth  earth ")
    printf("| execution time ratio:                                     ")
    printf("| grsq:                                 \n")
    printf("          nterms   time ")
    printf("| mars nofast nobeta minspan1 allowed weights   cv5 pmethcv ")
    printf("| earth mars nofast minspan1 weights pmethcv\n")
    spacer()
}

# data(trees)
# x <- trees[, -3]
# y <- trees[, 3]
# niter <- 500 # repeat calls to earth niter times to average out time variation
# printf("==== trees %d x %d ============\n\n", nrow(x), ncol(x))
# print.header()
# test(x, y, nk=21, degree=1, niter=niter)
# cat("\n")

data(ozone1)
x <- ozone1[,-1]
y <- ozone1[,1]
niter <- 50
printf("==== ozone %d x %d ============\n\n", nrow(x), ncol(x))
print.header()

test(x, y, nk=5, degree=1, niter=niter)
test(x, y, nk=5, degree=2, niter=niter)
test(x, y, nk=5, degree=3, niter=niter)
spacer()

test(x, y, nk=21, degree=1, niter=niter)
test(x, y, nk=21, degree=2, niter=niter)
test(x, y, nk=21, degree=3, niter=niter)
spacer()

test(x, y, nk=51, degree=1, niter=niter)
test(x, y, nk=51, degree=2, niter=niter)
test(x, y, nk=51, degree=3, niter=niter)
cat("\n")

robotarm <- robotarm(n=1000, p=20)
x <- robotarm$x
y <- robotarm$y
niter <- 10 # robot arm is slow (especially with Force.weights) so only do 10 iters
printf("\n==== robot arm %d x %d ========\n\n", nrow(x), ncol(x))
print.header()

test(x, y, nk=5, degree=1, niter=niter)
test(x, y, nk=5, degree=2, niter=niter)
test(x, y, nk=5, degree=3, niter=niter)
spacer()

test(x, y, nk=21, degree=1, niter=niter)
test(x, y, nk=21, degree=2, niter=niter)
test(x, y, nk=21, degree=3, niter=niter)
spacer()

test(x, y, nk=51, degree=1, niter=niter)
test(x, y, nk=51, degree=2, niter=niter)
test(x, y, nk=51, degree=3, niter=niter)
cat("\n")
