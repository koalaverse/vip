# test.emma.R: regression tests for emma (which uses earth internally)
# Stephen Milborrow, Shrewsbury Nov 2014

source("test.prolog.R")
print(R.version.string)
library(emma)
print(citation("emma"))

in.name <- c("x1","x2")
nlev <- c(10, 10)
lower <- c(-2.048, -2.048)
upper <- c(2.048, 2.048)
out.name <- "y"
weight <- 1
C <- 3
pr.mut <- c(0.1, 0.07, 0.04, rep(0.01, C-3))

emma(in.name, nlev, lower, upper, out.name, opt = "mn", nd = 8, na = 5,
     weight, C , w1 = 0.7, w2 = 0.4, c1i = 2.5, c1f = 0.5, c2i = 0.5,
     c2f = 2.5, b = 5, pr.mut, graph = "yes", fn1 = ackley)

source("test.epilog.R")
