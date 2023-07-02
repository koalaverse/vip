### R code from vignette source 'Design-issues.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(width=75)
library(Matrix)


###################################################
### code chunk number 2: dgC-ex
###################################################
getClass("dgCMatrix")


###################################################
### code chunk number 3: dgC-ex
###################################################
getClass("ntTMatrix")


###################################################
### code chunk number 4: diag-class
###################################################
(D4 <- Diagonal(4, 10*(1:4)))
str(D4)
diag(D4)


###################################################
### code chunk number 5: diag-2
###################################################
diag(D4) <- diag(D4) + 1:4
D4


###################################################
### code chunk number 6: unit-diag
###################################################
str(I3 <- Diagonal(3)) ## empty 'x' slot

getClass("diagonalMatrix") ## extending "sparseMatrix"


###################################################
### code chunk number 7: Matrix-ex
###################################################
(M <- spMatrix(4,4, i=1:4, j=c(3:1,4), x=c(4,1,4,8))) # dgTMatrix
m <- as(M, "matrix")
(M. <- Matrix(m)) # dsCMatrix (i.e. *symmetric*)


###################################################
### code chunk number 8: sessionInfo
###################################################
toLatex(sessionInfo())


