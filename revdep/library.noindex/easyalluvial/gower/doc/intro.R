### R code from vignette source 'intro.Rnw'

###################################################
### code chunk number 1: intro.Rnw:34-35
###################################################
options(prompt="    ")


###################################################
### code chunk number 2: intro.Rnw:68-72
###################################################
library(gower)
dat1 <- iris[1:10,]
dat2 <- iris[6:15,]
gower_dist(dat1, dat2)


###################################################
### code chunk number 3: intro.Rnw:77-78
###################################################
gower_dist(iris[1,], dat1)


###################################################
### code chunk number 4: intro.Rnw:87-92
###################################################
dat1 <- dat2 <- iris[1:10,]
names(dat2) <- tolower(names(dat2))
gower_dist(dat1, dat2)
# tell gower_dist to match columns 1..5 in dat1 with column 1..5 in dat2
gower_dist(dat1, dat2, pair_y=1:5)


###################################################
### code chunk number 5: intro.Rnw:97-98
###################################################
gower_dist(dat1, dat2, ignore_case=TRUE)


###################################################
### code chunk number 6: intro.Rnw:106-109
###################################################
dat1 <- iris[1:10,]
L <- gower_topn(x=dat1, y=iris, n=3)
L


###################################################
### code chunk number 7: intro.Rnw:125-127
###################################################
gower_dist(women[1,], women)
gower_dist(women[1,], women, weights=c(2,3))


