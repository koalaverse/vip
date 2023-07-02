# test.plotd.R

source("test.prolog.R")
library(earth)
data(etitanic)

printh <- function(caption)
    cat("===", caption, "\n", sep="")

multifigure <- function(caption)
{
    printh(caption)
    par(mfrow=c(2, 2))
    par(cex = 0.7)
    par(mar = c(4, 3, 1.7, 0.5))    # small margins and text to pack figs in
    par(mgp = c(1.6, 0.6, 0))       # flatten axis elements
    oma <- par("oma") # make space for caption
    oma[3] <- 2.4
    par(oma=oma)
}
do.caption <- function(caption)
    mtext(caption, outer=TRUE, font=2, line=1, cex=1)

# test plotd basic functionality on a numeric response

multifigure("a1") # start a new page
a1 <- earth(survived ~ ., data=etitanic, degree=2, glm=list(family=binomial))

plotd(a1)
do.caption("a1")

plotd(a1, main="earth-glm, numeric, kernel=epan adjust=.3", trace=TRUE,
      kernel="epan", adjust=.3, legend.names=c("mylegend", "mylegend2"),
      legend.pos=c(.3,4), cex.legend=1, legend.extra=TRUE,
      col=c(1, "green"), fill="red")

plotd(a1, main="earth-glm, numeric, type=earth, params",
      type="earth", xlab="my xlab", ylab="my ylab",
      legend.pos="topleft", xlim=c(-.5, 1.5), zero.line=TRUE, vline.col="green",
      col=c("pink", "red"), fill="pink")

plotd(a1, main="earth-glm, numeric, type=link, params", type="link", legend=FALSE,
      col=c("red", "blue"), lty=c(1,2),
      vline.thresh=1, vline.col="gray", vline.lty=2)

# test with earth.default (as opposed to earth.formula)
multifigure("a2") # start a new page
a2 <- earth(etitanic[,-2], etitanic$survived, degree=2, glm=list(family=binomial))
plotd(a2, main="earth.default-glm, numeric response")
do.caption("a2")

printh("a3")
a3 <- earth(etitanic[,-1], etitanic$pclass, degree=2, glm=list(family=binomial))
plotd(a3, main="earth.default-glm, 3 lev fac")

# test plotd with histograms

plotd(a3, main="earth-glm, 3 lev fac, hist", hist=TRUE)

plotd(a3, main="earth-glm, 3 lev fac, hist, params",
      hist=TRUE, col=c("green", "red", "black"), fill="pink", lty=c(1,3),
      xlab="my xlab", ylab="my ylab", xlim=c(-.2, 1.2),
      vline.thresh=.65, vline.col="brown", vline.lty=2, breaks=5)

# xlim and ylim tests

multifigure("xlim and ylim tests") # start a new page
plotd(a1, xlim=c(.25,.75), fill="gray")
printh("xlim and ylim tests")
plotd(a1, xlim=c(.25,1), ylim=c(0,2))
plotd(a1, xlim=c(.25,.75), hist=TRUE)
plotd(a1, xlim=c(.25,1), ylim=c(0,100), hist=TRUE)

# test plotd with a logical response

multifigure("a5")
bool.survived <- as.logical(etitanic$survived)
a5 <- earth(bool.survived ~ . - survived, data=etitanic, degree=2, glm=list(family=binomial))
plotd(a5, main="earth-glm, logical")
do.caption("a5")
plotd(a5, main="earth-glm, logical, hist", hist=TRUE)

# test plotd with a two level factor

multifigure("a6")
a6 <- earth(sex ~ ., data=etitanic, glm=list(family=binomial))
plotd(a6, main="earth-glm, 2 lev fac", fill="gray70")
do.caption("a6")
plotd(a6, main="earth-glm, 2 lev fac, type=class", type="class", fill="gray70")
plotd(a6, main="earth-glm, 2 lev fac, hist ", hist=TRUE)
plotd(a6, main="earth-glm, 2 lev fac, hist, type=class", type="class", hist=TRUE, labels=TRUE)

# test plotd with a 3 level factor

multifigure("a7")
a7 <- earth(pclass ~ ., data=etitanic, glm=list(family=binomial))

plotd(a7, main="earth-glm, 3 lev fac",
      col=c("pink", "red", "brown"), fill="pink")
do.caption("a7")

plotd(a7, main="earth-glm, 3 lev fac, params",
      xlab="my xlab", ylab="my ylab", xlim=c(-.2, 1.2),
      col=c("pink", "black", "green"), lty=c(1,3,1),
      vline.thresh=.2, vline.col="blue", vline.lty=3,
      adjust=.3)

plotd(a7, main="earth-glm, 3 lev fac, hist", hist=TRUE)

plotd(a7, main="earth-glm, 3 lev fac, hist, params",
      hist=TRUE, col=c("pink", "red", "black"), fill=c("pink"), lty=c(1,2,3),
      xlab="my xlab", ylab="my ylab", xlim=c(-.2, 1.2),
      vline.thresh=.65, vline.col="gray", vline.lty=1,
      breaks=5)

multifigure("a7 part 2")
plotd(a7, type="class", main="earth-glm, 3 lev fac, type=class", fill="gray70")
do.caption("a7 part 2")
plotd(a7, type="class", main="earth-glm, 3 lev fac, hist, type=class", hist=TRUE, labels=TRUE)

# test nresponse

multifigure("a7 with nresponse")
plotd(a7, main="earth.default-glm, 3 lev fac")
do.caption("a7  with nresponse")
plotd(a7, main="earth.default-glm, 3 lev fac, nresp=1", nresp=1)
plotd(a7, main="earth.default-glm, 3 lev fac, nresp=2", nresp=2)
#plotd(a7, main="earth.default-glm, 3 lev fac, nresp=c(1,2)", nresp=c(1,2))

# test plotd with earth not glm

multifigure("a8")
a8 <- earth(survived ~ ., data=etitanic, degree=2)
plotd(a8, main="earth, numeric, no glm arg")
do.caption("a8")
plotd(a8, main="earth, hist, num, no glm arg, type=class", hist=TRUE, type="class")

printh("a9")
a9 <- earth(survived - .5 ~ .-survived, data=etitanic, degree=2)
plotd(a9, main="earth, survived-.5, type=class, thresh=0", hist=TRUE, type="class",thresh=0,vline.col="brown",xaxis.cex=.8, fill="pink",breaks=4,labels=TRUE)
plotd(a9, main="earth, survived-.5, type=class, thresh=0.3", hist=TRUE, type="class",thresh=0.3,vline.col="brown", xaxis.cex=.7,breaks=3,labels=TRUE)

multifigure("a10")
bool.survived <- as.logical(etitanic$survived)
a10 <- earth(bool.survived ~ . - survived, data=etitanic, degree=2)
plotd(a10, main="earth, logical, no glm arg")
do.caption("a10")

printh("a11")
a11 <- earth(sex ~ ., data=etitanic, degree=2)
plotd(a11, main="earth, 2 lev fac, no glm arg")

printh("a12")
a12 <- earth(pclass ~ ., data=etitanic, degree=2)
plotd(a12, main="earth, 3 lev fac, no glm arg")

# test that we can change the order of the levels and still get the same results
multifigure("compare pclass with different factor levels")
printh("fit.pclass")
fit.pclass <- earth(pclass ~ ., data=etitanic, degree=2)
plotd(fit.pclass, type="class", hist=1, main="fit.pclass", fill=0,
           col=c(2, 1, "lightblue"))
do.caption("left and right graphs should match, up to level order")
printh("fit.pclass.reorder")
tit <- etitanic
pclass <- as.character(tit$pclass)
pclass[pclass == "1st"] <- "first"
pclass[pclass == "2nd"] <- "class2"
pclass[pclass == "3rd"] <- "classthird"
tit$pclass <- factor(pclass, levels=c("class2", "classthird", "first"))
fit.pclass.reorder <- earth(pclass ~ ., data=tit, degree=2)
plotd(fit.pclass.reorder, type="class", hist=1, main="fit.pclass.reorder",
      col=c(1, "lightblue", 2), fill=0, legend.pos="topright")

# examples from the man page

printh("example(plotd)")
example(plotd)
do.caption("example(plotd)")

multifigure("glm.model example from man page")
library(earth); data(etitanic)
glm.model <- glm(sex ~ ., data=etitanic, family=binomial)
plotd(glm.model)
do.caption("glm.model example from man page")

printh("lm.model example from man page")
library(earth); data(etitanic)
lm.model <- lm(as.numeric(sex) ~ ., data=etitanic)
plotd(lm.model, trace=2)
plot(1,1) # empty.plot
plot(1,1)

# test with rpart (also test nresponse with a character value)
printh("rpart")
library(rpart); library(earth); data(etitanic)
rpart.model <- rpart(sex ~ ., data = etitanic, method="class")
plotd(rpart.model, type="prob", nresponse="female")
plotd(rpart.model, type="prob", nresponse="ma")
plotd(rpart.model, type="class", hist=TRUE, labels=TRUE)
plotd(rpart.model, hist=TRUE, labels=TRUE) # default type is "vector"

printh("lda.model examples from man pages")
library(MASS); library(earth); data(etitanic)
lda.model <- lda(sex ~ ., data=etitanic)
plotd(lda.model, type="response")
plotd(lda.model, hist=TRUE, labels=TRUE)

library(MASS); library(earth); set.seed(420)
example(lda)
plotd(z, type="response", nresponse=1) # nresponse=1 selects first linear discriminant
do.caption("lda.model example from example(lda)")

a.qda <- qda(survived ~ ., data=etitanic)
plotd(a.qda)
plotd(a.qda, type="post")

# test plotd with lm models

multifigure("lm1")
lm1 <- lm(survived ~ ., data=etitanic)
plotd(lm1)
do.caption("lm1")
plotd(lm1, main="lm1, survived")
plotd(lm1, hist=TRUE, main="lm1, survived, hist=TRUE, labels=1", labels=1)

printh("lm2")
bool.survived <- as.logical(etitanic$survived)
lm2 <- lm(bool.survived ~ . - survived, data=etitanic)
plotd(lm2, main="lm, logical")

# following commented out because lm doesn't like factor responses(?)
# printh("lm3")
# lm3 <- lm(sex ~ ., data=etitanic)
# plotd(lm3, main="lm, 2 lev fac")
#
# printh("lm4")
# lm4 <- lm(pclass ~ ., data=etitanic)
# plotd(lm4, main="lm, 3 lev fac")

multifigure("lm5")
lm5 <- lm(age - mean(age)~ ., data=etitanic)
plotd(lm5, main="lm5, age - mean(age)")
do.caption("lm5")

printh("lm6")
lm6 <- lm(unclass(pclass)-1 ~ ., data=etitanic)
plotd(lm6, main="lm6, unclass(pclass)-1")
plotd(lm6, main="lm6, unclass(pclass)-1, fac=TRUE", hist=TRUE)

printh("lm7")
lm7 <- lm(cbind(survived, sin(age)) ~ ., data=etitanic) # nonsense model
plotd(lm7, xlim=c(-.5,1.5), hist=TRUE, main="lm7, NCOL(y)==2")

multifigure("lm5")
lm8 <- lm(cbind(survived, sin(age), cos(age)) ~ ., data=etitanic) # nonsense model
plotd(lm8, hist=TRUE, main="lm8, NCOL(y)==3")
do.caption("lm8")

# test plotd with glm models

multifigure("glm1")
glm1 <- glm(survived ~ ., data=etitanic, family=binomial)
plotd(glm1, main="glm1, survived")
do.caption("glm1")

printh("glm2")
glm2 <- glm(pclass ~ ., data=etitanic, family=binomial)
plotd(glm2, main="glm2, pclass")

printh("glm3")
glm3 <- glm(sex ~ ., data=etitanic, family=binomial)
plotd(glm3, main="glm3, sex")

multifigure("glm, 3 level factor with dichot")
glm4 <- glm(pclass ~ ., data=etitanic, family=binomial)
plotd(glm4, dichot = TRUE, type="link")
do.caption("glm, 3 level factor with dichot")
plotd(glm4, dichot = FALSE, type="link")
plotd(glm4, dichot = TRUE) # default type="response"
plotd(glm4, dichot = FALSE, type=NULL) # default type="response"

# lda with formula interface

library(MASS)
multifigure("lda1")
lda1 <- lda(sex ~ ., data=etitanic)
plotd(lda1, main="lda1, 2 lev fac", trace=1)
do.caption("lda1")
plotd(lda1, main="lda1, 2 lev fac, hist=TRUE", type="response", hist=TRUE)
plotd(lda1, main="lda1, 2 lev fac, hist=TRUE, type=post", hist=TRUE, type="post")
plotd(lda1, main="lda1, 2 lev fac, hist=TRUE, type=class", hist=TRUE, type="class", labels=TRUE)

multifigure("lda2")
lda2 <- lda(pclass ~ ., data=etitanic)
plotd(lda2, type="response", main="lda2, 3 lev fac, nresponse=1", jitter=TRUE, nresponse=1)
do.caption("lda2")
plotd(lda2, type="response", main="lda2, 3 lev fac, nresponse=1", jitter=TRUE, nresponse=1)
plotd(lda2, type="response", main="lda2, 3 lev fac, nresponse=2", jitter=TRUE, nresponse=2)
# plotd(lda2, main="lda2, 3 lev fac, nresponse=NULL", jitter=TRUE, nresponse=NULL)

multifigure("lda2 part 2")
# plotd(lda2, type="response", main="lda2, 3 lev fac, hist=TRUE", hist=TRUE)
plotd(lda2, main="lda2, 3 lev fac, hist=TRUE, type=p, nresponse=1", hist=TRUE, type="p", nresponse=1)
do.caption("lda2 part 2")
plotd(lda2, main="lda2, 3 lev fac, type=p", type="p")
plotd(lda2, main="lda2, 3 lev fac, hist=TRUE, type=class, nresponse=1", hist=TRUE, type="class", nresponse=1)

multifigure("lda2 with dichot")
plotd(lda2, main="lda2, 3 lev fac, type=p, nresponse=1", hist=TRUE, type="p", nresponse=1)
do.caption("lda2 with dichot")
plotd(lda2, main="lda2, 3 lev fac, dichot=1, type=p, nresponse=1", hist=TRUE, type="p", nresponse=1, dichot=TRUE)
plotd(lda2, main="lda2, 3 lev fac, type=p, nresponse=1", type="p", nresponse=1)
plotd(lda2, main="lda2, 3 lev fac, dichot=1, type=p, nresponse=1", type="p", nresponse=1, dichot=TRUE)

multifigure("lda3")
lda3 <- lda(survived ~ ., data=etitanic)
plotd(lda3, type="response", main="lda3, logical")
do.caption("lda3")
plotd(lda3, type="response", main="lda3, logical, hist=TRUE", hist=TRUE)
plotd(lda3, main="lda3, logical, hist=TRUE, type=posterior", hist=TRUE, type="posterior")
plotd(lda3, main="lda3, logical, hist=TRUE, type=class", hist=TRUE, type="class", labels=TRUE)

# lda with default interface

# predict.lda (called by plotd) can't deal with factors in x argument
etitanic1 <- etitanic
etitanic1[,1] <- as.numeric(etitanic1[,1]) # pclass
etitanic1[,3] <- as.numeric(etitanic1[,3]) # sex

multifigure("ldad1")
ldad1 <- lda(etitanic1[,-3], etitanic$sex)
plotd(ldad1, type="response", main="ldad1, 2 lev fac")
do.caption("ldad1")
plotd(ldad1, type="response", main="ldad1, 2 lev fac, hist=TRUE", hist=TRUE)
plotd(ldad1, main="ldad1, 2 lev fac, hist=TRUE, type=post", hist=TRUE, type="post")
plotd(ldad1, main="ldad1, 2 lev fac, hist=TRUE, type=class", hist=TRUE, type="class")

multifigure("ldad2")
ldad2 <- lda(etitanic1[,-1], etitanic$pclass)
# plotd(ldad2, type="response", main="ldad2, 3 lev fac", jitter=TRUE)
plotd(ldad2, type="response", main="ldad2, 3 lev fac, nresponse=1", jitter=TRUE, nresponse=1)
do.caption("ldad2")
plotd(ldad2, type="response", main="ldad2, 3 lev fac, nresponse=2", jitter=TRUE, nresponse=2)
multifigure("ldad2 part 2")
plotd(ldad2, type="response", main="ldad2, 3 lev fac, hist=TRUE, nresponse=1", hist=TRUE, nresponse=1)
do.caption("ldad2 part 2")
plotd(ldad2, main="ldad2, 3 lev fac, hist=TRUE, type=p, nresponse=1", hist=TRUE, type="p", nresponse=1)
plotd(ldad2, main="ldad2, 3 lev fac, type=p, nresponse=1", type="po", nresponse=1)
plotd(ldad2, main="ldad2, 3 lev fac, hist=TRUE, type=class, nresponse=1", hist=TRUE, type="cla", nresponse=1)

multifigure("ldad3")
ldad3 <- lda(etitanic1[,-2], etitanic$survived)
plotd(ldad3, type="response", main="ldad3, logical")
do.caption("ldad3")
plotd(ldad3, type="response", main="ldad3, logical, hist=TRUE", hist=TRUE)
plotd(ldad3, main="ldad3, logical, hist=TRUE, type=post", hist=TRUE, type="post")
plotd(ldad3, main="ldad3, logical, hist=TRUE, type=cl", hist=TRUE, type="cl")

# err shading

multifigure("err shading")
a.shade <- earth(survived ~ ., data=etitanic, degree=2, glm=list(family=binomial))
plotd(a.shade, vline.col="gray", err.col=c("slategray1","slategray3"), fill=0)
do.caption("err shading")
plotd(a.shade, vline.col="gray", err.col=c(0, 0,"pink"), fill=0, vline.thresh = .6, err.border=c(0,0,2))
# try various err.shade options
plotd(a.shade, vline.thresh = .7, vline.col=1, vline.lty=2, vline.lwd=3, fill=0, col=c(2,1),
      err.col=c("slategray1","slategray3","pink"),
      err.border=c(3,4,5), err.lwd=c(1,2,3))
# reverse direction of reducible error area
a1.shade <- earth(!survived ~ ., data=etitanic, degree=2, glm=list(family=binomial))
plotd(a1.shade, vline.col="gray", err.col=c("slategray1","slategray3","pink"), err.border=c("slategray1","slategray3","red"))

# clip xlim into the shaded area and make sure area is still shaded correctly
multifigure("err shading with xlim")
a.shade <- earth(survived ~ ., data=etitanic, degree=2, glm=list(family=binomial))
plotd(a.shade, vline.thresh = .7, vline.col=1, vline.lty=2, vline.lwd=3, fill=0, col=c(2,1),
      err.col=c("slategray1","slategray3","pink"),
      err.border=c(3,4,5), err.lwd=c(1,2,3), xlim=c(.3,1))
plotd(a.shade, vline.thresh = .7, vline.col=1, vline.lty=2, vline.lwd=3, fill=0, col=c(2,1),
      err.col=c("slategray1","slategray3","pink"),
      err.border=c(3,4,5), err.lwd=c(1,2,3), xlim=c(.5,1))
plotd(a.shade, vline.thresh = .7, vline.col=1, vline.lty=2, vline.lwd=3, fill=0, col=c(2,1),
      err.col=c("slategray1","slategray3","pink"),
      err.border=c(3,4,5), err.lwd=c(1,2,3), xlim=c(.3,.6))
# reverse direction of reducible error area
a1.shade <- earth(!survived ~ ., data=etitanic, degree=2, glm=list(family=binomial))
plotd(a1.shade, vline.col="gray", err.col=c("slategray1","slategray3","pink"),
      err.border=c("slategray1","slategray3","red"), xlim=c(.52, .9))

par(org.par)

source("test.epilog.R")
