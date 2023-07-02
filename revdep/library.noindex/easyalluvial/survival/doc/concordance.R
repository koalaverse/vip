### R code from vignette source 'concordance.Rnw'

###################################################
### code chunk number 1: concordance.Rnw:23-32
###################################################
options(continue="  ", width=60)
options(SweaveHooks=list(fig=function() par(mar=c(4.1, 4.1, .3, 1.1))))
pdf.options(pointsize=10) #text in graph about the same as regular text
options(contrasts=c("contr.treatment", "contr.poly")) #ensure default

#require("survival")
#library(survival)
library(survival)
library(splines)


###################################################
### code chunk number 2: examples1
###################################################
# direct
concordance(y2 ~ x1, data= anscombe)

# logistic regression using Fisher's iris data
fit1 <- glm(Species=="versicolor" ~ ., family=binomial, data=iris) 
concordance(fit1)  # equivalent to an AUC

# linear regression using the Anscombe data
fit2 <- lm(y2 ~ x1 + x4, data= anscombe)
concordance(fit2)  # (R = .89)

# parametric survival 
fit3 <- survreg(Surv(time, status) ~ karno + age + trt, data=veteran)
concordance(fit3)

# 3 Cox models
fit4 <- coxph(Surv(time, status) ~ karno + age + trt, data=veteran)
fit5 <- update(fit4, . ~ . + celltype)
fit6 <- update(fit5, . ~ . + prior)
ctest <- concordance(fit4, fit5, fit6)
ctest


###################################################
### code chunk number 3: balance
###################################################
getOption("SweaveHooks")[["fig"]]()
# The balance figure for the concondance document
btree <- function(n) {
   tfun <- function(n, id, power) {
       if (n==1) id
       else if (n==2) c(2*id, id)
       else if (n==3) c(2*id, id, 2*id+1)
       else {
           nleft <- if (n== power*2) power  else min(power-1, n-power/2)
           c(tfun(nleft, 2*id, power/2), id,
             tfun(n-(nleft+1), 2*id +1, power/2))
           }
       }
   tfun(n, 1, 2^(floor(logb(n-1,2))))
   }

temp <- c(1,2,6,8, 9,12,14, 18, 19, 21, 23, 24, 27    )
indx <- btree(13)

xpos <- 1:15
xpos[4:7] <- tapply(xpos[8:15], rep(1:4, each=2), mean)
xpos[2:3] <- tapply(xpos[4:7], rep(1:2, each=2),mean)
xpos[1] <- mean(xpos[2:3])
ypos <- rep(4:1, c(1,2,4,8))

oldpar <- par(mar=c(1,1,1,1))

plot(xpos, ypos, type='n', xaxt='n', yaxt='n', bty='n',
     xlab="", ylab="")
temp2 <-  c(13,7,5,3,3,3,1,1,1,1,1,1,1)
#text(xpos[indx], ypos[indx], paste(temp, " (", temp2[indx], ")",  sep=''))
text(xpos[indx], ypos[indx], as.character(temp))

delta=.1
for (i in 1:6) {
    segments(xpos[i]-delta, ypos[i]-delta,
             xpos[2*i]+delta, ypos[2*i]+delta)
    segments(xpos[i]+delta, ypos[i]-delta,
             xpos[2*i+1]-delta, ypos[2*i+1] +delta)
}
par(oldpar)


###################################################
### code chunk number 4: concordance.Rnw:283-284
###################################################
concordance(Surv(time, status) ~ predict(fit4), data= veteran)


###################################################
### code chunk number 5: veteran2
###################################################
fit4b <- coxph(formula = Surv(time, status) ~ karno + age + trt + 
                   strata(celltype), veteran)
concordance(fit4b)


###################################################
### code chunk number 6: amlexample
###################################################
afit <- survfit(Surv(time, status) ~1, aml, se =FALSE)
summary(afit, times=afit$time[1:6], censor=TRUE)


###################################################
### code chunk number 7: tmwt
###################################################
getOption("SweaveHooks")[["fig"]]()
colonfit <- coxph(Surv(time, status) ~ rx + nodes + extent, data=colon,
                 subset=(etype==2))   # death only
cord1 <- concordance(colonfit, timewt="n",    ranks=TRUE)
cord2 <- concordance(colonfit, timewt="S",    ranks=TRUE)
cord3 <- concordance(colonfit, timewt="S/G",  ranks=TRUE)
cord4 <- concordance(colonfit, timewt="n/G2", ranks=TRUE)
temp <- c("n(t)"= coef(cord1), S=coef(cord2), "S/G"= coef(cord3), 
          "n/G2"= coef(cord4))
round(temp,5)
matplot(cord1$ranks$time/365.25, cbind(cord1$ranks$timewt,
                                       cord2$ranks$timewt,
                                       cord3$ranks$timewt),
        type= "l", 
        xlab="Years since enrollment", ylab="Weight")
legend(1, 3000, c("n(t)", "nS(t-)", "nS(t-)/G(t-)"), lwd=2,
       col=1:4, lty=1:4, bty="n")


###################################################
### code chunk number 8: manycurve
###################################################
getOption("SweaveHooks")[["fig"]]()
duo <- function(time, status, name, conf.int=FALSE) {
    sfit <- survfit(Surv(time, status) ~1)
    gfit <- survfit(Surv(time, max(status)-status) ~1)
    plot(sfit, conf.int=conf.int, xlab=name, lwd=2)
    lines(gfit, col=2, lwd=2, conf.int = conf.int)
}
oldpar <- par(mfrow=c(3,3), mar=c(5,5,1,1))
with(subset(colon, etype==1), duo(time/365.25, status, "NCCTG colon cancer"))
duo(flchain$futime/365.25, flchain$death, "Free light chain")
duo(kidney$time/12, kidney$status, "McGilchrist kidney")
duo(lung$time/365.25, lung$status, "advanced lung cancer")
duo(mgus2$futime/12, mgus2$death, "MGUS")
duo(nafld1$futime/365.25, nafld1$status, "NAFLD")

duo(pbc$time/365.25, pmin(pbc$status,1), "PBC")
with(rotterdam, duo(pmin(rtime, dtime)/365.25, pmax(recur, death), "Rotterdam"))

nfit <- coxph(Surv(futime/365.25, status) ~ age + male, nafld1)
znfit <- cox.zph(nfit, transform='identity')
plot(znfit[1], resid=FALSE)
par(oldpar)


###################################################
### code chunk number 9: nafld1
###################################################
nfit <- coxph(Surv(futime/365.25, status) ~ age + male, nafld1)
ncord1 <- concordance(nfit)
ncord2 <- concordance(nfit, timewt="S")
ncord4 <- concordance(nfit, timewt="n/G2")
temp <- c(n= coef(ncord1), S=coef(ncord2), "n/G2"= coef(ncord4))
round(temp,6)


###################################################
### code chunk number 10: rankresid (eval = FALSE)
###################################################
## # pick a data set with a smaller number of points, and non PH
## vfit <- coxph(Surv(time/365.25, status) ~ age + karno, veteran)
## temp <- concordance(vfit, ranks=TRUE)$rank
## # Two outliers at 999 days = 2.7 years stretch the axis too far
## plot(rank ~ time, data=temp, xlim=c(0,1.6),
##      xlab="Years", ylab="rank residual")
## lines(lowess(temp$time, temp$rank, iter=1), lwd=2, col=2)
## abline(0, 0, lty=3)


###################################################
### code chunk number 11: rankresid2
###################################################
getOption("SweaveHooks")[["fig"]]()
# pick a data set with a smaller number of points, and non PH
vfit <- coxph(Surv(time/365.25, status) ~ age + karno, veteran)
temp <- concordance(vfit, ranks=TRUE)$rank
# Two outliers at 999 days = 2.7 years stretch the axis too far
plot(rank ~ time, data=temp, xlim=c(0,1.6),
     xlab="Years", ylab="rank residual")
lines(lowess(temp$time, temp$rank, iter=1), lwd=2, col=2)
abline(0, 0, lty=3)


###################################################
### code chunk number 12: rotterdam
###################################################
getOption("SweaveHooks")[["fig"]]()
# should this be included?
# recurrence free survival = earlier of recurrence and death
rdata <- rotterdam
rdata$rfs <-     with(rdata, ifelse(recur==1, 1, death))
rdata$rfstime <- with(rdata, ifelse(recur==1, rtime, dtime))/ 365.25

rfit <- coxph(Surv(rfstime, rfs) ~ age + meno + grade + pspline(nodes), rdata)
ctemp <- matrix(0, 100, 2)  # concordance and std err
ctime <- seq(.1, 10, length=100)
for (i in 1:100) { 
    temp <- concordance(rfit, ymax=ctime[i])
    ctemp[i,] <- c(temp$concordance, sqrt(temp$var))
}
yhat <- ctemp[,1] + outer(ctemp[,2], c(0, -1.96, 1.96), '*')
matplot(ctime, yhat, type='l', lty=c(1,2,2), lwd=c(2,1,1), col=1, 
        xlab="Upper cutoff", ylab="C", ylim=c(0.5,1))


###################################################
### code chunk number 13: test
###################################################
ctest <- concordance(fit4, fit5, fit6)
ctest

# compare concordance values of fit4 and fit5
contr <- c(-1, 1, 0)
dtest <- contr %*% coef(ctest)
dvar  <- contr %*% vcov(ctest) %*% contr

c(contrast=dtest, sd=sqrt(dvar), z=dtest/sqrt(dvar))


###################################################
### code chunk number 14: Cztrans
###################################################
zci <- function(fit, p=.95) {
    ilogist   <- function(p) log(p/(1-p))         # inverse logistic
    logistic  <- function(x) exp(x)/(1 + exp(x))  
    temp <- concordance(fit, influence =1)
    cminus <- temp$concordance - temp$dfbeta # values of concordance, without i

    newd    <- ilogist(temp$concordance) - ilogist(cminus) # dfbeta on new scale
    new.sd  <- sqrt(sum(newd^2))
    old.sd  <- sqrt(sum(temp$dfbeta^2))  # same as sqrt(temp$var)
    
    z <-  qnorm((1-p)/2)
    old.ci  <- temp$concordance + c(z, -z)*old.sd
    new.ci  <- logistic(ilogist(temp$concordance) + c(z, -z)* new.sd)
    rbind(old = old.ci, new= new.ci)
}

round(zci(colonfit), 4)


###################################################
### code chunk number 15: close
###################################################
set.seed(1953)
ytest <- matrix(rexp(20), ncol=2) %*% chol(matrix(c(1, .98, .98, 1), 2))
cor(ytest)
lfit <- lm(ytest[,1] ~ ytest[,2])
zci(lfit)


