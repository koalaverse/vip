## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
mets <- lava:::versioncheck('mets', 1)

## -----------------------------------------------------------------------------
library('lava')
m0 <- lvm(y1+y2 ~ x, y1 ~~ y2)
edgelabels(m0, y1 + y2 ~ x) <- c(expression(beta[1]), expression(beta[2]))
edgelabels(m0, y1 ~ y2) <- expression(rho)
plot(m0, layoutType="circo")

## ----load, results="hide",message=FALSE,warning=FALSE-------------------------
library('lava')

## ----m0-----------------------------------------------------------------------
m0 <- lvm() |>
  covariance(y1 ~ y2, value='r') |>
  regression(y1 + y2 ~ x)

## ----coef---------------------------------------------------------------------
coef(m0, labels=TRUE)

## ----sim----------------------------------------------------------------------
d <- sim(m0, 500, p=c(r=0.9), seed=1)
head(d)

## ----defcens------------------------------------------------------------------
cens1 <- function(threshold,type='right') {
  function(x) {
    x <- unlist(x)
    if (type=='left')
      return( survival::Surv(pmax(x,threshold), x>=threshold, type='left') )
      return ( survival::Surv(pmin(x,threshold), x<=threshold) )
  }
}

m0 <- 
  transform(m0, s1 ~ y1, cens1(-2, 'left')) |>
  transform(s2 ~ y2, cens1(2,  'right'))

## ----sim2---------------------------------------------------------------------
d <- sim(m0, 500, p=c(r=0.9), seed=1)
head(d)

## ----est1---------------------------------------------------------------------
m <- lvm() |>
     regression(y1 + y2 ~ x) |>
     covariance(y1 ~ y2)

e <- estimate(m, data=d)
e

## ----delta--------------------------------------------------------------------
estimate(e, function(p) p['y1~~y2']/(p['y1~~y1']*p['y2~~y2'])^.5)

## ----correlation--------------------------------------------------------------
correlation(e)

## -----------------------------------------------------------------------------
estimate(e, function(p) atanh(p['y1~~y2']/(p['y1~~y1']*p['y2~~y2'])^.5), back.transform=tanh)

## ----constraints--------------------------------------------------------------
m2 <- m |>
    parameter(~ l1 + l2 + z) |>
    variance(~ y1 + y2, value=c('v1','v2')) |>
    covariance(y1 ~ y2, value='c') |>
    constrain(v1 ~ l1, fun=exp) |>
    constrain(v2 ~ l2, fun=exp) |>
    constrain(c ~ z+l1+l2, fun=function(x) tanh(x[1])*sqrt(exp(x[2])*exp(x[3])))

## ----estconstraints-----------------------------------------------------------
e2 <- estimate(m2, d)
e2

## ----deltaconstraints---------------------------------------------------------
estimate(e2, 'z', back.transform=tanh)

## ----constraints2-------------------------------------------------------------
m2 <- lvm() |>
  regression(y1 + y2 ~ x) |>
  covariance(y1 ~ y2, constrain=TRUE, rname='z')

e2 <- estimate(m2, data=d)
e2

## ----e2backtransform----------------------------------------------------------
estimate(e2, 'z', back.transform=tanh)

## ----profileci, cache=TRUE----------------------------------------------------
tanh(confint(e2, 'z', profile=TRUE))

## ----bootstrap, cache=TRUE----------------------------------------------------
set.seed(1)
b <- bootstrap(e2, data=d, R=50, mc.cores=1)
b

## ----cache=TRUE---------------------------------------------------------------
quantile(tanh(b$coef[,'z']), c(.025,.975))

## ----cache=TRUE, eval=mets----------------------------------------------------
m3 <- lvm() |>
  regression(y1 + s2 ~ x) |>
  covariance(y1 ~ s2, constrain=TRUE, rname='z')

e3 <- estimate(m3, d)

## ----eval=mets----------------------------------------------------------------
e3

## ----cache=TRUE, eval=mets----------------------------------------------------
estimate(e3, 'z', back.transform=tanh)

## ----cache=TRUE, eval=mets----------------------------------------------------
m3b <- lvm() |>
  regression(s1 + s2 ~ x) |>
  covariance(s1 ~ s2, constrain=TRUE, rname='z')

e3b <- estimate(m3b, d)
e3b

## ----eval=mets----------------------------------------------------------------
e3b

## ----cache=TRUE, eval=mets----------------------------------------------------
estimate(e3b, 'z', back.transform=tanh)

## ----profilecens, cache=TRUE, eval=mets---------------------------------------
tanh(confint(e3b, 'z', profile=TRUE))

## -----------------------------------------------------------------------------
sessionInfo()

