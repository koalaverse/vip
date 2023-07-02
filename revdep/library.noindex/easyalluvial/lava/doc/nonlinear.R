## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
mets <- lava:::versioncheck('mets', 1)
fullVignette <- Sys.getenv("_R_FULL_VIGNETTE_") %in% c("1","TRUE")

## ----load, results="hide",message=FALSE,warning=FALSE-------------------------
library('lava')

## ----sim----------------------------------------------------------------------
f <- function(x) cos(1.25*x) + x - 0.25*x^2
m <- lvm(x1+x2+x3 ~ eta1, y1+y2+y3 ~ eta2, latent=~eta1+eta2)
regression(m) <- eta1+eta2 ~ z
functional(m, eta2~eta1) <- f

d <- sim(m, n=200, seed=42) # Default is all parameters are 1

## -----------------------------------------------------------------------------
plot(m, plot.engine="visNetwork")

## ----specifymodels------------------------------------------------------------
m1 <- lvm(x1+x2+x3 ~ eta1, eta1 ~ z, latent=~eta1)
m2 <- lvm(y1+y2+y3 ~ eta2, eta2 ~ z, latent=~eta2)

## -----------------------------------------------------------------------------
nonlinear(m2, type="quadratic") <- eta2 ~ eta1

## ----twostage1----------------------------------------------------------------
e1 <- twostage(m1, m2, data=d)
e1

## ----linear_mle---------------------------------------------------------------
e0 <- estimate(regression(m1%++%m2, eta2~eta1), d)
estimate(e0,keep="^eta2~[a-z]",regex=TRUE) ## Extract coef. matching reg.ex.

## ----pred1--------------------------------------------------------------------
newd <- expand.grid(eta1=seq(-4, 4, by=0.1), z=0)
pred1 <- predict(e1, newdata=newd, x=TRUE)
head(pred1)

## ----spline_twostage----------------------------------------------------------
kn <- seq(-3,3,length.out=5)
nonlinear(m2, type="spline", knots=kn) <- eta2 ~ eta1
e2 <- twostage(m1, m2, data=d)
e2

## ----spline_ci----------------------------------------------------------------
p <- cbind(eta1=newd$eta1,
	  estimate(e2,f=function(p) predict(e2,p=p,newdata=newd))$coefmat)
head(p)

## ----fig:pred2----------------------------------------------------------------
plot(I(eta2-z) ~ eta1, data=d, col=Col("black",0.5), pch=16,
     xlab=expression(eta[1]), ylab=expression(eta[2]), xlim=c(-4,4))
lines(Estimate ~ eta1, data=as.data.frame(p), col="darkblue", lwd=5)
confband(p[,1], lower=p[,4], upper=p[,5], polygon=TRUE,
	 border=NA, col=Col("darkblue",0.2))

## ----spline_several-----------------------------------------------------------
m2a <- nonlinear(m2, type="linear", eta2~eta1)
m2b <- nonlinear(m2, type="quadratic", eta2~eta1)
kn1 <- seq(-3,3,length.out=5)
kn2 <- seq(-3,3,length.out=8)
m2c <- nonlinear(m2, type="spline", knots=kn1, eta2~eta1)
m2d <- nonlinear(m2, type="spline", knots=kn2, eta2~eta1)

## ----cv_fit, cache=TRUE, eval=fullVignette------------------------------------
#  ## Scale models in stage 2 to allow for a fair RMSE comparison
#  d0 <- d
#  for (i in endogenous(m2))
#      d0[,i] <- scale(d0[,i],center=TRUE,scale=TRUE)
#  ## Repeated 5-fold cross-validation:
#  ff <- lapply(list(linear=m2a,quadratic=m2b,spline4=m2c,spline6=m2d),
#  	    function(m) function(data,...) twostage(m1,m,data=data,stderr=FALSE,control=list(start=coef(e0),contrain=TRUE)))
#  fit.cv <- cv(ff,data=d,K=5,rep=2,mc.cores=parallel::detectCores(),seed=1)

## ----results="hide", echo=FALSE-----------------------------------------------
## To save time building the vignettes on CRAN, we cache time consuming computations
if (fullVignette) {
  fit.cv$fit <- NULL
  saveRDS(fit.cv, "data/nonlinear_fitcv.rds")
} else {
  fit.cv <- readRDS("data/nonlinear_fitcv.rds")
}

## -----------------------------------------------------------------------------
summary(fit.cv)

## ----multifit-----------------------------------------------------------------
fit <- lapply(list(m2a,m2b,m2c,m2d),
	     function(x) {
		 e <- twostage(m1,x,data=d)
		 pr <- cbind(eta1=newd$eta1,predict(e,newdata=newd$eta1,x=TRUE))
		 return(list(estimate=e,predict=as.data.frame(pr)))
	     })

plot(I(eta2-z) ~ eta1, data=d, col=Col("black",0.5), pch=16,
     xlab=expression(eta[1]), ylab=expression(eta[2]), xlim=c(-4,4))
col <- c("orange","darkred","darkgreen","darkblue")
lty <- c(3,4,1,5)
for (i in seq_along(fit)) {
    with(fit[[i]]$pr, lines(eta2 ~ eta1, col=col[i], lwd=4, lty=lty[i]))
}
legend("bottomright",
      c("linear","quadratic","spline(df=4)","spline(df=6)"),
      col=col, lty=lty, lwd=3)

## ----twostageCV, cache=TRUE, eval=fullVignette--------------------------------
#  selmod <- twostageCV(m1, m2, data=d, df=2:4, nmix=1:2,
#  	    nfolds=2, rep=1, mc.cores=parallel::detectCores())

## ----results="hide", echo=FALSE-----------------------------------------------
## To save time building the vignettes on CRAN, we cache time consuming computations
if (fullVignette) {
  saveRDS(summary(selmod), "data/nonlinear_selmod.rds")
} else {
  selmod <- readRDS("data/nonlinear_selmod.rds")
}

## -----------------------------------------------------------------------------
selmod

## -----------------------------------------------------------------------------
d$g <- (d$z<0)*1 ## Group variable
mm1 <- regression(m1, ~g)  # Add grouping variable as exogenous variable (effect specified via 'predict.fun')
mm2 <- regression(m2, eta2~ u1+u2+u1:g+u2:g+z)
pred <- function(mu,var,data,...) {
    cbind("u1"=mu[,1],"u2"=mu[,1]^2+var[1],
	  "u1:g"=mu[,1]*data[,"g"],"u2:g"=(mu[,1]^2+var[1])*data[,"g"])
}
ee1 <- twostage(mm1, model2=mm2, data=d, predict.fun=pred)
estimate(ee1,keep="eta2~u",regex=TRUE)

## -----------------------------------------------------------------------------
summary(estimate(ee1,keep="(:g)", regex=TRUE))

## -----------------------------------------------------------------------------
m1 <- baptize(m1)  ## Label all parameters
intercept(m1, ~x1+eta1) <- list(0,NA) ## Set intercept of x1 to zero. Remove the label of eta1
regression(m1,x1~eta1) <- 1 ## Factor loading fixed to 1

## ----mixture1, cache=TRUE, eval=fullVignette----------------------------------
#  set.seed(1)
#  em0 <- mixture(m1, k=2, data=d)

## ----estmixture, cache=TRUE,warnings=FALSE,messages=FALSE,eval=FALSE----------
#  em0 <- NULL
#  ll <- c()
#  for (i in 1:5) {
#      set.seed(i)
#      em <- mixture(m1, k=2, data=d, control=list(trace=0))
#      ll <- c(ll,logLik(em))
#      if (is.null(em0) || logLik(em0)<tail(ll,1))
#  	em0 <- em
#  }

## ----results="hide", echo=FALSE-----------------------------------------------
## To save time building the vignettes on CRAN, we cache time consuming computations
if (fullVignette) {
  saveRDS(em0, "data/nonlinear_em0.rds")
} else {
  em0 <- readRDS("data/nonlinear_em0.rds")
}

## -----------------------------------------------------------------------------
summary(em0)

## ----eval=mets----------------------------------------------------------------
e0 <- estimate(m1,data=d)
AIC(e0,em0)

## ----eval=mets----------------------------------------------------------------
em2 <- twostage(em0,m2,data=d)
em2

## ----mixturefit, eval=mets----------------------------------------------------
plot(I(eta2-z) ~ eta1, data=d, col=Col("black",0.5), pch=16,
     xlab=expression(eta[1]), ylab=expression(eta[2]))

lines(Estimate ~ eta1, data=as.data.frame(p), col="darkblue", lwd=5)
confband(p[,1], lower=p[,4], upper=p[,5], polygon=TRUE,
	 border=NA, col=Col("darkblue",0.2))

pm <- cbind(eta1=newd$eta1,
	    estimate(em2, f=function(p) predict(e2,p=p,newdata=newd))$coefmat)
lines(Estimate ~ eta1, data=as.data.frame(pm), col="darkred", lwd=5)
confband(pm[,1], lower=pm[,4], upper=pm[,5], polygon=TRUE,
	 border=NA, col=Col("darkred",0.2))
legend("bottomright", c("Gaussian","Mixture"),
       col=c("darkblue","darkred"), lwd=2, bty="n")

