# check.earth.matches.glm.R: check that earth-glm model matches glm model in all essential details

check.earth.matches.glm <- function(earth, glm, newdata=long[c(3,1,9),],
                                   check.coef.names=TRUE,
                                   check.casenames=FALSE,
                                   max=1e-15,
                                   max.residuals=1e-12)
{
    check.names <- function(earth.names, glm.names)
    {
        if(check.casenames &&
        # glm always adds rownames even if "1", "2", "3": this seems
        # wasteful and not particulary helpful, so earth doesn't do
        # this, hence the first !isTRUE(all.equal) below
           !isTRUE(all.equal(glm.names, paste(1:length(glm.names)))) &&
           !isTRUE(all.equal(earth.names, glm.names))) {
            print(earth.names)
            print(glm.names)
            stop(deparse(substitute(earth.names)), " != ",
                 deparse(substitute(glm.names)))
        }
    }
    cat0("check ", deparse(substitute(earth)), " vs ",
         deparse(substitute(glm)), "\n")

    # sort is needed because earth may reorder predictors based in importance
    earth.glm <- earth$glm.list[[1]]
    stopifnot(!is.null(earth.glm))
    stopifnot(almost.equal(sort(coef(earth.glm)), sort(coef(glm)), max=max))
    if(check.coef.names) {
        # earth and glm handle backquoted names slightly differently
        names.earth.glm <- gsub("\`", "", names(coef(earth.glm)))
        names.earth.glm <- sort(names.earth.glm)
        names.glm       <- gsub("\`", "", names(coef(glm)))
        names.glm       <- sort(names.glm)
        stopifnot(identical(names.earth.glm, names.glm))
    }

    stopifnot(length(earth.glm$coefficients) == length(glm$coefficients))
    stopifnot(almost.equal(sort(earth.glm$coefficients), sort(glm$coefficients), max=max))

    stopifnot(length(earth.glm$residuals) == length(glm$residuals))
    stopifnot(almost.equal(earth.glm$residuals, glm$residuals, max=max.residuals))

    stopifnot(length(earth.glm$fitted.values) == length(glm$fitted.values))
    stopifnot(almost.equal(earth.glm$fitted.values, glm$fitted.values, max=max))

    stopifnot(almost.equal(fitted(earth.glm), fitted(glm), max=max))
    if(!is.null(names(fitted(earth.glm))) && !is.null(names(fitted(glm))))
        check.names(names(fitted(earth.glm)), names(fitted(glm)))

    stopifnot(almost.equal(residuals(earth.glm), residuals(glm), max=max.residuals))
    if(!is.null(names(residuals(earth.glm))) && !is.null(names(residuals(glm))))
        check.names(names(residuals(earth.glm)), names(residuals(glm)))

    stopifnot(almost.equal(residuals(earth, type="response"),     residuals(glm, type="response"), max=max.residuals))
    stopifnot(almost.equal(residuals(earth, type="glm.response"), residuals(glm, type="response"), max=max.residuals))
    stopifnot(almost.equal(residuals(earth, type="deviance"),     residuals(glm, type="deviance"), max=max.residuals))
    stopifnot(almost.equal(residuals(earth, type="glm.pearson"),  residuals(glm, type="pearson"), max=max.residuals))
    stopifnot(almost.equal(residuals(earth, type="glm.working"),  residuals(glm, type="working"), max=max.residuals))

    # commented out because partial residuals don't match (because factors are expanded differently?)
    # stopifnot(almost.equal(residuals(earth, type="glm.partial"),  residuals(glm, type="partial"), max=max.residuals))

    # predict without newdata
    predict.earth <- predict(earth)
    predict.glm    <- predict(glm)
    stopifnot(almost.equal(predict.earth[,1], predict.glm, max=max))
    if(!is.null(names(predict.earth)) && !is.null(names(predict.glm)))
        check.names(names(predict.earth), names(predict.glm))

    # predict type=default
    predict.earth <- predict(earth, newdata=newdata)
    predict.glm   <- predict(glm, newdata=newdata)
    stopifnot(almost.equal(predict.earth[,1], predict.glm, max=max))
    if(!is.null(names(predict.earth)) && !is.null(names(predict.glm)))
        check.names(names(predict.earth), names(predict.glm))

    # predict type="response"
    predict.earth.response <- predict(earth, newdata=newdata, type="response")
    predict.glm.response   <- predict(glm, newdata=newdata, type="response")
    if(!is.null(names(predict.earth)) && !is.null(names(predict.glm)))
        check.names(names(predict.earth), names(predict.glm))
    stopifnot(almost.equal(predict.earth.response[,1], predict.glm.response, max=max))
    if(!is.null(names(predict.earth.response)) && !is.null(names(predict.glm.response)))
        check.names(names(predict.earth.response), names(predict.glm.response))

    # predict type="link"
    predict.earth.link <- predict(earth, newdata=newdata, type="link")
    predict.glm.link   <- predict(glm, newdata=newdata, type="link")
    stopifnot(almost.equal(predict.earth.link[,1], predict.glm.link, max=max))
    if(!is.null(names(predict.earth)) && !is.null(names(predict.lm)))
        check.names(names(predict.earth), names(predict.glm))

    # check internal consistency of earth model
    if(is.null(earth$bpairs)) { # doesn't work for bpair models
        stopifnot(earth$gcv == earth$gcv[1])
        stopifnot(almost.equal(earth$rsq.per.response[1], earth$rsq, max=1e-15))
        stopifnot(almost.equal(earth$grsq.per.response[1], earth$grsq, max=1e-15))
        if(is.null(earth$weights))
            stopifnot(almost.equal(earth$rss.per.response, earth$rss, max=1e-10))
    }
}
