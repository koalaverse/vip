# check.models.equal.R

almost.equal <- function(x, y, max=1e-8, msg="")
{
    stopifnot(max >= 0 && max < .1)
    almost.equal <- length(x) == length(y) && max(abs(x - y)) < max
    if(!almost.equal) {
        xname <- deparse(substitute(x))
        yname <- deparse(substitute(y))
        printf("\n---%s-------------------------------------------------------\n", msg)
        printf("not almost.equal(%s, %s, max=%g)\n\n", xname, yname, max)
        printf("%s:\n", xname)
        print(x)
        printf("\n%s:\n", yname)
        print(y)
        printf("----------------------------------------------------------\n\n")
    }
    almost.equal
}
check.almost.equal <- function(x, y, max=1e-8, msg="", verbose=FALSE)
{
    if(any(y == 0))
        diff <- x - y
    else
        diff <- (x - y) / y
    if(any(abs(diff) > max)) {
        cat(msg, "\n1st matrix:\n", sep="")
        print(x)
        cat("2nd matrix:\n")
        print(y)
        cat("diff:\n")
        print(diff)
        stop("check.almost.equal failed for ", msg, call.=FALSE)
    }
    if(verbose)
        cat(msg, "OK\n")
}

check.same <- function(mod1, mod2, msg="", allow.different.names=FALSE, max=0)
{
    if(!identical(mod1, mod2)) {
        stop.with.msg = TRUE
        if(!is.null(dim(mod1)) && !is.null(dim(mod2))) {
            # check if it is the column names
            mod1a <- mod1
            mod2a <- mod2
            colnames(mod1a) <- NULL
            colnames(mod2a) <- NULL
            if(identical(mod1a, mod2a)) {
                cat("mod1", msg, "\n")
                print(colnames(mod1))
                cat("mod2", msg, "\n")
                print(colnames(mod2))
                cat("\n")
                if(allow.different.names) {
                    warning(msg, " has different column names but is otherwise identical, see above messages\n", call.=FALSE)
                    stop.with.msg = FALSE
                } else
                    stop(msg, " has different column names but is otherwise identical, see above messages", call.=FALSE)
            }
            # check if it is the row names
            mod1a <- mod1
            mod2a <- mod2
            rownames(mod1a) <- NULL
            rownames(mod2a) <- NULL
            if(identical(mod1a, mod2a)) {
                cat("\nm1", msg, "\n"); print(head(mod1)); cat("\nm2", msg, "\n"); print(head(mod2)); cat("\n")
                if(allow.different.names) {
                    warning(msg, " has different row names but is otherwise identical, see above messages\n", call.=FALSE)
                    stop.with.msg = FALSE
                } else
                    stop(msg, " has different row names but is otherwise identical, see above messages", call.=FALSE)
            }
        }
        if(max != 0) {
            same <- almost.equal(mod1, mod2, max=max, msg=msg)
            stop.with.msg = FALSE
        }
        if(stop.with.msg) {
            cat("\nm1", msg, "\n"); print(mod1);
            cat("\nm2", msg, "\n"); print(mod2);
            cat("\ndifference mod1-mod2", msg, "\n"); try(print(mod1-mod2));
            cat("\n")
            stop(msg, " don't match, see above messages (max=", max, ")", call.=FALSE)
        }
    }
}

check.models.equal <- function(mod1, mod2, msg="",
                               check.subsets=TRUE, allow.different.names=FALSE,
                               newdata=NULL)
{
    mod1$call <- NULL
    mod2$call <- NULL
    mod1$trace <- NULL
    mod2$trace <- NULL
    msg.colon <- if(nchar(msg) != 0) paste0(msg, ": ") else ""
    if(identical(mod1, mod2))
        cat(msg.colon, "models identical\n", sep="")
    else {
        cat(msg.colon, "models not identical\n\n", sep="");
        # cat("mod1\n"); print(summary(mod1)); cat("mod2\n"); print(summary(mod2)); cat("\n")
        # TODO why do we need a nonzero max here and below?
        check.same(mod1$bx, mod2$bx, "bx", allow.different.names=allow.different.names, max=1e-14)
        check.same(mod1$coefficients, mod2$coefficients, "coefficients", allow.different.names=allow.different.names, max=1e-14)
        check.same(mod1$dirs, mod2$dirs, "dirs", allow.different.names=allow.different.names)
        check.same(mod1$cuts, mod2$cuts, "cuts", allow.different.names=allow.different.names)
        check.same(mod1$residuals, mod2$residuals, "residuals", max=1e-14, allow.different.names=allow.different.names)
        check.same(mod1$selected.terms, mod2$selected.terms, "selected.terms", allow.different.names=allow.different.names)
        if(check.subsets) {
            # leaps and xtx pruning can give different prune.terms, so skip test
            check.same(mod1$prune.terms, mod2$prune.terms, "prune.terms")
            check.same(mod1$rss.per.response, mod2$rss.per.response, "rss.per.response", max=1e-14)
            check.same(mod1$rsq.per.response, mod2$rsq.per.response, "rsq.per.response")
            check.same(mod1$gcv.per.response, mod2$gcv.per.response, "gcv.per.response")
            check.same(mod1$grsq.per.response, mod2$grsq.per.response, "grsq.per.response")
            check.same(mod1$rss.per.subset, mod2$rss.per.subset, "rss.per.subset", max=1e-14)
            check.same(mod1$gcv.per.subset, mod2$gcv.per.subset, "gcv.per.subset", max=1e-14)
        }
        check.same(predict(mod1),                  predict(mod2),                  "predict with no newdata, default type", allow.different.names=allow.different.names)
        check.same(predict(mod1, type="link"),     predict(mod2, type="link"),     "predict with no newdata, type=\"link\"", allow.different.names=allow.different.names)
        check.same(predict(mod1, type="response"), predict(mod2, type="response"), "predict with no newdata, type=\"response\"", allow.different.names=allow.different.names)
        check.same(predict(mod1, type="earth"),    predict(mod2, type="earth"),    "predict with no newdata, type=\"earth\"", allow.different.names=allow.different.names)
        if(!is.null(newdata)) {
            check.same(predict(mod1, newdata), predict(mod2, newdata), "predict with newdata, default type", allow.different.names=allow.different.names)
            check.same(predict(mod1, newdata, type="link"), predict(mod2, newdata, type="link"), "predict with newdata, , type=\"link\"", allow.different.names=allow.different.names)
            check.same(predict(mod1, newdata, type="response"), predict(mod2, newdata, type="response"), "predict with newdata, , type=\"response\"", allow.different.names=allow.different.names)
            check.same(predict(mod1, newdata, type="earth"), predict(mod2, newdata, type="earth"), "predict with newdata, , type=\"earth\"", allow.different.names=allow.different.names)
        }
        if(!almost.equal(mod1$rss, mod2$rss, msg=msg))
            stop(msg.colon, "different rss")
        if(!almost.equal(mod1$rsq, mod2$rsq, msg=msg))
            stop(msg.colon, "different rsq")
        if(mod1$rsq != mod2$rsq)
            cat("mod1$rsq ", mod1$rsq, " != mod2$rsq ", mod2$rsq, " (although almost equal)\n", sep="")
        if(!almost.equal(mod1$gcv, mod2$gcv, msg=msg))
            stop(msg.colon, "different gcv")
        if(!almost.equal(mod1$grsq, mod2$grsq, msg=msg))
            stop(msg.colon, "different grsq")
        form1 <- try(formula(mod1), silent=TRUE)
        form2 <- try(formula(mod2), silent=TRUE)
        if(!identical(form1, form2))
            cat("Formulas differ: ", gsub("\n", "", format(form1)),
                "\nand:             ",
                 gsub("\n", "", format(form2)), "\n\n", sep="")
        else if(!identical(terms(mod1), terms(mod2)))
            cat("terms(mod1) != terms(mod2)\n", sep="")
        glm1 <- mod1$glm.list[[1]]
        glm2 <- mod2$glm.list[[1]]
        if(is.null(glm1) && !is.null(glm2))
            cat(msg.colon, "mod2 has a GLM submodel but mod1 does not\n")
        else if(!is.null(glm1) && is.null(glm2))
            cat(msg.colon, "mod1 has a GLM submodel but mod2 does not\n")
        else if(!is.null(glm1) && !is.null(glm2)) {
            if(identical(glm1, glm2))
                cat(msg.colon, "glm submodels identical\n", sep="")
            else {
                if(!almost.equal(glm1$coefficients, glm2$coefficients, msg=msg))
                    stop(msg.colon, "different coefficients")
                if(!almost.equal(residuals(glm1), residuals(glm2), msg=msg))
                    stop(msg.colon, "different residuals")
                # I have the seen the following with a quasibinomial model with a zero weight
                else if(!almost.equal(glm1$residuals, glm2$residuals, msg=msg))
                    warning("residuals(glm1) == residuals(glm2) but glm1$residuals != glm2$residuals\n\n")
                if(!almost.equal(glm1$fitted.values, glm2$fitted.values, msg=msg))
                    stop(msg.colon, "different fitted.values")
                # cat("summary(glm.glm1, details=TRUE):\n")
                # print(summary(glm1, details=TRUE))
                # cat("summary(glm.glm2, details=TRUE):\n")
                # print(summary(glm2, details=TRUE))
                form1 <- try(formula(glm1), silent=TRUE)
                form2 <- try(formula(glm2), silent=TRUE)
                form1s <- paste0(gsub("\n", "", format(form1)), collapse="")
                form2s <- paste0(gsub("\n", "", format(form2)), collapse="")
                if(identical(form1s, form2s))
                    printf("%sglm submodel formula strings are identical: %s\n", msg.colon, form1s)
                if(!identical(form1, form2)) {
                    if(identical(form1s, form2s))
                        cat(msg.colon, "but the actual glm submodel formulas differ ",
                        "(classes are \"", class(form1)[1], "\" and \"", class(form2)[1], "\")\n", sep="")
                    else
                        cat(msg.colon, "glm submodel formulas differ: ", form1s,
                            "\nand:                          ",  form2s,
                            "(classes are \"", class(form1)[1], "\" and \"", class(form2)[1], ")\"\n",
                             sep="")
                } else if(!identical(terms(glm1), terms(glm2)))
                    cat(msg.colon, "terms(glm1) != terms(glm2)\n", sep="")
                cat(msg.colon, "glm submodels not identical (but coefs, residuals, fitted.values are the same)\n", sep="")
            }
        }
        cat(msg.colon, "Models are equivalent, within numerical tolerances\n", sep="")
    }
    cat("\n")
}
