#' Model-specific variable importance
#'
#' Compute model-specific variable importance scores for the predictors in a
#' model. (This function is meant for internal use only.)
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param type Character string specifying the type of variable importance to
#' return (only used for some models). See details for which methods this
#' argument applies to.
#'
#' @param ... Additional optional arguments.
#'
#' @return A tidy data frame (i.e., a \code{"tibble"} object) with two columns:
#' \code{Variable} and \code{Importance}. For \code{"lm"/"glm"}-like object, an
#' additional column, called \code{Sign}, is also included which includes the
#' sign (i.e., POS/NEG) of the original coefficient.
#'
#' @details
#'
#' Computes model-specific variable importance scores depending on the class of
#' \code{object}:
#'
#' \describe{
#'
#' \item{\code{\link[C50]{C5.0}}}{Variable importance is measured by determining
#' the percentage of training set samples that fall into all the terminal nodes
#' after the split. For example, the predictor in the first split automatically
#' has an importance measurement of 100 percent since all samples are affected
#' by this split. Other predictors may be used frequently in splits, but if the
#' terminal nodes cover only a handful of training set samples, the importance
#' scores may be close to zero. The same strategy is applied to rule-based
#' models and boosted versions of the model. The underlying function can also
#' return the number of times each predictor was involved in a split by using
#' the option \code{metric = "usage"}. See \code{\link[C50]{C5imp}} for
#' details.}
#'
#' \item{\code{\link[Cubist]{cubist}}}{The Cubist output contains variable usage
#' statistics. It gives the percentage of times where each variable was used in
#' a condition and/or a linear model. Note that this output will probably be
#' inconsistent with the rules shown in the output from summary.cubist. At each
#' split of the tree, Cubist saves a linear model (after feature selection) that
#' is allowed to have terms for each variable used in the current split or any
#' split above it. Quinlan (1992) discusses a smoothing algorithm where each
#' model prediction is a linear combination of the parent and child model along
#' the tree. As such, the final prediction is a function of all the linear
#' models from the initial node to the terminal node. The percentages shown in
#' the Cubist output reflects all the models involved in prediction (as opposed
#' to the terminal models shown in the output). The variable importance used
#' here is a linear combination of the usage in the rule conditions and the
#' model. See \code{\link[Cubist]{summary.cubist}} and
#' \code{\link[caret]{varImp.cubist}} for details.}
#'
#' \item{\code{\link[glmnet]{glmnet}}}{Similar to (generalized) linear models,
#' the absolute value of the coefficients are returned for a specific model.
#' It is important that the features  (and hence, the estimated coefficients) be
#' standardized prior to fitting the model. You can specify which coefficients
#' to return by passing the specific value of the penalty parameter via the
#' \code{...} argument. See \code{\link[glmnet]{coef.glmnet}} for details. By
#' default, the coefficients corresponding to the final penalty value in the
#' sequence is returned; in other words, you should ALWAYS SPECIFY THIS VALUE!
#' For \code{"cv.glmnet"} objects, the largest value of lambda such that error
#' is within one standard error of the minimum is used by default. For
#' \code{"multnet"} objects, the coefficients corresponding to the first class
#' are used; that is, the fist component of \code{\link[glmnet]{coef.glmnet}}.}
#'
#' \item{\code{\link[partykit]{cforest}}}{Variable importance is measured in a
#' way similar to those computed by \code{\link[randomForest]{importance}}.
#' Besides the standard version, a conditional version is available that
#' adjusts for correlations between predictor variables. If
#' \code{conditional = TRUE}, the importance of each variable is computed by
#' permuting within a grid defined by the predictors that are associated (with
#' 1 - \emph{p}-value greater than threshold) to the variable of interest. The
#' resulting variable importance score is conditional in the sense of beta
#' coefficients in regression models, but represents the effect of a variable in
#' both main effects and interactions. See Strobl et al. (2008) for details.
#' Note, however, that all random forest results are subject to random
#' variation. Thus, before interpreting the importance ranking, check whether
#' the same ranking is achieved with a different random seed - or otherwise
#' increase the number of trees ntree in \code{\link[partykit]{ctree_control}}.
#' Note that in the presence of missings in the predictor variables the
#' procedure described in Hapfelmeier et al. (2012) is performed. See
#' \code{\link[partykit]{varimp}} for details.}
#'
#' \item{\code{\link[earth]{earth}}}{The \code{\link[earth]{earth}} package uses
#' three criteria for estimating the variable importance in a MARS model (see
#' \code{\link[earth]{evimp}} for details):
#' \itemize{
#'   \item The \code{nsubsets} criterion (\code{type = "nsubsets"}) counts the
#'   number of model subsets that include each feature. Variables that are
#'   included in more subsets are considered more important. This is the
#'   criterion used by \code{\link[earth]{summary.earth}} to print variable
#'   importance. By "subsets" we mean the subsets of terms generated by
#'   \code{earth()}'s backward pass. There is one subset for each model size
#'   (from one to the size of the selected model) and the subset is the best set
#'   of terms for that model size. (These subsets are specified in the
#'   \code{$prune.terms} component of \code{earth()}'s return value.) Only
#'   subsets that are smaller than or equal in size to the final model are used
#'   for estimating variable importance. This is the default method used by
#'   \strong{vip}.
#'
#'   \item The \code{rss} criterion (\code{type = "rss"}) first calculates the
#'   decrease in the RSS for each subset relative to the previous subset during
#'   \code{earth()}’s backward pass. (For multiple response models, RSS's are
#'   calculated over all responses.) Then for each variable it sums these
#'   decreases over all subsets that include the variable. Finally, for ease of
#'   interpretation the summed decreases are scaled so the largest summed
#'   decrease is 100. Variables which cause larger net decreases in the RSS are
#'   considered more important.
#'
#'   \item The \code{gcv} criterion (\code{type = "gcv"}) is similar to the
#'   \code{rss} approach, but uses the GCV statistic instead of the RSS. Note
#'   that adding a variable can sometimes increase the GCV. (Adding the variable
#'   has a deleterious effect on the model, as measured in terms of its
#'   estimated predictive power on unseen data.) If that happens often enough,
#'   the variable can have a negative total importance, and thus appear less
#'   important than unused variables.
#' }}
#'
#' \item{\code{\link[gbm]{gbm}}}{Variable importance is computed using one of
#' two approaches (See \code{\link[gbm]{summary.gbm}} for details):
#' \itemize{
#'
#'   \item The standard approach (\code{type = "relative.influence"}) described
#'   in Friedman (2001). When \code{distribution = "gaussian"} this returns the
#'   reduction of squared error attributable to each variable. For other loss
#'   functions this returns the reduction attributable to each variable in sum
#'   of squared error in predicting the gradient on each iteration. It describes
#'   the \emph{relative influence} of each variable in reducing the loss
#'   function. This is the default method used by \strong{vip}.
#'
#'   \item An experimental permutation-based approach
#'   (\code{type = "permutation"}). This method randomly permutes each predictor
#'   variable at a time and computes the associated reduction in predictive
#'   performance. This is similar to the variable importance measures Leo
#'   Breiman uses for random forests, but \strong{gbm} currently computes using
#'   the entire training dataset (not the out-of-bag observations).
#'
#' }}
#'
#' \item{\code{\link[h2o]{H2OModel}}}{See \code{\link[h2o]{h2o.varimp}} or visit
#' \url{http://docs.h2o.ai/h2o/latest-stable/h2o-docs/variable-importance.html}
#' for details.}
#'
#' \item{\code{\link[nnet]{nnet}}}{Two popular methods for constructing variable
#' importance scores with neural networks are the Garson algorithm
#' (Garson 1991), later modified by Goh (1995), and the Olden algorithm
#' (Olden et al. 2004). For both algorithms, the basis of these importance
#' scores is the network’s connection weights. The Garson algorithm determines
#' variable importance by identifying all weighted connections between the nodes
#' of interest. Olden’s algorithm, on the other hand, uses the product of the
#' raw connection weights between each input and output neuron and sums the
#' product across all hidden neurons. This has been shown to outperform the
#' Garson method in various simulations. For DNNs, a similar method due to
#' Gedeon (1997) considers the weights connecting the input features to the
#' first two hidden layers (for simplicity and speed); but this method can be
#' slow for large networks.. To implement the Olden and Garson algorithms, use
#' \code{type = "olden"} and \code{type = "garson"}, respectively. See
#' \code{\link[NeuralNetTools]{garson}} and \code{\link[NeuralNetTools]{olden}}
#' for details.}
#'
#' \item{\code{\link[stats]{lm}}}{In (generalized) linear models, variable
#' importance is typically based on the absolute value of the corresponding
#' \emph{t}-statistics. For such models, the sign of the original coefficient
#' is also returned. By default, \code{type = "stat"} is used; however, if the
#' inputs have been appropriately standardized then the raw coefficients can be
#' used with \code{type = "raw"}.}
#'
#' \item{\code{\link[sparklyr]{ml_feature_importances}}}{The Spark ML
#' library provides standard variable importance for tree-based methods (e.g.,
#' random forests). See \code{\link[sparklyr]{ml_feature_importances}} for
#' details.}
#'
#' \item{\code{\link[randomForest]{randomForest}}}{Random forests typically
#' provide two measures of variable importance. The first measure is computed
#' from permuting out-of-bag (OOB) data: for each tree, the prediction error on
#' the OOB portion of the data is recorded (error rate for classification and
#' MSE for regression). Then the same is done after permuting each predictor
#' variable. The difference between the two are then averaged over all trees in
#' the forest, and normalized by the standard deviation of the differences. If
#' the standard deviation of the differences is equal to 0 for a variable,
#' the division is not done (but the average is almost always equal to 0 in that
#' case).
#'
#' The second measure is the total decrease in node impurities from splitting on
#' the variable, averaged over all trees. For classification, the node impurity
#' is measured by the Gini index. For regression, it is measured by residual sum
#' of squares. See \code{\link[randomForest]{importance}} for details.}
#'
#' \item{\code{\link[party]{cforest}}}{Same approach described in
#' \code{\link[partykit]{cforest}} above. See \code{\link[party]{varimp}} and
#' \code{\link[party]{varimpAUC}} (if \code{type = "auc"}) for details.}
#'
#' \item{\code{\link[ranger]{ranger}}}{Variable importance for
#' \code{\link[ranger]{ranger}} objects is computed in the usual way for random
#' forests. The approach used depends on the \code{importance} argument provided
#' in the initial call to \code{\link[ranger]{ranger}}. See
#' \code{\link[ranger]{importance}} for details.}
#'
#' \item{\code{\link[rpart]{rpart}}}{As stated in one of the \strong{rpart}
#' vignettes. A variable may appear in the tree many times, either as a primary
#' or a surrogate variable. An overall measure of variable importance is the sum
#' of the goodness of split measures for each split for which it was the primary
#' variable, plus "goodness" * (adjusted agreement) for all splits in which it
#' was a surrogate. Imagine two variables which were essentially duplicates of
#' each other; if we did not count surrogates, they would split the importance
#' with neither showing up as strongly as it should. See
#' \code{\link[rpart]{rpart}} for details.}
#'
#' \item{\code{\link[caret]{train}}}{Various model-specific and model-agnostic
#' approaches that depend on the learning algorithm employed in the original
#' call to \code{\link[caret]{train}}. See \code{\link[caret]{varImp}} for
#' details.}
#'
#' \item{\code{\link[xgboost]{xgboost}}}{For linear models, the variable
#' importance is the absolute magnitude of the estimated coefficients. For that
#' reason, in order to obtain a meaningful ranking by importance for a linear
#' model, the features need to be on the same scale (which you also would want
#' to do when using either L1 or L2 regularization). Otherwise, the approach
#' described in Friedman (2001) for \code{\link[gbm]{gbm}}s is used. See
#' \code{\link[xgboost]{xgb.importance}} for details. If \code{type = NULL} (the
#' default), \code{"Gain"} is used. See \code{\link[xgboost]{xgb.importance}}
#' for details.}
#'
#' }
#'
#' @note Inspired by the \code{\link[caret]{varImp}} function.
#'
#' @rdname vi_model
#'
#' @export
vi_model <- function(object, ...) {
  UseMethod("vi_model")
}


#' @rdname vi_model
#'
#' @export
vi_model.default <- function(object, ...) {
  stop("Model-specific variable importance scores are currently not available ",
       "for this type of model.", call. = FALSE)
}


# Package: C50 -----------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.C5.0 <- function(object, type = c("usage", "splits"), ...) {

  # # Check for dependency
  # if (!requireNamespace("C50", quietly = TRUE)) {
  #   stop("Package \"C50\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Determine which type of variable importance to compute
  type <- match.arg(type)

  # Consruct model-specific variable importance scores
  vis <- C50::C5imp(object, metric = type, ...)
  tib <- tibble::tibble(
    "Variable" = rownames(vis),
    "Importance" = vis$Overall
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- type

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: caret ---------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.train <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("caret", quietly = TRUE)) {
  #   stop("Package \"caret\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- caret::varImp(object, ...)
  if (inherits(vis, "varImp.train")) {
    vis <- vis$importance
  }
  tib <- tibble::tibble(
    "Variable" = rownames(vis),
    "Importance" = vis$Overall
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- "caret"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: Cubist --------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.cubist <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("Cubist", quietly = TRUE)) {
  #   stop("Package \"Cubist\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Consruct model-specific variable importance scores
  vis <- caret::varImp(object, ...)
  tib <- tibble::tibble(
    "Variable" = rownames(vis),
    "Importance" = vis$Overall
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- "usage"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: earth ---------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.earth <- function(object, type = c("nsubsets", "rss", "gcv"), ...) {

  # # Check for dependency
  # if (!requireNamespace("earth", quietly = TRUE)) {
  #   stop("Package \"earth\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Determine which type of variable importance to compute
  type <- match.arg(type)

  # Construct model-specific variable importance scores
  vis <- earth::evimp(object, trim = FALSE, ...)[, type, drop = TRUE]
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )
  tib$Variable <- gsub("-unused$", replacement = "", x = tib$Variable)

  # Add variable importance type attribute
  attr(tib, which = "type") <- type

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: gbm -----------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.gbm <- function(object, type = c("relative.influence", "permutation"),
                         ...) {

  # # Check for dependency
  # if (!requireNamespace("gbm", quietly = TRUE)) {
  #   stop("Package \"gbm\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Determine which type of variable importance to compute
  type <- match.arg(type)

  # Construct model-specific variable importance scores
  vis <- if (type == "relative.influence") {
    gbm::summary.gbm(object, plotit = FALSE, order = TRUE,
                     method = gbm::relative.influence, ...)
  } else {
    gbm::summary.gbm(object, plotit = FALSE, order = TRUE,
                     method = gbm::permutation.test.gbm, ...)
  }
  tib <- tibble::tibble(
    "Variable" = vis$var,
    "Importance" = vis$rel.inf
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- type

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: glmnet --------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.glmnet <- function(object, ...) {

  # Extract coefficients
  s <- list(...)$s
  if (is.null(s)) {
    s <- min(object$lambda)
  }
  coefs <- stats::coef(object, s = s)
  if (inherits(coefs, what = "list")) {  # "multnet" objects
    coefs <- coefs[[1L]]
  }
  coefs <- coefs[, 1L, drop = TRUE]

  # Remove intercept (if it's there)
  if ("(Intercept)" %in% names(coefs)) {
    coefs <- coefs[setdiff(x = names(coefs), y = "(Intercept)")]
  }

  # Construct model-specific variable importance scores
  tib <- tibble::tibble(
    "Variable" = names(coefs),
    "Importance" = coefs,
    "Sign" = ifelse(sign(coefs) == 1, yes = "POS", no = "NEG")
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- "|coefficient|"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.cv.glmnet <- function(object, ...) {

  # Extract coefficients
  s <- list(...)$s
  if (is.null(s)) {
    s <- "lambda.1se"
  }
  coefs <- stats::coef(object, s = s)
  if (inherits(coefs, what = "list")) {  # "multnet" objects
    coefs <- coefs[[1L]]
  }
  coefs <- coefs[, 1L, drop = TRUE]

  # Remove intercept (if it's there)
  if ("(Intercept)" %in% names(coefs)) {
    coefs <- coefs[setdiff(x = names(coefs), y = "(Intercept)")]
  }

  # Construct model-specific variable importance scores
  tib <- tibble::tibble(
    "Variable" = names(coefs),
    "Importance" = coefs,
    "Sign" = ifelse(sign(coefs) == 1, yes = "POS", no = "NEG")
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- "|coefficient|"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: h2o -----------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.H2OBinomialModel <- function(object, ...) {

  # Construct model-specific variable importance scores
  tib <- tibble::as.tibble(h2o::h2o.varimp(object))
  if (object@algorithm == "glm") {
    names(tib) <- c("Variable", "Importance", "Sign")
    # FIXME: Extra row at the bottom?
  } else {
    tib <- tib[1L:2L]
    names(tib) <- c("Variable", "Importance")
  }

  # Add variable importance type attribute
  attr(tib, which = "type") <- "h2o"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.H2OMultinomialModel <- function(object, ...) {

  # Construct model-specific variable importance scores
  tib <- tibble::as.tibble(h2o::h2o.varimp(object))
  if (object@algorithm == "glm") {
    names(tib) <- c("Variable", "Importance", "Sign")
    # FIXME: Extra row at the bottom?
  } else {
    tib <- tib[1L:2L]
    names(tib) <- c("Variable", "Importance")
  }

  # Add variable importance type attribute
  attr(tib, which = "type") <- "h2o"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.H2ORegressionModel <- function(object, ...) {

  # Construct model-specific variable importance scores
  tib <- tibble::as.tibble(h2o::h2o.varimp(object))
  if (object@algorithm == "glm") {
    names(tib) <- c("Variable", "Importance", "Sign")
    # FIXME: Extra row at the bottom?
  } else {
    tib <- tib[1L:2L]
    names(tib) <- c("Variable", "Importance")
  }

  # Add variable importance type attribute
  attr(tib, which = "type") <- "h2o"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: neuralnet -----------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.nn <- function(object, type = c("olden", "garson"), ...) {

  # Check for dependency
  if (!requireNamespace("NeuralNetTools", quietly = TRUE)) {
    stop("Package \"NeuralNetTools\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }

  # Determine which type of variable importance to compute
  type <- match.arg(type)

  # Construct model-specific variable importance scores
  tib <- if (type == "olden") {  # Olden's algorithm
    vis <- NeuralNetTools::olden(object, bar_plot = FALSE)
    tibble::tibble(
      "Variable" = rownames(vis),
      "Importance" = vis$importance
    )
  } else {  # Garson's algorithm
    vis <- NeuralNetTools::garson(object, bar_plot = FALSE)
    tibble::tibble(
      "Variable" = rownames(vis),
      "Importance" = vis$rel_imp
    )
  }

  # Add variable importance type attribute
  attr(tib, which = "type") <- type

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: nnet ----------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.nnet <- function(object, type = c("olden", "garson"), ...) {

  # Check for dependency
  if (!requireNamespace("NeuralNetTools", quietly = TRUE)) {
    stop("Package \"NeuralNetTools\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }

  # Determine which type of variable importance to compute
  type <- match.arg(type)

  # Construct model-specific variable importance scores
  tib <- if (type == "olden") {  # Olden's algorithm
    vis <- NeuralNetTools::olden(object, bar_plot = FALSE)
    tibble::tibble(
      "Variable" = rownames(vis),
      "Importance" = vis$importance
    )
  } else {  # Garson's algorithm
    vis <- NeuralNetTools::garson(object, bar_plot = FALSE)
    tibble::tibble(
      "Variable" = rownames(vis),
      "Importance" = vis$rel_imp
    )
  }

  # Add variable importance type attribute
  attr(tib, which = "type") <- type

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: party ---------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.RandomForest <- function(object, type = c("accuracy", "auc"), ...) {

  # # Check for dependency
  # if (!requireNamespace("party", quietly = TRUE)) {
  #   stop("Package \"party\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Determine which type of variable importance to compute
  type <- match.arg(type)

  # Construct model-specific variable importance scores
  vis <- if (type == "auc") {
    # Check for dependency
    if (!requireNamespace("varImp", quietly = TRUE)) {
      stop("Package \"varImp\" needed for this function to work. Please ",
           "install it.", call. = FALSE)
    }
    party::varimpAUC(object, ...)  # rm ... for now
  } else {
    party::varimp(object, ...)  # rm ... for now
  }
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- type

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: partykit ------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.constparty <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("partykit", quietly = TRUE)) {
  #   stop("Package \"partykit\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- partykit::varimp(object, ...)
  features <- attr(stats::terms(object), which = "term.labels")
  unused <- setdiff(features, names(vis))
  unused <- stats::setNames(rep(0, times = length(unused)), nm = unused)
  vis <- c(vis, unused)
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- "permutation"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.cforest <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("partykit", quietly = TRUE)) {
  #   stop("Package \"partykit\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- partykit::varimp(object, ...)
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- "permutation"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: pls -----------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.mvr <- function(object, ...) {
  # FIXME: For now, just default to using caret.
  #
  # Check for dependency
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package \"caret\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }
  vis <- caret::varImp(object, ...)
  tib <- tibble::tibble(
    "Variable" = rownames(vis),
    "Importance" = vis[["Overall"]]
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- "caret"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: randomForest --------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.randomForest <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("randomForest", quietly = TRUE)) {
  #   stop("Package \"randomForest\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- randomForest::importance(object, ...)
  type <- colnames(vis)[1L]
  if (dim(vis)[2L] == 0) {  # possible when importance = FALSE in RF call
    importance_scores <- object$importance
  }
  vis <- vis[, 1L, drop = TRUE]
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- type

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: ranger --------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.ranger <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("ranger", quietly = TRUE)) {
  #   stop("Package \"ranger\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- ranger::importance(object)
  tib <- tibble::tibble(
    "Variable" = names(vis),
    "Importance" = vis
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- object$importance.mode

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: rpart ---------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.rpart <- function(object, ...) {

  # Construct model-specific variable importance scores
  importance_scores <- object$variable.importance
  if (is.null(importance_scores)) {
    stop("Cannot extract variable importance scores from a tree with no ",
         "splits.", call. = FALSE)
  }
  feature_names <- names(importance_scores)

  # Place variable importance scores in a tibble (the first and second columns
  # should always be labelled "Variable" and "Importance", respectively)
  tib <- tibble::tibble(
    "Variable" = feature_names,
    "Importance" = importance_scores
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- "GoodnessOfSplit"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: RSNNS ---------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.mlp <- function(object, type = c("olden", "garson"), ...) {

  # Check for dependency
  if (!requireNamespace("NeuralNetTools", quietly = TRUE)) {
    stop("Package \"NeuralNetTools\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }

  # Determine which type of variable importance to compute
  type <- match.arg(type)

  # Construct model-specific variable importance scores
  tib <- if (type == "olden") {  # Olden's algorithm
    vis <- NeuralNetTools::olden(object, bar_plot = FALSE)
    tibble::tibble(
      "Variable" = rownames(vis),
      "Importance" = vis$importance
    )
  } else {  # Garson's algorithm
    vis <- NeuralNetTools::garson(object, bar_plot = FALSE)
    tibble::tibble(
      "Variable" = rownames(vis),
      "Importance" = vis$rel_imp
    )
  }

  # Add variable importance type attribute
  attr(tib, which = "type") <- type

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: sparklyr ------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.ml_model_decision_tree_regression <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("sparklyr", quietly = TRUE)) {
  #   stop("Package \"sparklyr\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- sparklyr::ml_feature_importances(object, ...)
  names(vis) <- c("Variable", "Importance")
  tib <- tibble::as_tibble(vis)

  # Add variable importance type attribute
  attr(tib, which = "type") <- "impurity"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.ml_model_decision_tree_classification <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("sparklyr", quietly = TRUE)) {
  #   stop("Package \"sparklyr\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- sparklyr::ml_feature_importances(object, ...)
  names(vis) <- c("Variable", "Importance")
  tib <- tibble::as_tibble(vis)

  # Add variable importance type attribute
  attr(tib, which = "type") <- "impurity"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.ml_model_gbt_regression <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("sparklyr", quietly = TRUE)) {
  #   stop("Package \"sparklyr\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- sparklyr::ml_feature_importances(object, ...)
  names(vis) <- c("Variable", "Importance")
  tib <- tibble::as_tibble(vis)

  # Add variable importance type attribute
  attr(tib, which = "type") <- "impurity"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.ml_model_gbt_classification <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("sparklyr", quietly = TRUE)) {
  #   stop("Package \"sparklyr\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- sparklyr::ml_feature_importances(object, ...)
  names(vis) <- c("Variable", "Importance")
  tib <- tibble::as_tibble(vis)

  # Add variable importance type attribute
  attr(tib, which = "type") <- "impurity"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.ml_model_generalized_linear_regression <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("sparklyr", quietly = TRUE)) {
  #   stop("Package \"sparklyr\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- sparklyr::tidy(object, ...)[, c("term", "statistic")]
  if (vis$term[1L] == "(Intercept)") {
    vis <- vis[-1L, ]
  }
  vis$Sign <- ifelse(sign(vis$statistic) == 1, yes = "POS", no = "NEG")
  vis$statistic <- abs(vis$statistic)
  names(vis) <- c("Variable", "Importance", "Sign")
  tib <- tibble::as_tibble(vis)

  # Add variable importance type attribute
  attr(tib, which = "type") <- "|z-statistic|"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.ml_model_linear_regression <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("sparklyr", quietly = TRUE)) {
  #   stop("Package \"sparklyr\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- sparklyr::tidy(object, ...)[, c("term", "statistic")]
  if (vis$term[1L] == "(Intercept)") {
    vis <- vis[-1L, ]
  }
  vis$Sign <- ifelse(sign(vis$statistic) == 1, yes = "POS", no = "NEG")
  vis$statistic <- abs(vis$statistic)
  names(vis) <- c("Variable", "Importance", "Sign")
  tib <- tibble::as_tibble(vis)

  # Add variable importance type attribute
  attr(tib, which = "type") <- "|t-statistic|"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.ml_model_random_forest_regression <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("sparklyr", quietly = TRUE)) {
  #   stop("Package \"sparklyr\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- sparklyr::ml_feature_importances(object, ...)
  names(vis) <- c("Variable", "Importance")
  tib <- tibble::as_tibble(vis)

  # Add variable importance type attribute
  attr(tib, which = "type") <- "impurity"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


#' @rdname vi_model
#'
#' @export
vi_model.ml_model_random_forest_classification <- function(object, ...) {

  # # Check for dependency
  # if (!requireNamespace("sparklyr", quietly = TRUE)) {
  #   stop("Package \"sparklyr\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Construct model-specific variable importance scores
  vis <- sparklyr::ml_feature_importances(object, ...)
  names(vis) <- c("Variable", "Importance")
  tib <- tibble::as_tibble(vis)

  # Add variable importance type attribute
  attr(tib, which = "type") <- "impurity"

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: stats ---------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.lm <- function(object, type = c("stat", "raw"), ...) {

  # Determine which type of variable importance to compute
  type <- match.arg(type)

  # pattern to match based on type
  if (type == "stat") {
    type_pattern <- "^(t|z) value"
  } else {
    type_pattern <- "Estimate"
  }

  # Construct model-specific variable importance scores
  coefs <- summary(object)$coefficients
  if (attr(object$terms, "intercept") == 1) {
    coefs <- coefs[-1L, , drop = FALSE]
  }
  pos <- grep(type_pattern, x = colnames(coefs))
  tib <- tibble::tibble(
    "Variable" = rownames(coefs),
    "Importance" = abs(coefs[, pos]),
    "Sign" = ifelse(sign(coefs[, "Estimate"]) == 1, yes = "POS", no = "NEG")
  )

  # Add variable importance type attribute
  if (type == "stat") {
    label <- colnames(coefs)[pos]
    label <- substr(label, start = 1, stop = 1)  # strip off t or z
    attr(tib, which = "type") <- paste0("|", label, "-statistic|")
  } else {
    attr(tib, which = "type") <- "|raw coefficients|"
  }

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}


# Package: xgboost -------------------------------------------------------------

#' @rdname vi_model
#'
#' @export
vi_model.xgb.Booster <- function(object, type = c("gain", "cover", "frequency"),
                                 ...) {

  # # Check for dependency
  # if (!requireNamespace("xgboost", quietly = TRUE)) {
  #   stop("Package \"xgboost\" needed for this function to work. Please ",
  #        "install it.", call. = FALSE)
  # }

  # Determine which type of variable importance to compute
  type <- match.arg(type)

  # Construct model-specific variable importance scores
  imp <- xgboost::xgb.importance(model = object, ...)
  names(imp) <- tolower(names(imp))
  vis <- tibble::as.tibble(imp)[, c("feature", type)]
  tib <- tibble::tibble(
    "Variable" = vis$feature,
    "Importance" = vis[[2L]]
  )

  # Add variable importance type attribute
  attr(tib, which = "type") <- type

  # Add "vi" class
  class(tib) <- c("vi", class(tib))

  # Return results
  tib

}
