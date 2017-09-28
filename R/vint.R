#' Variable Importance
#'
#' Compute variable importance scores for the predictors in a model.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param pred.var Character string giving the names of the predictor variables
#' of interest. Should be of length two.
#'
#' @param progress Character string giving the name of the progress bar to use
#' while constructing the interaction statistics. See
#' \code{\link[plyr]{create_progress_bar}} for details. Default is
#' \code{"none"}.
#'
#' @param parallel Logical indicating whether or not to run \code{partial} in
#' parallel using a backend provided by the \code{foreach} package. Default is
#' \code{FALSE}.
#'
#' @param paropts List containing additional options to be passed onto
#' \code{\link[foreach]{foreach}} when \code{parallel = TRUE}.
#'
#' @param ... Additional optional arguments to be passed onto
#' \code{\link[pdp]{partial}}.
#'
vint <- function(object, pred.var, progress = "none", parallel = FALSE,
                 paropts = NULL, ...) {
  if (!is.character(pred.var) || (length(pred.var) < 2)) {
    stop("'pred.var' with length >= 2.")
  }
  all.pairs <- utils::combn(pred.var, m = 2)
  ints <- plyr::aaply(all.pairs, .margins = 2, .progress = progress,
                      .parallel = parallel, .paropts = paropts,
                      .fun = function(x) {
                        pd <- pdp::partial(object, pred.var = x, ...)
                        mean(c(stats::sd(tapply(pd$yhat, INDEX = pd[[x[1L]]],
                                                FUN = stats::sd)),
                               stats::sd(tapply(pd$yhat, INDEX = pd[[x[2L]]],
                                                FUN = stats::sd))))
                      })
  ints <- data.frame("Predictors" = paste0(all.pairs[1L, ], "*", all.pairs[2L, ]),
                     "Interaction" = ints)
  ints <- ints[order(ints["Interaction"], decreasing = TRUE), ]
  tibble::as.tibble(ints)
}


#' #' @keywords internal
#' getTwoWayInt <- function(object, pred.var, ...) {
#'   pd <- pdp::partial(object, pred.var = pred.var, ...)
#'   mean(
#'     c(stats::sd(tapply(pd$yhat, INDEX = pd[[pred.var[1L]]], FUN = stats::sd)),
#'       stats::sd(tapply(pd$yhat, INDEX = pd[[pred.var[2L]]], FUN = stats::sd)))
#'   )
#' }


# # Load required packages
# library(gbm)
# library(vip)
#
# # Simulate the data
# set.seed(101)  # for reproducibility
# trn <- as.data.frame(mlbench::mlbench.friedman1(n = 500, sd = 1))
#
# # Fit a GBM
# set.seed(937)
# trn.gbm <- gbm(y ~ ., data = trn, distribution = "gaussian", n.trees = 25000,
#                shrinkage = 0.01, interaction.depth = 2, bag.fraction = 1,
#                train.fraction = 0.8, cv.folds = 5, verbose = TRUE)
# best.iter <- gbm.perf(trn.gbm, method = "cv")
# print(best.iter)
#
# getTwoWayInt(trn.gbm, pred.var = c("x.1", "x.2"), n.trees = best.iter)
# int <- vint(trn.gbm, pred.var = paste0("x.", 1:10), progress = "text",
#             n.trees = best.iter)
# barplot(int$Interaction, names.arg = int$Predictors,
#         horiz = TRUE, las = 1, cex.names = 0.75)
