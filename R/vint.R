#' Interaction effects
#'
#' Quantify the strength of two-way interaction effects using a simple
#' \emph{feature importance ranking measure} (FIRM) approach. For details, see
#' \href{https://arxiv.org/abs/1805.04755}{Greenwell et al. (2018)}.
#'
#' @param object A fitted model object (e.g., a \code{"randomForest"} object).
#'
#' @param feature_names Character string giving the names of the two features of
#' interest.
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
#' @details This function quantifies the strength of interaction between
#' features $X_1$ and $X_2$ by measuring the change in variance along slices of
#' the partial dependence of $X_1$ and $X_2$ on the target $Y$. See
#' \href{https://arxiv.org/abs/1805.04755}{Greenwell et al. (2018)} for
#' details and examples.
#'
#' @references
#' Greenwell, B. M., Boehmke, B. C., and McCarthy, A. J.: A Simple
#' and Effective Model-Based Variable Importance Measure. arXiv preprint
#' arXiv:1805.04755 (2018).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #
#' # The Friedman 1 benchmark problem
#' #
#'
#' # Load required packages
#' library(gbm)
#' library(ggplot2)
#' library(mlbench)
#'
#' # Generate training data
#' set.seed(101)  # for reproducibility
#' friedman1 <- as.data.frame(mlbench.friedman1(500, sd = 0.1))
#'
#' #
#' # NOTE: The only interaction that actually occurs in the model from which
#' # these data are generated is between x.1 and x.2!
#' #
#'
#' # Fit a GBM to the training data
#' set.seed(102)  # for reproducibility
#' fit <- gbm(y ~ ., data = friedman1, distribution = "gaussian",
#'            n.trees = 1000, interaction.depth = 2, shrinkage = 0.01,
#'            bag.fraction = 0.8, cv.folds = 5)
#' best_iter <- gbm.perf(fit, plot.it = FALSE, method = "cv")
#'
#' # Quantify relative interaction strength
#' all_pairs <- combn(paste0("x.", 1:10), m = 2)
#' res <- NULL
#' for (i in seq_along(all_pairs)) {
#'   interact <- vint(fit, feature_names = all_pairs[, i], n.trees = best_iter)
#'   res <- rbind(res, interact)
#' }
#'
#' # Plot top 20 results
#' top_20 <- res[1:20, ]
#' ggplot(top_20, aes(x = reorder(Variables, Interaction), y = Interaction)) +
#'   geom_col() +
#'   coord_flip() +
#'   xlab("") +
#'   ylab("Interaction strength")
#' }
vint <- function(object, feature_names, progress = "none", parallel = FALSE,
                 paropts = NULL, ...) {
  # warning("This function is experimental, use at your own risk!", call. = FALSE)
  # FIXME: Should we force `chull = FALSE` in the call to `pdp::partial()`?
  all.pairs <- utils::combn(feature_names, m = 2)
  ints <- plyr::aaply(
    all.pairs, .margins = 2, .progress = progress, .parallel = parallel,
    .paropts = paropts,
    .fun = function(x) {
      pd <- pdp::partial(object, pred.var = x, ...)
      mean(c(
        stats::sd(tapply(pd$yhat, INDEX = pd[[x[1L]]], FUN = stats::sd)),
        stats::sd(tapply(pd$yhat, INDEX = pd[[x[2L]]], FUN = stats::sd))
      ))
  })
  ints <- data.frame(
    "Variables" = paste0(all.pairs[1L, ], "*", all.pairs[2L, ]),
    "Interaction" = ints
  )
  ints <- ints[order(ints["Interaction"], decreasing = TRUE), ]
  tibble::as.tibble(ints)
}
