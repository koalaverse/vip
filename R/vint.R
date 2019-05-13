#' Interaction Effects
#'
#' Compute the strength of two-way interaction effects. For details, see the
#' reference below.
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
#' @details
#' Coming soon!
#'
#' @references
#' Greenwell, B. M., Boehmke, B. C., and McCarthy, A. J.: A Simple
#' and Effective Model-Based Variable Importance Measure. arXiv preprint
#' arXiv:1805.04755 (2018).
#'
#' @export
vint <- function(object, feature_names, progress = "none", parallel = FALSE,
                 paropts = NULL, ...) {
  warning("This function is experimental, use at your own risk!", call. = FALSE)
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
