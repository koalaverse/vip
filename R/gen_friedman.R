#' Bin a numeric vector
#'
#' Function to bin a numeric vector
#'
#' @param x A numeric vector.
#'
#' @param Integer specifying the number of bins to split \code{x} into.
#'
#' @keywords internal
bin <- function(x, n_bins) {
  quantiles <- stats::quantile(x, probs = seq(from = 0, to = 1,
                                              length = n_bins + 1))
  bins <- cut(x, breaks = quantiles, label = FALSE, include.lowest = TRUE)
  as.factor(paste0("class", bins))
}


#' Friedman benchmark data
#'
#' Simulate data from the Friedman 1 benchmark problem. See
#' \code{\link[mlbench]{mlbench.friedman1}} for details and references.
#'
#' @param n_samples Integer specifying the number of samples (i.e., rows) to
#' generate. Default is 100.
#'
#' @param n_features Integer specifying the number of features to generate.
#' Default is 10.
#'
#' @param n_bins Integer specying the number of (roughly) equal sized bins to
#' split the response into. Default is \code{NULL} for no binning. Settinig to
#' a positive integer > 1 effectively turns this into a classification problem
#' where \code{n_bins} gives the number of classes.
#'
#' @param sigma Numeric specifying the standard deviation of the noise.
#'
#' @param seed Integer specying the random seed. If \code{NULL} (the default)
#' the results will be different each time the function is run.
#'
#' @note This function is mostly used for internal testing.
#'
#' @export
#'
#' @examples
#' gen_friedman()
gen_friedman <- function(n_samples = 100, n_features = 10, n_bins = NULL,
                         sigma = 0.1, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  x <- matrix(stats::runif(n_samples * n_features), ncol = n_features)
  colnames(x) <- paste0("x", seq_len(n_features))
  y = 10 * sin(pi * x[, 1L] * x[, 2L]) + 20 * (x[, 3L] - 0.5) ^ 2 +
    10 * x[, 4L] + 5 * x[, 5L] + stats::rnorm(n_samples, sd = sigma)
  friedman <- as.data.frame(cbind(y = y, x))
  if (!is.null(n_bins)) {
    n_bins <- as.integer(n_bins)
    if (n_bins < 2) {
      stop("Argument `n_bins` shouls be a postive integer > 1.", call. = FALSE)
    }
    friedman$y <- bin(friedman$y, n_bins = n_bins)
  }
  friedman
}
