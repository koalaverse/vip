#' Add sparklines
#'
#' Create an HTML widget to display variable importance scores with a sparkline
#' representation of each features effect (i.e., its partial dependence
#' function).
#'
#' @param object An object that inherits from class \code{"vi"}.
#'
#' @param fit The original fitted model. Only needed if `vi()` was not called
#' with `method = "firm"`.
#'
#' @param digits Integer specifying the minimal number of significant digits to
#' use for displaying importance scores and, if available, their standard
#' deviations.
#'
#' @param free_y Logical indicating whether or not the the y-axis limits should
#' be allowed to vary for each sparkline. Default is \code{FALSE}.
#'
#' @param verbose Logical indicating whether or not to print progress. Default
#' is \code{FALSE}.
#'
#' @param ... Additional optional arguments to be passed onto
#' \code{\link[pdp]{partial}}.
#'
#' @return An object of class \code{c("datatables", "htmlwidget")}; essentially,
#' a data frame with three columns: \code{Variable}, \code{Importance}, and
#' \code{Effect} (a sparkline representation of the partial dependence
#' function). For \code{"lm"/"glm"}-like objects, an additional column, called
#' \code{Sign}, is also included which includes the sign (i.e., POS/NEG) of the
#' original coefficient.
#'
#' @references
#' Greenwell, B. M., Boehmke, B. C., and McCarthy, A. J. A Simple
#' and Effective Model-Based Variable Importance Measure. arXiv preprint
#' arXiv:1805.04755 (2018).
#'
#' @rdname add_sparklines
#'
#' @export
add_sparklines <- function(object, fit, digits = 3, free_y = FALSE,
                verbose = FALSE, ...) {
  UseMethod("add_sparklines")
}


#' @rdname add_sparklines
#'
#' @export
add_sparklines.vi <- function(object, fit, digits = 3, free_y = FALSE,
                              verbose = FALSE, ...) {

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package \"DT\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package \"htmlwidgets\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }
  if (!requireNamespace("sparkline", quietly = TRUE)) {
    stop("Package \"sparkline\" needed for this function to work. Please ",
         "install it.", call. = FALSE)
  }

  # Save attribute
  vi_type <- attr(object, which = "type")

  # Remove rows with NA
  object <- stats::na.omit(object)

  # Get partial dependence functions
  if (vi_type == "firm") {
    pd <- attr(object, which = "effects")
    if ("yhat.id" %in% names(pd)) {
      pd <- lapply(pd, FUN = average_ice_curves.ice)
    }
  } else {  # compute partial dependence
    if (!requireNamespace("pdp", quietly = TRUE)) {
      stop("Package \"pdp\" needed for this function to work. Please ",
           "install it.", call. = FALSE)
    }
    if (verbose) {
      message("Computing partial dependence...")
    }
    pd <- lapply(object$Variable, FUN = function(x) {
      if (verbose) {
        message("  ", x)
      }
      pdp::partial(fit, pred.var = x, ...)
    })
    names(pd) <- object$Variable
  }

  # Prep data for sparklines
  object$Effect <- NA
  for (i in seq_len(nrow(object))) {
    object[["Effect"]][i] <-
      paste(pd[[object$Variable[i]]][["yhat"]], collapse = ",")
  }

  # Assign column definition to the column labeled "Effect"
  columnDefs = list(list(
    targets = which(names(object) == "Effect"),
    render = htmlwidgets::JS("function(data, type, full){
    return '<span class=spark>' + data + '</span>'
  }")
  ))

  # Function that is called every time the DataTable performs a draw.
  if (free_y) {
    fnDrawCallback = htmlwidgets::JS(
      "function (oSettings, json) {
        $('.spark:not(:has(canvas))').sparkline('html', {
          type: 'line',
          highlightColor: 'orange'
        });
      }"
    )
  } else {
    ylim <- range(sapply(pd, FUN = function(x) x$yhat))
    fnDrawCallback <- htmlwidgets::JS(paste0(
      "function (oSettings, json) {
        $('.spark:not(:has(canvas))').sparkline('html', {
          type: 'line',
          highlightColor: 'orange',
          chartRangeMin: ", ylim[1L], ",
          chartRangeMax: ", ylim[2L], "
        });
      }"
    ))
  }

  # Pad with zeros so that decimals are aligned in the DataTable
  object$Importance <- sprintf(object$Importance,
                               fmt = paste0("%#.", digits, "f"))
  if ("StDev" %in% names(object)) {
    object$StDev <- sprintf(object$StDev, fmt = paste0("%#.", digits, "f"))
  }

  # Construct the DataTable
  d <- DT::datatable(object, options = list(
    columnDefs = columnDefs,
    fnDrawCallback = fnDrawCallback
  ))
  d$dependencies <-
    append(d$dependencies, htmlwidgets::getDependency('sparkline'))

  # Restore attribute
  attr(d, which = "type") <- vi_type

  # Add "vi" class
  # class(d) <- c("vit", class(d))

  # Return results
  d

}


#' @keywords internal
average_ice_curves <- function(object) {
  UseMethod("average_ice_curves")
}


#' @keywords internal
average_ice_curves.ice <- function(object) {
  yhat <- tapply(
    object[["yhat"]], INDEX = as.factor(object[[1L]]), FUN = mean,
    simplify = FALSE
  )
  res <- data.frame(
    "x" = object[seq_len(length(yhat)), 1L, drop = TRUE],
    "yhat" = unlist(yhat)
  )
  names(res)[1L] <- names(object)[1L]
  res
}


#' @keywords internal
average_ice_curves.cice <- function(object) {
  average_ice_curves.ice(object)
}
