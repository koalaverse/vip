#' @keywords internal
vi_all <- function(object, pred.var, FUN = NULL, ...) {
  imp <- lapply(pred.var, function(x) {
    pdVarImp(object, pred.var = x, FUN = FUN, ...)
  })
  names(imp) <- pred.var
  if (!is.null(attr(imp[[1L]], "partial"))) {
    pd <- lapply(imp, FUN = function(x) attr(x, "partial"))
    names(pd) <- pred.var
    imp <- unlist(imp)
    attr(imp, "partial") <- pd
    imp
  } else {
    unlist(imp)
  }
}
