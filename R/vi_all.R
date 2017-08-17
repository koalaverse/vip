#' @keywords internal
vi_all <- function(object, pred.var, ...) {
  imp <- lapply(pred.var, function(x) pdVarImp(object, pred.var = x, ...))
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
