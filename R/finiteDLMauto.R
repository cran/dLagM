#' @export
finiteDLMauto <- function(formula, data, x, y, q.min = 1, q.max = 10, k.order = NULL, model.type = c("dlm","poly"), error.type = c("MASE","AIC","BIC","GMRAE", "MBRAE", "radj"), trace = FALSE, type)UseMethod("finiteDLMauto")

