
#' @export
summary.dlm <- function(object,...){
  model <- object$model
  print(summary(model))
  ic <- data.frame(AIC(model) , BIC(model))
  colnames(ic) <- c("AIC" , "BIC")
  cat("AIC and BIC values for the model:\n")
  print(ic)
}

#' @export
residuals.dlm <- function(object,...){
  print(residuals(object$model))
}

#' @export
coef.dlm <- function(object,...){
  print(coefficients(object$model))
}

#' @export
fitted.dlm <- function(object,...){
  print(fitted(object$model))
}

#' @export
AIC.dlm <- function(object,...){
  print(AIC(object$model))
}

#' @export
BIC.dlm <- function(object,...){
  print(BIC(object$model))
}

#' @export
summary.ardlDlm <- function(object,...){
  print(summary(object$model))
}

#' @export
coef.ardlDlm <- function(object,...){
  print(coefficients(object$model))
}

#' @export
residuals.ardlDlm <- function(object,...){
  print(residuals(object$model))
}


#' @export
fitted.ardlDlm <- function(object,...){
  print(fitted(object$model))
}

#' @export
AIC.ardlDlm <- function(object,...){
  print(AIC(object$model))
}

#' @export
BIC.ardlDlm <- function(object,...){
  print(BIC(object$model))
}

#' @export
summary.koyckDlm <- function(object, diagnostics = FALSE, ...){
  print(summary(object$model))
  cat("Diagnostic tests:\n")
  print(summary(object$model, diagnostics = diagnostics)$diagnostics[1:2,])
  cat("\n")
  print(object$geometric.coefficients)
}

#' @export
residuals.koyckDlm <- function(object, ...){
  print(residuals(object$model))
}

#' @export
fitted.koyckDlm <- function(object, ...){
  print(fitted(object$model))
}

#' @export
coef.koyckDlm <- function(object, ...){
  print(coefficients(object$model))
}

#' @export
AIC.koyckDlm <- function(object, ...){
  attributes(object$model)$class[1] <- "lm"
  print(AIC(object$model))
}

#' @export
BIC.koyckDlm <- function(object, ...){
  attributes(object$model)$class[1] <- "lm"
  print(BIC(object$model))
}

#' @export
summary.polyDlm <- function(object,...){
  print(summary(object$model))
}

#' @export
coef.polyDlm <- function(object, ...){
  print(coefficients(object$model))
}

#' @export
fitted.polyDlm <- function(object, ...){
  print(fitted(object$model))
}

#' @export
residuals.polyDlm <- function(object, ...){
  print(residuals(object$model))
}

#' @export
AIC.polyDlm <- function(object, ...){
  print(AIC(object$model))
}

#' @export
BIC.polyDlm <- function(object, ...){
  print(BIC(object$model))
}
