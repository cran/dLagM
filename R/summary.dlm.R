#' @export
summary.dlm = function(object,...){
  model = object$model
  print(summary(model))
  ic = data.frame(AIC(model) , BIC(model))
  colnames(ic) = c("AIC" , "BIC")
  cat("AIC and BIC values for the model:\n")
  print(ic)
}
  