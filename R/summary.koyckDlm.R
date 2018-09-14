#' @export
summary.koyckDlm = function(object,...){
  print(summary(object$model))
  print(object$geometric.coefficients)
}
  