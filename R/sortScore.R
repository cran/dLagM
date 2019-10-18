#' @export
sortScore<- function(x, score = c("bic", "aic", "mase", "smape", "mape", "mrae", "gmrae", "mbrae")){
  if (score == "aic"){
    print(x[with(x, order(AIC)),])
  } else if (score == "bic") {
    print(x[with(x, order(BIC)),])
  } else if (score == "mase") {
    print(x[with(x, order(MASE)),])
  } else if (score == "smape") {
    print(x[with(x, order(sMAPE)),])
  } else if (score == "mape"){
    print(x[with(x, order(MAPE)),])
  } else if (score == "mrae"){
    print(x[with(x, order(MRAE)),])
  } else if (score == "gmrae"){
    print(x[with(x, order(GMRAE)),])
  } else if (score == "mbrae") {
    print(x[with(x, order(MBRAE)),])
  } else {
    warning("The argument score only accepts valid arguments aic, bic, 
            mase, mape, smape, mrae, gmrae, mbrae.")
  }
}