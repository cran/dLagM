sortScore.main <- function(x, score){
  if (score == "aic"){
    print(x[with(x, order(AIC)),])
  } else if (score == "bic") {
    print(x[with(x, order(BIC)),])
  } else if (score == "mase") {
    print(x[with(x, order(MASE)),])
  } else {
    warning("The argument score only accepts valid arguments aic, bic, or mase.")
  }
}