sortScore.main <- function(x, score){
  if (score == "aic"){
    print(x[with(x, order(AIC)),])
  } else if (score == "bic") {
    print(x[with(x, order(BIC)),])
  } else {
    warning("The argument score only accepts valid arguments either aic or bic.")
  }
}