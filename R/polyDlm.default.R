#' @export
polyDlm.default = function(x , y , q , k , show.beta = TRUE){
  
  options(warn=-1)
  
  if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
    stop("Data series x and y both must be in a vector format!")
  }
  q = round(q)
  if (q < 0){
    stop("Lag order must be a positive integer!")
  }
  k = round(k)
  if (k < 1){
    stop("The order of polynomial must be an integer greater than or equal to 1!")
  }
  
  res = polyDlm.main(x = x , y = y , q = q , k = k , show.beta = show.beta)
  res$call = match.call()
  class(res) = c("polyDlm" , "dLagM")
  res
}