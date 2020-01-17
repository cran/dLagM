koyckDlmForecast.main <- function( model , x , h = 1 , epsilon = NULL){
  if (is.null(epsilon) == TRUE){
    epsilon <- array(0, h)
  }
  forecasts <- array(NA, (h+1))
  coefs <- model$model$coefficients
  y <- model$model$model[,1]
  n <- length(y)
  forecasts[1] = y[n] # The first element of forecasts vector is the last observation of y
  for (i in 2:(h+1)){
    forecasts[i] <- coefs[1] + coefs[2] * forecasts[i-1]  + coefs[3] * x[i-1] + epsilon[(i-1)]
  }
  return(list(forecasts = forecasts[2:(h+1)]))
}
