dlmForecast.main = function(model , x , h = 1){
  forecasts = array(NA, h)
  coefs = model$model$coefficients
  n = nrow(model$model$model)
  m = ncol(model$model$model)
  x.obs = model$model$model[n , 2:m] # The last row of design matrix
  x.obs = guyrot(x.obs,1)
  x.obs[1] = x[1] 
  x.obs = as.vector(data.frame(1 , x.obs))
  for (i in 1:h){
    forecasts[i] = as.vector(coefs)%*%t(x.obs)
    x.obs = guyrot(x.obs,1)
    if (i != h){
      x.obs[1] = 1
      x.obs[2] = x[(i+1)]
    }
  }
  return(list(forecasts = forecasts))
}