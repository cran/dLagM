ardlDlmForecast.main = function(model , x , h = 1){

  coefs = unlist(model$model$coefficients)
  design = unlist(model$model$model[nrow(model$model$model) , ]) # Get the las line of the design matrix
  p = model$order[1]
  q = model$order[2]
  forecasts = array(NA, (h + q))
  forecasts[1] = design[1]
  forecasts[2:(2 + q -1)] = design[(1 + 1 + p + 1):length(design)]
  
  x.obs = design[2:(p + 2)]
  x.obs = guyrot(x.obs,1)
  x.obs[1] = x[1] 
  y.obs = forecasts[1:q]
  for (i in (q + 1): (h + q)){
    obs = unlist(c(1 , x.obs , y.obs))
    forecasts[i] = as.vector(coefs) %*% obs
    x.obs = guyrot(x.obs,1)
    if (i != (h + q)){
      x.obs[1] = x[(i - q + 1)]
    }
    y.obs = forecasts[(i - q + 1):i]
  }
  return(list(forecasts = forecasts[(q + 1): (h + q)]))
}