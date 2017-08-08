polyDlmForecast.main = function(model , x , h = 1 ){
  forecasts = array(NA, h)
  coefs = model$model$coefficients
  n = nrow(model$designMatrix.x)
  m = ncol(model$designMatrix.x) # The first column is y.t
  x.obs = model$designMatrix.x[n , 2:m] # The last row of design matrix
  x.obs = guyrot(x.obs,1)
  x.obs[1] = x[1]  # New x-vector is created
  x.obs = unlist(x.obs)
  s = length(coefs) - 1 
  q = m - 2 
  tr.matrix = model$transformation.matrix
  for (i in 1:h){
    z = c(1 , tr.matrix %*% x.obs)
    forecasts[i] = as.vector(coefs)%*%z
    x.obs = guyrot(x.obs,1)
    if (i != h){
      x.obs[1] = x[(i+1)]
    }
  }
  return(list(forecasts = forecasts))
}