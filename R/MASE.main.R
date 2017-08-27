MASE.main = function(observed , fitted ){
  Y.t = observed
  n = length(fitted)
  e.t = Y.t - fitted
  sum = 0 
  for (i in 2:n){
    sum = sum + Y.t[i] - Y.t[i-1] 
  }
  q.t = sum/(n-1)
  MASE = mean(abs(q.t))
  return(list(MASE = MASE))
}