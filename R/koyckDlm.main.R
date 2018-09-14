koyckDlm.main = function(x , y){
  X.t_1 = array(NA , length(x))
  Y.1 = array(NA , length(y))
  X.t_1[-1] = x[1:(length(x) - 1)] 
  Y.1[-1] = y[1:(length(y) - 1)] 
  y.t = y
  X.t = x
  model = ivreg(y.t ~ Y.1 + X.t | Y.1 + X.t_1)
  coefs = model$coefficients
  beta = coefs[3]
  phi = coefs[2]
  alpha = coefs[1]/(1 - phi)
  geometric.coefficients = data.frame(alpha, beta , phi)
  rownames(geometric.coefficients) = "Geometric coefficients: "
  model$call = "Y ~ (Intercept) + Y.1 + X.t"
  return(list(model = model, geometric.coefficients = geometric.coefficients))
}