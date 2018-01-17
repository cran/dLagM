koyckDlm.main = function(x , y , show.summary = TRUE){
  X.t_1 = Lag(x,+1)
  Y.1 = Lag(y,+1)
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
  if (show.summary == TRUE){
    print(summary(model))
    print(geometric.coefficients)
  }
  return(list(model = model, geometric.coefficients = geometric.coefficients))
}