koyckDlm.main = function(x , y , show.summary = TRUE){
  X.t_1 = Lag(x,+1)
  Y.t_1 = Lag(y,+1)
  Y.t = y
  X.t = x
  model = ivreg(Y.t ~ Y.t_1 + X.t | Y.t_1 + X.t_1)
  # Notice here that first we write full model and then link instrumental variable after the symbol "|"
  coefs = model$coefficients
  beta = coefs[3]
  phi = coefs[2]
  alpha = coefs[1]/(1 - phi)
  geometric.coefficients = data.frame(alpha, beta , phi)
  if (show.summary == TRUE){
    print(summary(model))
    print(geometric.coefficients)
  }
  rownames(geometric.coefficients) = "Geometric coefficients: "
  return(list(model = model, geometric.coefficients = geometric.coefficients))
  
}