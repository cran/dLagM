#' @importFrom wavethresh guyrot
#' @importFrom AER ivreg
#' @importFrom plyr summarize
#' @importFrom plyr is.discrete
#' @importFrom dynlm dynlm
#' @importFrom formula.tools get.vars
#' @importFrom plyr is.formula
#' @import nardl


poly.dlm.tests = function(coef , y.t , design.z , tr.matrix , n , q , print = TRUE ){
  z = data.frame(array(1,(n-q)) , design.z )
  z = as.matrix(z) # Create design matrix
  H = z%*%solve(t(z)%*%z)%*%t(z) # Hat martix
  I = diag(n-q)
  MSE = (t(y.t)%*%(I-H)%*%y.t)/(n - q - length(coef)) 
  
  C = MSE[1,1]*solve(t(z)%*%z) 
  # Covariance matrix of parameter estimates for Almon DLM
  # Get the values of gamma parameters from the model object
  gamma = coef[2:length(coef)] 
  beta = t(tr.matrix) %*% gamma
  covar.beta = t(tr.matrix) %*% C[2:nrow(C),2:nrow(C)] %*% tr.matrix
  var.beta = diag(covar.beta)
  
  stDev.beta = sqrt(var.beta)
  t.value = beta/stDev.beta
  t.pr = 2 * pt(abs(beta/stDev.beta), n-q-q,lower.tail = FALSE)
  
  beta.estimates = data.frame(beta, stDev.beta, t.value, t.pr)
  colnames(beta.estimates) = c("Estimate", "Std. Error", "t value", "P(>|t|)")
  rowNames = array(NA , (q+1))
  for (i in 1:(q+1)){
    rowNames[i] = paste0("beta.",(i-1))
  }
  rownames(beta.estimates) = rowNames
  if (print == TRUE){
    cat("Estimates and t-tests for beta coefficients:\n")
    print(signif(beta.estimates, digits =3))
  }
  return(list(beta = beta , stDev.beta = stDev.beta , cov.beta = covar.beta , t.value = t.value , t.pr = t.pr))
}