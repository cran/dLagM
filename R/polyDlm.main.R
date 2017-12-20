polyDlm.main = function(x , y , q , k , show.beta = TRUE , show.summary = TRUE){
  n=length(x)
  design  = dlm(x = x , y = y , q = q , show.summary = FALSE)$designMatrix
  tr.matrix = array(0, dim = c((q+1),(k+1)))
  design.z = array(0,dim=c(length((q+1):n),(k+1)))
  design.z.colnames = array(NA, (k+1))
  
  for (i in 1:(q+1)){
    s = i - 1
    tr.matrix[i , 1] = 1
    for (j in 2:(k+1)){
      tr.matrix[i , j] = s^(j - 1)
    }
  }
  tr.matrix = t(tr.matrix)
  
  for ( i in 1:nrow(design)){
    design.z[i,] = t(tr.matrix %*% t(design[i,2:ncol(design)]))
  }
  
  for (i in 1:(k+1)){
    design.z.colnames[i] = paste0("z.t",(i-1))
  }
  
  y.t = design[,1]
  z = data.frame(y.t, design.z)
  colnames(z) = c("y.t" , design.z.colnames)
  model = lm(y.t ~ ., data = z)
  
  if (show.summary == TRUE){
    print(summary(model))
  }
  if (show.beta == TRUE){
    tests = poly.dlm.tests(coef = model$coefficients , tr.matrix = tr.matrix , y.t = y.t , design.z = design.z , n = n , q = q , print = TRUE )
    return(list(model = model, designMatrix = design.z, designMatrix.x = design, beta.coefficients = tests, transformation.matrix = tr.matrix))
  } else {
    return(list(model = model, designMatrix = design.z, designMatrix.x = design, transformation.matrix = tr.matrix))
  }
}

