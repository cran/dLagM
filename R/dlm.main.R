dlm.main = function(x , y , q , show.summary = TRUE){
  n=length(x)
  design = array(NA, dim = c(length((q+1):n),(q+2)))
  design.colnames = array(NA, q+2)
  design[,1] = y[(q+1):n]
  design[,2] = x[(q+1):n]
  design.colnames[1] = "y.t"
  design.colnames[2] = "x.t"
  modelStr = "y.t ~ x.t +"
  for (i in 3:(q+2)){
    design[,i] = x[(q-(i-3)):(n-(i-2))]
    design.colnames[i] = paste0("x.",(i-2))
  }
  design = data.frame(design)
  colnames(design) = design.colnames
  modelStr = paste0(modelStr, " + x.",(i-3))
  model = lm(y.t ~ . , design )
  if (show.summary == TRUE){
    print(summary(model))
    ic = data.frame(AIC(model) , BIC(model))
    colnames(ic) = c("AIC" , "BIC")
    cat("AIC and BIC values for the model:\n")
    print(ic)
  }
  return(list(model = model, designMatrix = design))
}
