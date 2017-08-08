ardlDlm.main = function( x , y , p = 1 , q = 1 , show.summary = TRUE){
  Y.t = ts(y)
  X.t = ts(x)
  model.text = "Y.t ~ X.t"
  for (i in 1:p){
    model.text = paste0(model.text , " + L(X.t," , i , ")")
  }
  for (i in 1:q){
    model.text = paste0(model.text , " + L(Y.t," , i , ")")
  }
  model = dynlm( formula(model.text) )  
  if (show.summary == TRUE){
    print(summary(model))
  }
  return(list(model = model , order = c(p , q)))
}