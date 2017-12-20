ardlDlm.main = function(formula , data , x , y , p = 1 , q = 1 , remove.p , remove.q , show.summary = TRUE , type = 1){
  if (type == 1){
    # y.t = ts(y)
    # X.t = ts(x)

    data = ts(data.frame(y, x))
    colnames(data) = c("y.t" , "X.t")
    model.text = "y.t ~ X.t"
    seq.p = 1:p 
    seq.p = seq.p[ ! seq.p %in% remove.p]
    for (i in seq.p){
      model.text = paste0(model.text , " + L(X.t," , i , ")")
    }
    seq.q = 1:q 
    seq.q = seq.q[ ! seq.q %in% remove.q]
    for (i in seq.q){
      model.text = paste0(model.text , " + L(y.t," , i , ")")
    }

    model.fit = dynlm( formula = as.formula(model.text) , data = data  )
    output = list(model = model.fit , order = c(p , q))
  } else if (type == 2){
    vars = get.vars(formula)
    
    dep = vars[1] #get the name of dependent variable as a string
    indeps = vars[2:length(vars)] # get the names of independents variables

    data = ts(data)
    k = length(indeps) # the number of independent series
      
    model.text = paste0(dep , " ~ ")
    for (j  in 1:k){
      model.text = paste0(model.text , " + " , indeps[j])
      seq.p = 1:p 
      seq.p = seq.p[ ! seq.p %in% remove.p[j , ]]      
      for (i in seq.p){
        model.text = paste0(model.text , " + L(" , indeps[j] , "," , i , ")")
      }
    }
    seq.q = 1:q 
    seq.q = seq.q[ ! seq.q %in% remove.q]
    for (i in seq.q){
      model.text = paste0(model.text , " + L(", dep,  "," , i , ")")
    }

    model.fit = dynlm( formula = as.formula(model.text) , data = data)
    output = list(model = model.fit , order = c(p , q) ,  removed.p = remove.p , removed.q = remove.q , formula = formula , data = data)
  }
  
  if (show.summary == TRUE){
    print(summary(model.fit))
  }
  
  return(output)
}