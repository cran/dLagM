dlm.main = function(formula , data , x , y , q , remove , show.summary = TRUE , type = 1){
  
  if (type == 1){
    n=length(x)
    r = length(remove)
    design = array(NA, dim = c(length((q+1):n),(q+2-r)))
    design.colnames = array(NA, q+2-r)
    design[,1] = y[(q+1):n]
    design[,2] = x[(q+1):n]
    design.colnames[1] = "y.t"
    design.colnames[2] = "x.t"
    modelStr = "y.t ~ x.t +"
    seq = 3:(q+2) 
    seq = seq[ ! seq %in% (remove+2)] # remove the lags to be dropped from the list
    count = 2 
    for (i in seq){
      count = count + 1
      design[,count] = x[(q-(i-3)):(n-(i-2))]
      design.colnames[count] = paste0("x.",(i-2))
    }
    design = data.frame(design)
    colnames(design) = design.colnames
    modelStr = paste0(modelStr, " + x.",(i-3))
    model = lm(y.t ~ . , design )
    output = list(model = model, designMatrix = design , q = q , removed = remove)
    model$call = "Y ~ X"
  } else if (type == 2){
    n=nrow(data)
    vars = formula.tools::get.vars(formula)
    
    dep = vars[1] #get the name of dependent variable as a string
    indeps = vars[2:length(vars)] # get the names of independents variables

    indepData = data[,indeps] # get the data for independent series
    if (length(vars) > 2){ # If the number of independent series is greater than 1
     
      k = ncol(indepData) # the number of independent series
      design = array(NA, dim = c(length((q+1):n) , (q+1) , k)) # the first dimension keeps the design matrix for each independent series
      design.colnames = array(NA, (k*q+k))
      modelStr = paste0(dep, " ~ ")
      depVar = data[(q+1):n , dep]
      count = 0
      design2 = list()
      for (j  in 1:k){
        
        count = count + 1
        design[ , 1 , j] = indepData[(q+1):n , j]
        design.colnames[count] = paste0(vars[j + 1] , ".t")
        modelStr = paste0(modelStr , vars[j + 1] , ".t + ")
        seq = 2:(q+1) 
        if (is.null(remove) == FALSE){
          seq = seq[ ! seq %in% ( remove[j , which(is.na(remove[ j , ]) == FALSE )]  + 1)] # remove the lags to be dropped from the list
        }
        count2 = 1 
        
        for (i in seq){#2:(q+1)){
          count2 = count2 + 1
          count = count + 1
          design[, count2 , j] = indepData[(q-(i-2)):(n-(i-1)), j ]
          design.colnames[count] = paste0(vars[j + 1] , "." , (i-1))
          modelStr = paste0(modelStr, vars[j + 1] , "." , (i-1) , " + ")
        }
        if (is.null(remove) == FALSE){# If there is something to remove, you will have columns with all NAs! Get rid of them.
            design2[[j]] = design[ , colSums(is.na(design[ , , j])) < nrow(design[, , j]) , j]
        }
      }
      if (is.null(remove) == FALSE){
        design = design2
      }
    } else {
      k = 1 # the number of independent series
       
      design = array(NA, dim = c(length((q+1):n) , (q+1))) # the first dimension keeps the design matrix for each independent series
      design.colnames = array(NA, (k*q+k))
      modelStr = paste0(dep, " ~ ")
      depVar = data[(q+1):n , dep]
      count = 1
      design[ , 1] = indepData[(q+1):n]
      design.colnames[count] = paste0(vars[2] , ".t")
      modelStr = paste0(modelStr , vars[2] , ".t + ")
      seq = 2:(q+1) 
      if (is.null(remove) == FALSE){
        seq = seq[ ! seq %in% ( remove[1 , which(is.na(remove[ 1 , ]) == FALSE )]  + 1)] # remove the lags to be dropped from the list
      }
      count2 = 1 
      for (i in seq){
        count = count + 1
        count2 = count2 + 1
        design[, count2] = indepData[(q-(i-2)):(n-(i-1))]
        design.colnames[count] = paste0(vars[2] , "." , (i-1))
        modelStr = paste0(modelStr, vars[2] , "." , (i-1) , " + ")
      }
    }
      
    design = data.frame(depVar , design)
    colnames(design) = c(dep , design.colnames[1:(length(design)-1)])
    model = lm(formula(substr(modelStr, 1, nchar(modelStr)-3) ) , design )
    output = list(model = model, designMatrix = design , k = (length(vars) - 1) , q = q , removed = remove , formula = formula , data = data)
    model$call = toString(formula)
  }
  
  if (show.summary == TRUE){
    print(summary(model))
    ic = data.frame(AIC(model) , BIC(model))
    colnames(ic) = c("AIC" , "BIC")
    cat("AIC and BIC values for the model:\n")
    print(ic)
  }
  return(output)
}
