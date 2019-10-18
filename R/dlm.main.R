dlm.main <- function(formula , data , x , y , q , remove , type = 1){
  remove.main = FALSE
  remove.original = remove
  if (type == 1){
    remove = unlist(remove)
    remove.intercept <- FALSE
    if ( sum(remove %in% -1) >= 1 ){ # See if remove intercept
      remove.intercept <- TRUE
      remove <- remove[which(remove != -1)]
    }
    
    if (sum(remove == 0) > 0){ 
      remove.main = TRUE
      remove = remove[which(remove != 0)]
    }
    n = length(x)
    r = length(remove)
    design = array(NA, dim = c(length((q+1):n),(q+2-r)))
    design.colnames = array(NA, q+2-r)
    design[,1] = y[(q+1):n]
    design[,2] = x[(q+1):n]
    design.colnames[1] = "y.t"
    design.colnames[2] = "x.t"
    
    if (remove.intercept){
      modelStr = "y.t ~ -1 + x.t +"
    } else {
      modelStr = "y.t ~ x.t +"
    }
    
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
    
    if (remove.main){ 
      modelStr <- gsub(" x.t \\+" , "" , modelStr) 
      design <- design[-2]
    }
    
    if (remove.intercept){
      model.formula <- as.formula(y.t ~ . -1)
    } else {
      model.formula <- as.formula(y.t ~ .)
    }

    model = lm(formula = model.formula , design )
    
    output = list(model = model, designMatrix = design , q = q , removed = remove)
    model$call = "Y ~ X"
  } else if (type == 2){
    
    n=nrow(data)
    vars = formula.tools::get.vars(formula)
    
    remove.intercept <- FALSE
    
    if ( attr(terms(formula),"intercept") == 0 ){ # See if remove intercept
      remove.intercept <- TRUE
    }
    
    dep = vars[1] #get the name of dependent variable as a string
    indeps = vars[2:length(vars)] # get the names of independents variables
    indepData = data[,indeps] # get the data for independent series
    if (length(vars) > 2){ # If the number of independent series is greater than 1
      k = ncol(indepData) # the number of independent series
      remove.main = array(FALSE , k)
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
        if (is.null(remove[[indeps[j]]]) == FALSE){
          if (sum(remove[[indeps[j]]] == 0) > 0){ 
            remove.main[j] = TRUE
            remove[[indeps[j]]] = remove[[indeps[j]]][which(remove[[indeps[j]]] != 0)]
          }
          seq = seq[ ! seq %in% ( remove[[indeps[j]]] + 1)] # remove the lags to be dropped from the list
        }
        count2 = 1 
        for (i in seq){#2:(q+1)){
          count2 = count2 + 1
          count = count + 1
          design[, count2 , j] = indepData[(q-(i-2)):(n-(i-1)), j ]
          design.colnames[count] = paste0(vars[j + 1] , "." , (i-1))
          modelStr = paste0(modelStr, vars[j + 1] , "." , (i-1) , " + ")
        }
        if (is.null(remove[[indeps[j]]]) == FALSE){# If there is something to remove, you will have columns with all NAs! Get rid of them.
            design2[[j]] = design[ , colSums(is.na(design[ , , j])) < nrow(design[, , j]) , j]
        } else {
          design2[[j]] = design[ , , j]
        }
        if (remove.main[j]){ 
          design2[[j]] = design2[[j]][ , -1]
          design.colnames = design.colnames[which( design.colnames != paste0(vars[j + 1], ".t") )]
          modelStr <- gsub(paste0(vars[j + 1] , ".t \\+") , "" , modelStr) 
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
        if ( is.list(remove) == TRUE){
          remove = unlist(remove)
          if (sum(remove == 0) > 0){ 
            remove.main = TRUE
            remove = remove[which(remove != 0)]
          }
        }
        seq = seq[ ! seq %in% ( remove[which(is.na(remove) == FALSE )]  + 1)] # remove the lags to be dropped from the list
      }
      count2 = 1 
      for (i in seq){
        count = count + 1
        count2 = count2 + 1
        design[, count2] = indepData[(q-(i-2)):(n-(i-1))]
        design.colnames[count] = paste0(vars[2] , "." , (i-1))
        modelStr = paste0(modelStr, vars[2] , "." , (i-1) , " + ")
      }
      if (remove.main){ 
        # print(paste0(indeps ,".t \\+"))
        # print(modelStr)
        modelStr <- gsub(paste0(indeps ,".t +") , "" , modelStr) 
        # print(modelStr)
        design <- design[,-1]
        design.colnames = design.colnames[-1]
      }
    }
    design.colnames = design.colnames[which(!is.na(design.colnames))]
    design = data.frame(depVar , design)
    colnames(design) = c(dep , design.colnames[1:(length(design)-1)])
    
    model.formula <- substr(modelStr, 1, nchar(modelStr)-3) 
   
    if (remove.intercept){
      model.formula <- paste0(model.formula, " -1")
      model = lm(as.formula(model.formula) , design )
    } else {
      model = lm(as.formula(model.formula) , design )
    }
    output = list(model = model, designMatrix = design , k = (length(vars) - 1) , q = q , removed = remove.original , formula = formula , data = data)
    model$call = toString(formula)
  }

  return(output)
}
