ardlDlm.main = function(formula , data , x , y , p = 1 , q = 1 , remove , type = 1){
  remove.p <- remove$p
  remove.q <- remove$q

  if (type == 1){
    remove.intercept <- FALSE
    if ( sum(remove.q %in% -1) >= 1 ){ # See if remove intercept
      remove.intercept <- TRUE
      remove.q <- remove.q[which(remove.q != -1)]
    }
    # y.t = ts(y)
    # X.t = ts(x)
    remove.p <- unlist(remove.p)

    data <- ts(data.frame(y, x))
    colnames(data) <- c("y.t" , "X.t")
    if (remove.intercept){
      model.text <- "y.t ~ -1 + X.t"
      coef.names <- c("X.t")
    } else {
      model.text <- "y.t ~ X.t"
      coef.names <- c("(Intercept)", "X.t")
    }
    seq.p <- 1:p 
    seq.p <- seq.p[ ! seq.p %in% remove.p]
    for (i in seq.p){
      model.text <- paste0(model.text , " + L(X.t," , i , ")")
      coef.names <- c(coef.names, paste0("X." , i))
    }
    seq.q = 1:q 
    seq.q = seq.q[ ! seq.q %in% remove.q]
    for (i in seq.q){
      model.text <- paste0(model.text , " + L(y.t," , i , ")")
      coef.names <- c(coef.names, paste0("Y." , i))
    }
    
    # remove the main effect if wanted
    if (sum(remove.p == 0) > 0){ 
      model.text <- gsub(" X.t \\+" , "" , model.text) 
      coef.names <- coef.names[-2]
    }

    model.fit <- dynlm( formula = as.formula(model.text) , data = data  )
    names(model.fit$coefficients) <- coef.names
    output <- list(model = model.fit , order = c(p , q))
    model.fit$call <- paste0("Y ~ ", paste(coef.names , collapse = " + "))
  } else if (type == 2){
    
    vars = get.vars(formula)

    dep <- vars[1] #get the name of dependent variable as a string
    indeps <- vars[2:length(vars)] # get the names of independents variables
    data <- ts(data)
    k <- length(indeps) # the number of independent series
    if (sum(grep("-1",gsub("\\s", "", formula))) == 0){
      coef.names <- c("(Intercept)")
    } else {
      coef.names <- c()
    }
    model.text = paste0(dep , " ~ ")
    for (j  in 1:k){
      model.text <- paste0(model.text , " + " , indeps[j])
      coef.names <- c(coef.names, paste0(indeps[j] , ".t"))
      # coef.names = c(coef.names, paste0("X" , j , ".t"))
      seq.p = 1:p 
      if (is.null(remove.p[[indeps[j]]]) == FALSE){
        seq.p <- seq.p[ ! seq.p %in% remove.p[[indeps[j]]] ]      
      }
      for (i in seq.p){
        model.text <- paste0(model.text , " + L(" , indeps[j] , "," , i , ")")
        coef.names <- c(coef.names, paste0( indeps[j] , ".", i))
        # coef.names = c(coef.names, paste0("X" , j , ".", i))
      }
    }
    seq.q = 1:q 
    seq.q = seq.q[ ! seq.q %in% remove.q]
    for (i in seq.q){
      model.text <- paste0(model.text , " + L(", dep,  "," , i , ")")
      coef.names <- c(coef.names, paste0(dep , ".", i))
      # coef.names = c(coef.names, paste0("Y.", i))
    }

    # remove the main effects if wanted
    for ( i in 1:length(remove.p)){
      if (sum(unlist(remove.p[indeps[i]]) == 0) > 0){ 
        model.text <- gsub(paste0(" " , indeps[i] , " \\+") , "" , model.text) 
        coef.names <- coef.names[-which(coef.names == paste0(indeps[i] , ".t"))]
        # coef.names <- coef.names[-which(coef.names == paste0("X" , i , ".t"))]
      }
    }
    if (sum(grep("-1",gsub("\\s", "", formula))) != 0){
      model.text <- paste0(model.text, "-1")
    }

    model.fit <- dynlm( formula = as.formula(model.text) , data = data)
    names(model.fit$coefficients) <- coef.names
    output <- list(model = model.fit , order = c(p , q) ,  removed = remove , formula = formula , data = data)
    model.fit$call<- paste0(dep , " ~ ", paste(coef.names , collapse = " + "))
  }
  return(output)
}