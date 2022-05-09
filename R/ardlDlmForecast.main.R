matchCoefsToDesign <- function(vars, indeps, coefs, design){
  obs <- array()
  terms <- names(coefs)
  for (j in 1:length(terms)){
    elements <- strsplit(terms[j], "\\.")[[1]]
    if (terms[j] == "(Intercept)"){
      obs[j] <- 1  
    } else if (length(which(elements[1] == indeps)) > 0){
      if (elements[2] == "t"){ # Main effect of independent
        elements <- strsplit(terms[j], "\\.")[[1]]
        obs[j] <- design[which(names(design) == paste0("L(", elements[1], ", ", "0)"))]
      } else { # Lag of independent
        element <- paste0("L(", strsplit(terms[j], "\\.")[[1]][1], ", " , strsplit(terms[j], "\\.")[[1]][2], ")")
        obs[j] <- design[which(names(design) == element)]
      }
    } else if (grepl(setdiff(vars,indeps), terms[j], fixed = TRUE)){ # Dependent series
      element <- paste0("L(", strsplit(terms[j], "\\.")[[1]][1], ", " , strsplit(terms[j], "\\.")[[1]][2], ")")
      obs[j] <- design[which(names(design) == element)]
    }
  }
  return(obs)
}

ardlDlmForecast.main = function(model , x , h = 1 , type , epsilon = NULL){
# x must be a matrix of dimension (number of indep series) x h
  if (is.null(epsilon) == TRUE){
    epsilon = array(0, h)
  }
  vars = get.vars(model$formula)
  indeps = vars[2:length(vars)] # get the names of independents variables
  
  coefs = unlist(model$model$coefficients)
  design = unlist(model$model$model[nrow(model$model$model) , ]) # Get the last line of the design matrix
  p = model$order[1]
  q = model$order[2]
  forecasts = array(NA, (h + q))
  forecasts[1] = design[1]
  forecasts[2:(2 + q -1)] = design[(length(design)-q+1):length(design)] 

  if (type == 1){
    x.obs = design[2:(length(design)-q)]
    x.obs = guyrot(x.obs,1)
    x.obs[1] = x[1]
    y.obs = forecasts[1:q]
    for (i in (q + 1): (h + q)){
      obs = unlist(c(1 , x.obs , y.obs))
      forecasts[i] = as.vector(coefs) %*% obs + epsilon[(i-q)]
      x.obs = guyrot(x.obs,1)
      if (i != (h + q)){
        x.obs[1] = x[(i - q + 1)]
      }
      y.obs <- guyrot(y.obs,1)
      y.obs[1] <- forecasts[i]
    }
    return(list(forecasts = forecasts[(q + 1): (h + q)]))
  } else if (type == 2){
    k = nrow(x) # number of indep series
    y.obs = forecasts[1:q]
    x.obs = array(NA, dim = c(k , (p+1)))
    designM = matrix(design[2:(length(design)-q)], nrow = k , ncol = (p+1) , byrow = TRUE)
    for ( j in 1:k){
      x.obs[j, ] = designM[j, ]
      x.obs[j, ] = guyrot(x.obs[j, ],1)
      x.obs[j , 1] = x[j , 1] 
    }
    for (i in (q + 1): (h + q)){
      if (sum(names(model$model$coefficients) == "(Intercept)") == 1 ){
        obs = unlist(c(1 , t(x.obs) , y.obs))
      } else {
        obs = unlist(c( t(x.obs) , y.obs))
      }

      forecasts[i] = as.vector(coefs) %*% obs + epsilon[(i-q)]
      for ( j in 1:k){
        x.obs[j, ] = guyrot(x.obs[j, ] , 1)
        if (i != (h + q)){ 
          x.obs[j , 1] = x[j , (i - q + 1)]
        }
      }
      if (i == (q + 1)){
        y.obs[1] <- forecasts[i]
        if (q > 1){
          y.obs[2:(i-1)] <- forecasts[1:(q-1)]
        }
      } else {
        y.obs <- guyrot(y.obs,1)
        y.obs[1] <- forecasts[i]
      }
    }
    return(list(forecasts = forecasts[(q + 1): (h + q)]))
  } else if (type == 3){
    k = nrow(x) # number of indep series
    # Columns of x show t+1, t+2, ...
    # Rows of x show x1 and x2
    
    # designOriginal <- design
    # design <- designOriginal
    full.model = ardlDlm(formula = model$formula , data = data.frame(model$data) , p = p , q = q )
    design = unlist(full.model$model$model[nrow(model$model$model) , ]) 
    designTerms <- names(design)
    mains <- which(grepl("(", designTerms, fixed = TRUE) == FALSE)
    for (j in 1:length(mains)){
      designTerms[mains[j]] <- paste0("L(",designTerms[mains[j]], ", ", "0)")
    }
    names(design) <- designTerms
    
    forecasts <- array()
    designNew <- array()
    for ( t in (1:h)){
      #yDesign <- guyrot(design[grepl(setdiff(vars,indeps), names(design), fixed = TRUE)],1)# The first element of this will be replaced by the new forecast
      yDesign <- guyrot(design[grepl(paste0("(",setdiff(vars,indeps), ","), names(design), fixed = TRUE)],1)# The first element of this will be replaced by the new forecast
      designNew <- c(yDesign)
      xDesign <- list()
      for (j in 1:k){
        xDesign[[j]] <- guyrot(design[grepl(indeps[j], names(design), fixed = TRUE)],1)
        # The first element of these will be replaced by the new independent observation
        xDesign[[j]][1] <- x[j, t] # t counts time
        designNew <- c(designNew, xDesign[[j]])
      }
      
      obs <- matchCoefsToDesign(vars, indeps, coefs, designNew)
      
      forecasts[t] <- as.vector(coefs) %*% obs + epsilon[t]
      designNew[1] <- forecasts[t]
      design <- designNew
    }
    return(list(forecasts = forecasts))
  }
}
