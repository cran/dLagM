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
      y.obs = forecasts[(i - q + 1):i]
    }
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
      y.obs = forecasts[(i - q + 1):i]
    }
  } else if (type == 3){
    k = nrow(x) # number of indep series
    y.obs = forecasts[1:q]
    x.obs = array(NA, dim = c(k , (p+1)))
    full.model = ardlDlm(formula = model$formula , data = data.frame(model$data) , p = p , q = q )
    design = unlist(full.model$model$model[nrow(model$model$model) , ]) 
    designM = matrix(design[2:(length(design)-q)], nrow = k , ncol = (p+1) , byrow = TRUE)
    for ( j in 1:k){
      x.obs[j, ] = designM[j, ]
      x.obs[j, ] = guyrot(x.obs[j, ],1)
      x.obs[j , 1] = x[j , 1] 
    }
    removed.p = model$removed[["p"]]
    removed.q = model$removed[["q"]]
    x.obsO = x.obs
    for ( j in 1:k){
      if ( is.null(removed.p[[indeps[j]]]) == FALSE){
        x.obs[j , (removed.p[[indeps[j]]][which(is.na(removed.p[[indeps[j]]]) == FALSE) ] + 1) ] = NA 
      }
    }
    seq.q = 1:q 
    seq.q = seq.q[ ! seq.q %in% removed.q]
    for (i in (q + 1): (h + q)){
      if (sum(names(model$model$coefficients) == "(Intercept)") == 1 ){
        obs = c(1 ,  x.obs[which(is.na(x.obs) == FALSE)] , y.obs[seq.q])
      } else {
        obs = c(x.obs[which(is.na(x.obs) == FALSE)] , y.obs[seq.q])
      }
      forecasts[i] = as.vector(coefs) %*% obs + epsilon[(i-q)]
      x.obs = x.obsO
      for ( j in 1:k){
        x.obs[j, ] = guyrot(x.obs[j, ] , 1)
        if (i != (h + q)){ 
          x.obs[j , 1] = x[j , (i - q + 1)]
        }
        x.obsO = x.obs
        if ( is.null(removed.p[[indeps[j]]]) == FALSE){
          x.obs[j , (removed.p[[indeps[j]]][which(is.na(removed.p[[indeps[j]]]) == FALSE) ] + 1) ] = NA
        }
      }
      y.obs = forecasts[(i - q + 1):i]
    }
  }
  return(list(forecasts = forecasts[(q + 1): (h + q)]))
}