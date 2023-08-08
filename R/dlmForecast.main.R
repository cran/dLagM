dlmForecast.main <- function(model , x , h = 1 , type , epsilon = NULL){
    if (is.null(epsilon) == TRUE){
      epsilon <- array(0, h)
    }
    forecasts <- array(NA, h)
    coefs <- model$model$coefficients
    n <- nrow(model$model$model)
    m <- ncol(model$model$model)
    if (type == 1){
      removed <- unlist(model$removed)
      x.obs <- model$model$model[n , 2:m] # The last row of design matrix
      x.obs <- wavethresh::guyrot(x.obs,1)
      x.obs[1] <- x[1] 
      # HD: added in version 1.1.9
      if (is.null(removed)){
        x.obs <- as.vector(data.frame(1 , x.obs))
      } else if ( sum(removed %in% -1) >= 1 ){ # See if remove intercept 
        x.obs <- as.vector(x.obs)
        removed <- removed[which(removed != -1)]
      } 
      # HD: added in version 1.1.9
      
      for (i in 1:h){
        forecasts[i] <- as.vector(coefs) %*% as.numeric(t(x.obs)) # HD: added as.numeric() in version 1.1.9
        x.obs <- wavethresh::guyrot(x.obs,1)
        if (i != h){
          x.obs[1] <- 1
          x.obs[2] <- x[(i+1)]
        }
      }
    } else if (type == 2){
      k <- model$k
      q <- model$q
      x.obsG <- array(NA , dim = c(k, q + 1))
      for (j in 1:k){
        x.obs <- model$model$model[n , ((j+1)+q*(j-1)):(j*(q+1)+1)] # The last row of design matrix for each independent series
        x.obs <- wavethresh::guyrot(x.obs,1)
        x.obs[1] <- x[j, 1] 
        x.obsG[j, ] <- as.vector(t(x.obs))
      }
      if (sum(names(model$model$coefficients) == "(Intercept)") == 1 ){
        x.obs <- c(1 , as.vector(t(x.obsG)))
      } else {
        x.obs <- as.vector(t(x.obsG))
      }
      for (i in 1:h){
        forecasts[i] <- as.vector(coefs)%*%x.obs + epsilon[i]
        if (i != h){
          for (j in 1:k){
            x.obsG[j ,] <-  wavethresh::guyrot(x.obsG[j ,] , 1)  
          }
          x.obsG[ , 1] <- x[ , i + 1]
          if (sum(names(model$model$coefficients) == "(Intercept)") == 1 ){
            x.obs <- c(1 , as.vector(t(x.obsG)))
          } else{
            x.obs <- as.vector(t(x.obsG))
          }
        }
      }
    } else if (type == 3){
      k <- model$k
      q <- model$q
      removed <- model$removed
      x.obsG <- array(NA , dim = c(k, q + 1))
      fullModel <- dlm(formula = model$formula, data = model$data , q = q )
      for (j in 1:k){
        x.obs <- fullModel$model$model[nrow(fullModel$model$model) , ((j+1)+q*(j-1)):(j*(q+1)+1)] # The last row of design matrix for each independent series
        x.obs <- wavethresh::guyrot(x.obs,1)
        x.obs[1] <- x[j, 1] 
        x.obsG[j, ] <- as.vector(t(x.obs))
      }
      x.obsO <- x.obsG
      for (j in 1:k){
        x.obsG[j, (removed[[j]] + 1)] <- NA
      }
      if (sum(names(model$model$coefficients) == "(Intercept)") == 1 ){
        x.obs <- c(1 , as.vector(t(x.obsG)))
      } else {
        x.obs <- as.vector(t(x.obsG))
      }
      x.obs <- x.obs[which(is.na(x.obs) == FALSE)]
      for (i in 1:h){
        forecasts[i] <- as.vector(coefs)%*%x.obs + epsilon[i]
        if (i != h){
          for (j in 1:k){
            x.obsO[j ,] <-  wavethresh::guyrot(x.obsO[j ,] , 1)  
          }
          x.obsG <- x.obsO
          x.obsG[ , 1] <- x[ , i + 1]
          for (j in 1:k){
            x.obsG[j, (removed[[j]] + 1)] <- NA
          }
          if (sum(names(model$model$coefficients) == "(Intercept)") == 1 ){
            x.obs <- c(1 , as.vector(t(x.obsG)))
          } else{
            x.obs <- as.vector(t(x.obsG))
          }
          x.obs <- x.obs[which(is.na(x.obs) == FALSE)]
        }
      }
      
    }
    return(list(forecasts = forecasts))
}