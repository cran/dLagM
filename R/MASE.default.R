MASE.default <-
  function(model, ... ){
    
    options(warn=-1)
    
    if(!missing(...)) {# Several models
      models = list(model, ...)
      m = length(models)
      for (j in 1:m){
        Y.t = models[[j]]$model$y.t
        fitted = models[[j]]$fitted.values
        n = length(fitted)
        e.t = Y.t - fitted
        sum = 0 
        for (i in 2:n){
          sum = sum + abs(Y.t[i] - Y.t[i-1] )
        }
        q.t = e.t / (sum/(n-1))
        if (j == 1){
          MASE = data.frame( n = n , MASE = mean(abs(q.t)))
          colnames(MASE) = c("n" , "MASE")
        } else {
          MASE = rbind(MASE, c(n , mean(abs(q.t))))
        }
      }
      Call <- match.call()
      row.names(MASE) = as.character(Call[-1L])
      MASE
    } else { # Only one model
      Y.t = model$model$y.t
      fitted = model$fitted.values
      n = length(fitted)
      e.t = Y.t - fitted
      sum = 0 
      for (i in 2:n){
        sum = sum + abs(Y.t[i] - Y.t[i-1] )
      }
      q.t = e.t / (sum/(n-1))
      MASE = data.frame( MASE = mean(abs(q.t)))
      colnames(MASE) = c("MASE")
      Call <- match.call()
      row.names(MASE) = as.character(Call[-1L])
      MASE
    }
    
  }