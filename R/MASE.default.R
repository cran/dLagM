MASE.default <-
  function(model, ... ){
    
    options(warn=-1)
    
    if(!missing(...)) {# Several models
      models = list(model, ...)
      m = length(models)
      for (j in 1:m){
        if ((class(models[[j]])[1] == "polyDlm") | (class(models[[j]])[1] == "dlm") | (class(models[[j]])[1] == "koyckDlm") | (class(models[[j]])[1] == "ardlDlm")){
          Y.t = models[[j]]$model$model$y.t
          fitted = models[[j]]$model$fitted.values
        } else if (class(models[[j]])[1] == "lm"){
          Y.t = models[[j]]$model[,1]
          fitted = models[[j]]$fitted.values
        } else {
          stop("MASE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send a bunch of these objects to the function.")
        }
        # Y.t = models[[j]]$model$y.t
        # fitted = models[[j]]$fitted.values
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
      if ((class(model)[1] == "polyDlm") | (class(model)[1] == "dlm") | (class(model)[1] == "koyckDlm") | (class(model)[1] == "ardlDlm")){
        Y.t = model$model$model$y.t
        fitted = model$model$fitted.values
      } else if (class(model)[1] == "lm"){
        Y.t = model$model[,1]
        fitted = model$fitted.values
      } else {
        stop("MASE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send one of these objects to the function.")
      }
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