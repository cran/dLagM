#' @export
MASE <- function(model, ... ){
  
  options(warn=-1)
  
  if(!missing(...)) {# Several models
    models = list(model, ...)
    m = length(models)
    for (j in 1:m){
      if ((class(models[[j]])[1] == "polyDlm") | (class(models[[j]])[1] == "dlm") | (class(models[[j]])[1] == "koyckDlm") | (class(models[[j]])[1] == "ardlDlm")){
        Y.t = as.vector(models[[j]]$model$model[,1])
        fitted = as.vector(models[[j]]$model$fitted.values)
      } else if (class(models[[j]])[1] == "lm"){
        Y.t = as.vector(models[[j]]$model[,1])
        fitted = as.vector(models[[j]]$fitted.values)
      } else {
        stop("MASE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send a bunch of model objects to the function.")
      }
      n = length(fitted)
      e.t = Y.t - fitted
      q.t = e.t / mean(abs(diff(Y.t))) 
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
      Y.t = as.vector(model$model$model[,1])
      fitted = as.vector(model$model$fitted.values)
    } else if (class(model)[1] == "lm"){
      Y.t = as.vector(model$model[,1])
      fitted = as.vector(model$fitted.values)
    } else {
      stop("MASE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send one of these objects to the function.")
    }
    n = length(fitted)
    e.t = Y.t - fitted
    q.t = e.t / mean(abs(diff(Y.t)))
    MASE = data.frame( MASE = mean(abs(q.t)))
    colnames(MASE) = c("MASE")
    Call <- match.call()
    row.names(MASE) = as.character(Call[-1L])
    MASE
  }
  
}
  