#' @export
GoF <- function(model, ... ){
  
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
      n <- length(fitted)
      e.t <- Y.t - fitted
      p.e.t <- e.t / Y.t  
      q.t <- e.t / mean(abs(diff(Y.t)), na.rm = TRUE) 
      lag.Y.t <- Y.t
      lag.Y.t[2:n] <- Y.t[1:(n-1)]
      if (sum((Y.t[2:n] - lag.Y.t[2:n] == 0)) > 0){
        MRAE.value <- mean( abs((Y.t[2:n] - fitted[2:n] + 1E-10) / (Y.t[2:n] - lag.Y.t[2:n] + 1E-10) ), na.rm = TRUE ) 
        MBRAE.value <- mean( (Y.t[2:n] - fitted[2:n] + 1E-10) / ( (Y.t[2:n] - fitted[2:n] + 1E-10) + (Y.t[2:n] - lag.Y.t[2:n] + 1E-10) ), na.rm = TRUE ) 
        GMRAE.value <- exp(mean( log( abs((Y.t[2:n] - fitted[2:n] + 1E-10) / (Y.t[2:n] - lag.Y.t[2:n] + 1E-10)) ), na.rm = TRUE ) )
      } else{
        MRAE.value <- mean( abs((Y.t[2:n] - fitted[2:n]) / (Y.t[2:n] - lag.Y.t[2:n]) ), na.rm = TRUE ) 
        GMRAE.value <- exp(mean( log( abs((Y.t[2:n] - fitted[2:n]) / (Y.t[2:n] - lag.Y.t[2:n])) ), na.rm = TRUE ) )
        MBRAE.value <- mean( (Y.t[2:n] - fitted[2:n]) / ( (Y.t[2:n] - fitted[2:n]) + (Y.t[2:n] - lag.Y.t[2:n]) ), na.rm = TRUE )
      }
      
      if (j == 1){
        MAE <- data.frame( n = n , mean(abs(e.t), na.rm = TRUE))
        MSE <- mean(e.t^2, na.rm = TRUE)
        MPE <- mean(p.e.t, na.rm = TRUE)
        MAPE <- mean(abs(p.e.t), na.rm = TRUE)
        MASE <- mean(abs(q.t), na.rm = TRUE)
        sMAPE <- mean( 2 * abs(Y.t - fitted) / ( abs(Y.t)+abs(fitted) ), na.rm = TRUE ) 
        MRAE <- MRAE.value 
        GMRAE <- GMRAE.value 
        MBRAE <- MBRAE.value
        UMBRAE <- MBRAE.value / (1 - MBRAE.value)
      } else {
        MAE <-rbind(MAE, c(n, mean(abs(e.t), na.rm = TRUE)) )
        MSE <- rbind(MSE, mean(e.t^2, na.rm = TRUE))
        MPE <- rbind(MPE, mean(p.e.t, na.rm = TRUE))
        MAPE <- rbind(MAPE, mean(abs(p.e.t), na.rm = TRUE))
        MASE <- rbind(MASE, mean(abs(q.t), na.rm = TRUE))
        sMAPE <- rbind(sMAPE, mean( 2 * abs(Y.t - fitted) / ( abs(Y.t)+abs(fitted) ), na.rm = TRUE ) ) 
        MRAE <- rbind(MRAE, MRAE.value ) 
        GMRAE <- rbind(GMRAE, GMRAE.value ) 
        MBRAE <- rbind(MBRAE, MBRAE.value)
        UMBRAE <- rbind(UMBRAE, MBRAE.value / (1 - MBRAE.value))
      }
    }
    GoF <- data.frame(MAE, MPE, MAPE, sMAPE, MASE, MSE, MRAE, GMRAE, MBRAE, UMBRAE ) 
    Call <- match.call()
    colnames(GoF) <- c("n", "MAE", "MPE", "MAPE", "sMAPE", "MASE", "MSE", "MRAE", "GMRAE", "MBRAE", "UMBRAE" ) 
    row.names(GoF) = as.character(Call[-1L])
    GoF
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
    q.t = e.t / mean(abs(diff(Y.t)), na.rm = TRUE)
    p.e.t <- e.t / Y.t 
    MAE <- data.frame(n, mean(abs(e.t), na.rm = TRUE) )
    MSE <- mean(e.t^2, na.rm = TRUE)
    MPE <- mean(p.e.t, na.rm = TRUE)
    MAPE <- mean(abs(p.e.t), na.rm = TRUE)
    MASE <- mean(abs(q.t), na.rm = TRUE)
    sMAPE <- mean( 2 * abs(Y.t - fitted) / ( abs(Y.t)+abs(fitted) ), na.rm = TRUE ) 
    lag.Y.t <- Y.t
    lag.Y.t[2:n] <- Y.t[1:(n-1)]
    if (sum((Y.t[2:n] - lag.Y.t[2:n] == 0)) > 0){
      MRAE <- mean( abs((Y.t[2:n] - fitted[2:n] + 1E-10) / (Y.t[2:n] - lag.Y.t[2:n] + 1E-10) ), na.rm = TRUE ) 
      GMRAE <- exp(mean( log( abs((Y.t[2:n] - fitted[2:n] + 1E-10) / (Y.t[2:n] - lag.Y.t[2:n] + 1E-10)) ), na.rm = TRUE ) )
    } else{
      MRAE <- mean( abs((Y.t[2:n] - fitted[2:n]) / (Y.t[2:n] - lag.Y.t[2:n]) ), na.rm = TRUE ) 
      GMRAE <- exp(mean( log( abs((Y.t[2:n] - fitted[2:n]) / (Y.t[2:n] - lag.Y.t[2:n])) ), na.rm = TRUE ) )
    }

    GoF <- data.frame(MAE, MPE, MAPE, sMAPE, MASE, MSE, MRAE, GMRAE ) 
    Call <- match.call()
    colnames(GoF) <- c("n", "MAE", "MPE", "MAPE", "sMAPE", "MASE", "MSE", "MRAE", "GMRAE" ) 
    row.names(GoF) = as.character(Call[-1L])
    GoF
  }
  
}


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
        MASE = data.frame( n = n , MASE = mean(abs(q.t), na.rm = TRUE))
        colnames(MASE) = c("n" , "MASE")
      } else {
        MASE = rbind(MASE, c(n , mean(abs(q.t), na.rm = TRUE)))
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
    q.t = e.t / mean(abs(diff(Y.t)), na.rm = TRUE)
    MASE = data.frame( MASE = mean(abs(q.t), na.rm = TRUE))
    colnames(MASE) = c("MASE")
    Call <- match.call()
    row.names(MASE) = as.character(Call[-1L])
    MASE
  }
  
}
  
#' @export
sMAPE <- function(model, ... ){
  
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
        stop("sMAPE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send a bunch of model objects to the function.")
      }
      n = length(fitted)
      if (j == 1){
        sMAPE = data.frame( n = n , sMAPE = mean( 2 * abs(Y.t - fitted) / ( abs(Y.t)+abs(fitted) ) ) )
        colnames(sMAPE) = c("n" , "sMAPE")
      } else {
        sMAPE = rbind(sMAPE, c(n , mean( 2 * abs(Y.t - fitted) / ( abs(Y.t)+abs(fitted) ) ) ) )
      }
    }
    Call <- match.call()
    row.names(sMAPE) = as.character(Call[-1L])
    sMAPE
  } else { # Only one model
    if ((class(model)[1] == "polyDlm") | (class(model)[1] == "dlm") | (class(model)[1] == "koyckDlm") | (class(model)[1] == "ardlDlm")){
      Y.t = as.vector(model$model$model[,1])
      fitted = as.vector(model$model$fitted.values)
    } else if (class(model)[1] == "lm"){
      Y.t = as.vector(model$model[,1])
      fitted = as.vector(model$fitted.values)
    } else {
      stop("sMAPE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send one of these objects to the function.")
    }
    n = length(fitted)
    sMAPE = data.frame( sMAPE = mean( 2 * abs(Y.t - fitted) / ( abs(Y.t)+abs(fitted) ) ) )
    colnames(sMAPE) = c("sMAPE")
    Call <- match.call()
    row.names(sMAPE) = as.character(Call[-1L])
    sMAPE
  }
  
}


#' @export
MAPE <- function(model, ... ){
  
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
        stop("MAPE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send a bunch of model objects to the function.")
      }
      n = length(fitted)
      if (j == 1){
        MAPE = data.frame( n = n , MAPE = mean( abs(Y.t - fitted) / abs(Y.t) ) )
        colnames(MAPE) = c("n" , "MAPE")
      } else {
        MAPE = rbind(MAPE, c(n , mean( abs(Y.t - fitted) / abs(Y.t) ) ) )
      }
    }
    Call <- match.call()
    row.names(MAPE) = as.character(Call[-1L])
    MAPE
  } else { # Only one model
    if ((class(model)[1] == "polyDlm") | (class(model)[1] == "dlm") | (class(model)[1] == "koyckDlm") | (class(model)[1] == "ardlDlm")){
      Y.t = as.vector(model$model$model[,1])
      fitted = as.vector(model$model$fitted.values)
    } else if (class(model)[1] == "lm"){
      Y.t = as.vector(model$model[,1])
      fitted = as.vector(model$fitted.values)
    } else {
      stop("MAPE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send one of these objects to the function.")
    }
    n = length(fitted)
    MAPE = data.frame( MAPE = mean( abs(Y.t - fitted) / abs(Y.t) ) )
    colnames(MAPE) = c("MAPE")
    Call <- match.call()
    row.names(MAPE) = as.character(Call[-1L])
    MAPE
  }
  
}

#' @export
MRAE <- function(model, ... ){
  
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
        stop("MRAE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send a bunch of model objects to the function.")
      }
      n = length(fitted)
      lag.Y.t <- Y.t
      lag.Y.t[2:n] <- Y.t[1:(n-1)]
      if (sum((Y.t[2:n] - lag.Y.t[2:n] == 0)) > 0){
        MRAE.value <- mean( abs((Y.t[2:n] - fitted[2:n] + 1E-10) / (Y.t[2:n] - lag.Y.t[2:n] + 1E-10) ) ) 
      } else{
        MRAE.value <- mean( abs((Y.t[2:n] - fitted[2:n]) / (Y.t[2:n] - lag.Y.t[2:n]) ) ) 
      }
      if (j == 1){
        MRAE = data.frame( n = n , MRAE = MRAE.value )
        colnames(MRAE) = c("n" , "MRAE")
      } else {
        MRAE = rbind(MRAE, c(n , MRAE.value ) )
      }
    }
    Call <- match.call()
    row.names(MRAE) = as.character(Call[-1L])
    MRAE
  } else { # Only one model
    if ((class(model)[1] == "polyDlm") | (class(model)[1] == "dlm") | (class(model)[1] == "koyckDlm") | (class(model)[1] == "ardlDlm")){
      Y.t = as.vector(model$model$model[,1])
      fitted = as.vector(model$model$fitted.values)
    } else if (class(model)[1] == "lm"){
      Y.t = as.vector(model$model[,1])
      fitted = as.vector(model$fitted.values)
    } else {
      stop("MRAE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send one of these objects to the function.")
    }
    n = length(fitted)
    lag.Y.t <- Y.t
    lag.Y.t[2:n] <- Y.t[1:(n-1)]
    if (sum((Y.t[2:n] - lag.Y.t[2:n] == 0)) > 0){
      MRAE.value <- mean( abs((Y.t[2:n] - fitted[2:n] + 1E-10) / (Y.t[2:n] - lag.Y.t[2:n] + 1E-10) ) ) 
    } else{
      MRAE.value <- mean( abs((Y.t[2:n] - fitted[2:n]) / (Y.t[2:n] - lag.Y.t[2:n]) ) ) 
    }
    MRAE = data.frame( MRAE =  MRAE.value )
    colnames(MRAE) = c("MRAE")
    Call <- match.call()
    row.names(MRAE) = as.character(Call[-1L])
    MRAE
  }
  
}

#' @export
GMRAE <- function(model, ... ){
  
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
        stop("GMRAE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send a bunch of model objects to the function.")
      }
      n <- length(fitted)
      lag.Y.t <- Y.t
      lag.Y.t[2:n] <- Y.t[1:(n-1)]
      if (sum((Y.t[2:n] - lag.Y.t[2:n] == 0)) > 0){
        GMRAE.value <- exp(mean( log( abs((Y.t[2:n] - fitted[2:n] + 1E-10) / (Y.t[2:n] - lag.Y.t[2:n] + 1E-10)) ) ) )
      } else{
        GMRAE.value <- exp(mean( log( abs((Y.t[2:n] - fitted[2:n]) / (Y.t[2:n] - lag.Y.t[2:n])) ) ) )
      }
      if (j == 1){
        GMRAE = data.frame( n = n , GMRAE = GMRAE.value )
        colnames(GMRAE) = c("n" , "GMRAE")
      } else {
        GMRAE = rbind(GMRAE, c(n , GMRAE.value ) )
      }
    }
    Call <- match.call()
    row.names(GMRAE) = as.character(Call[-1L])
    GMRAE
  } else { # Only one model
    if ((class(model)[1] == "polyDlm") | (class(model)[1] == "dlm") | (class(model)[1] == "koyckDlm") | (class(model)[1] == "ardlDlm")){
      Y.t = as.vector(model$model$model[,1])
      fitted = as.vector(model$model$fitted.values)
    } else if (class(model)[1] == "lm"){
      Y.t = as.vector(model$model[,1])
      fitted = as.vector(model$fitted.values)
    } else {
      stop("GMRAE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send one of these objects to the function.")
    }
    n = length(fitted)
    lag.Y.t <- Y.t
    lag.Y.t[2:n] <- Y.t[1:(n-1)]
    if (sum((Y.t[2:n] - lag.Y.t[2:n] == 0)) > 0){
      GMRAE.value <- exp(mean( log( abs((Y.t[2:n] - fitted[2:n] + 1E-10) / (Y.t[2:n] - lag.Y.t[2:n] + 1E-10)) ) ) )
    } else{
      GMRAE.value <- exp(mean( log( abs((Y.t[2:n] - fitted[2:n]) / (Y.t[2:n] - lag.Y.t[2:n])) ) ) )
    }
    GMRAE = data.frame( GMRAE =  GMRAE.value )
    colnames(GMRAE) = c("GMRAE")
    Call <- match.call()
    row.names(GMRAE) = as.character(Call[-1L])
    GMRAE
  }
  
}
  #' @export
  MBRAE <- function(model, ... ){
    
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
          stop("MBRAE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send a bunch of model objects to the function.")
        }
        n <- length(fitted)
        lag.Y.t <- Y.t
        lag.Y.t[2:n] <- Y.t[1:(n-1)]
        if (sum((Y.t[2:n] - lag.Y.t[2:n] == 0)) > 0){
          MBRAE.value <- mean( (Y.t[2:n] - fitted[2:n] + 1E-10) / ( (Y.t[2:n] - fitted[2:n] + 1E-10) + (Y.t[2:n] - lag.Y.t[2:n] + 1E-10) ), na.rm = TRUE ) 
        } else{
          MBRAE.value <- mean( (Y.t[2:n] - fitted[2:n]) / ( (Y.t[2:n] - fitted[2:n]) + (Y.t[2:n] - lag.Y.t[2:n]) ), na.rm = TRUE ) 
        }
        if (j == 1){
          MBRAE = data.frame( n = n , MBRAE = MBRAE.value, UMBRAE = MBRAE.value / (1 - MBRAE.value) )
          colnames(MBRAE) = c("n" , "MBRAE", "UMBRAE")
        } else {
          MBRAE = rbind(MBRAE, c(n , MBRAE.value , MBRAE.value / (1 - MBRAE.value) ) )
        }
      }
      Call <- match.call()
      row.names(MBRAE) = as.character(Call[-1L])
      MBRAE
    } else { # Only one model
      if ((class(model)[1] == "polyDlm") | (class(model)[1] == "dlm") | (class(model)[1] == "koyckDlm") | (class(model)[1] == "ardlDlm")){
        Y.t = as.vector(model$model$model[,1])
        fitted = as.vector(model$model$fitted.values)
      } else if (class(model)[1] == "lm"){
        Y.t = as.vector(model$model[,1])
        fitted = as.vector(model$fitted.values)
      } else {
        stop("MBRAE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send one of these objects to the function.")
      }
      n = length(fitted)
      lag.Y.t <- Y.t
      lag.Y.t[2:n] <- Y.t[1:(n-1)]
      if (sum((Y.t[2:n] - lag.Y.t[2:n] == 0)) > 0){
        MBRAE.value <- mean( (Y.t[2:n] - fitted[2:n] + 1E-10) / ( (Y.t[2:n] - fitted[2:n] + 1E-10) + (Y.t[2:n] - lag.Y.t[2:n] + 1E-10) ), na.rm = TRUE ) 
      } else{
        MBRAE.value <- mean( (Y.t[2:n] - fitted[2:n]) / ( (Y.t[2:n] - fitted[2:n]) + (Y.t[2:n] - lag.Y.t[2:n]) ), na.rm = TRUE ) 
      }
      MBRAE = data.frame( MBRAE =  MBRAE.value, UMBRAE = MBRAE.value / (1 - MBRAE.value)  )
      colnames(MBRAE) = c("MBRAE", "UMBRAE")
      Call <- match.call()
      row.names(MBRAE) = as.character(Call[-1L])
      MBRAE
    }
  
}