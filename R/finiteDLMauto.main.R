finiteDLMauto.main <- function(formula, data, x, y, q.min, q.max, k.order, model.type, error.type, trace, type){
  #set parameter value
  

  et <- tolower(error.type)
  mt <- tolower(model.type)
 
  results <- data.frame(array(NA, dim = c((q.max - q.min + 1), 8))) # array(NA, dim = c((q.max - q.min + 1), 6))#data.frame(1,2,3,4,5,6)
  if (type == 1){
    n <- length(x)
  } else if (type == 2){
    n <- nrow(data)
  }
  if(mt == "dlm"){
    for(i in q.min:q.max){
      if (type == 1){
        model <- dLagM::dlm(x = as.vector(x), y = as.vector(y), q = i)
      } else if (type == 2){
        model <- dLagM::dlm(formula = formula, data = data, q = i)
      }
      results[(i-q.min+1), 1] <- i
      results[(i-q.min+1), 2] <- round(MASE(model$model),5)
      results[(i-q.min+1), 3] <- round(AIC(model$model),5)
      results[(i-q.min+1), 4] <- round(BIC(model$model),5)
      results[(i-q.min+1), 5] <- round(GMRAE(model$model),5)
      results[(i-q.min+1), 6] <- round(MBRAE(model$model),5)
      results[(i-q.min+1), 7] <- round(summary(model$model)$adj.r.squared,5)
      results[(i-q.min+1), 8] <- stats::Box.test(model$model$residuals,type = "Ljung-Box")$p.value
    }
  }else if(mt == "poly"){
    if(is.null(k.order) || k.order == 0){
      k.order <- 2
    }else{
      k.order <- k.order
    }
    for(i in q.min:q.max){
      model <- dLagM::polyDlm(x = as.vector(x), y = as.vector(y), q = i, k = k.order, show.beta = FALSE)
      results[(i-q.min+1), 1] <- paste0(i," - ", k.order)
      results[(i-q.min+1), 2] <- round(MASE(model$model),5)
      results[(i-q.min+1), 3] <- round(AIC(model$model),5)
      results[(i-q.min+1), 4] <- round(BIC(model$model),5)
      results[(i-q.min+1), 5] <- round(GMRAE(model$model),5)
      results[(i-q.min+1), 6] <- round(MBRAE(model$model),5)
      results[(i-q.min+1), 7] <- round(summary(model$model)$adj.r.squared,5)
      results[(i-q.min+1), 8] <- stats::Box.test(model$model$residuals,type = "Ljung-Box")$p.value
    }
  }else{
    print("Model type is not correctly specified.")
  }
  results <- as.data.frame(results)
  colnames(results) <- c("q - k", "MASE", "AIC" , "BIC" , "GMRAE" , "MBRAE" , "R.Adj.Sq" , "Ljung-Box")
  
  #set the name for results variable:
  # names(results)[2]<-paste('MASE')
  # names(results)[3]<-paste('AIC')
  # names(results)[4]<-paste('BIC')
  # names(results)[5]<-paste('R.Adj.Sq')
  # names(results)[6]<-paste('Ljung-Box')
  #specify the model and get the measurement value
  
  if(et == 'mase'){
    results.ordered <- results[order(results[,2]),]
  }else if(et == 'aic'){
    results.ordered <- results[order(results[,3]),]
  }else if(et == 'bic'){
    results.ordered <- results[order(results[,4]),]
  }else if(et == 'gmrae'){
    results.ordered <- results[order(results[,5]),]
  }else if(et == 'mbrae'){
    results.ordered <- results[order(results[,6]),]
  }else if(et == 'radj'){
    results.ordered <- results[order(results[,7],decreasing = T),]
  }else{
    print("Method is sepecified by default: MASE, AIC, BIC, GMRAE, MBRAE, and Radj.")
  }
  
  if(trace == FALSE){
    return(results.ordered[1,])
  }else{
    return(results.ordered)
  }
  
}

