finiteDLMauto.main <- function(x, y, q.min, q.max, k.order, model.type, error.type, trace){
  #set parameter value
  
  if(is.null(q.min)){
    start = 1
  }else{
    start = q.min
  }
  if(is.null(q.max)){
    q.max = 10
  }
  
  et = tolower(error.type)
  mt = tolower(model.type)
  
  #set data frame to store the masurement value
  df = data.frame(1,2,3,4,5,6)
  
  if(mt == "dlm"){
    if (abs((q.max + 1) - length(x)) > 10){
      end = q.max
    } else{
      end = length(x) - 11
    }
    names(df)[1]<-paste('q')
    for(i in start:end){
      model = dLagM::dlm(x = as.vector(x), y = as.vector(y), q = i, show.summary = FALSE)
      df[i-start+1,1] = i
      df[i-start+1,2] = round(MASE(model$model),5)
      df[i-start+1,3] = round(AIC(model$model),5)
      df[i-start+1,4] = round(BIC(model$model),5)
      df[i-start+1,5] = round(summary(model$model)$adj.r.squared,5)
      df[i-start+1,6] = Box.test(model$model$residuals,type = "Ljung-Box")$p.value
    }
  }else if(mt == "poly"){
    if(is.null(k.order) || k.order == 0){
      k.order = 2
    }else{
      k.order = k.order
    }
    end = q.max
    names(df)[1]<-paste('q - k')
    for(i in start:end){
      model = dLagM::polyDlm(x = as.vector(x), y = as.vector(y), q = i, k = k.order, show.summary = FALSE, show.beta = FALSE)
      df[i-start+1,1] = paste(i,k.order,sep = ' - ')
      df[i-start+1,2] = round(MASE(model$model),5)
      df[i-start+1,3] = round(AIC(model$model),5)
      df[i-start+1,4] = round(BIC(model$model),5)
      df[i-start+1,5] = round(summary(model$model)$adj.r.squared,5)
      df[i-start+1,6] = Box.test(model$model$residuals,type = "Ljung-Box")$p.value
    }
  }else{
    print("Model Type is not correctly specified or mistaken typed.")
  }
  
  #set the name for df variable:
  names(df)[2]<-paste('MASE')
  names(df)[3]<-paste('AIC')
  names(df)[4]<-paste('BIC')
  names(df)[5]<-paste('R.Adj.Sq')
  names(df)[6]<-paste('Ljung-Box')
  #specify the model and get the measurement value
  
  if(et == 'mase'){
    df.ordered = df[order(df[,2]),]
  }else if(et == 'aic'){
    df.ordered = df[order(df[,3]),]
  }else if(et == 'bic'){
    df.ordered = df[order(df[,4]),]
  }else if(et == 'radj'){
    df.ordered = df[order(df[,5],decreasing = T),]
  }else{
    print("Method sepecified default: MASE, AIC, BIC, and Radj.")
  }
  
  if(trace == FALSE){
    return(df.ordered[1,])
  }else{
    return(df.ordered)
  }
  
}

