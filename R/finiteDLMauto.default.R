finiteDLMauto.default <- function(x, y, q.min = NULL, q.max = NULL, k.order = NULL, model.type = c("dlm","poly"), error.type = c("MASE","AIC","BIC","radj"), trace = FALSE){
  
  if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
    stop("Data series x and y both must be in a vector format!")
  }
  
  if (is.null(model.type) == TRUE){
    model.type = "dlm"
  }
  
  if (is.null(error.type) == TRUE){
    model.type = "AIC"
  }
  
  res = finiteDLMauto.main(x = x, y = y, q.min = q.min, q.max = q.max, k.order = k.order, 
                           model.type = model.type , error.type = error.type, trace = trace)
  # res$call = match.call()
  # class(res) = c("finiteDLMauto" , "dLagM")
  res

}

