finiteDLMauto.default <- function(formula = NULL , data = NULL, x = NULL, y =NULL, q.min = 1 , q.max = 10, 
                                  k.order = NULL , model.type = c("dlm","poly") , error.type = c("MASE","AIC","BIC","radj") , trace = FALSE){
  if (is.null(model.type) == TRUE){
    model.type = "dlm"
  }
  
  if (is.null(error.type) == TRUE){
    model.type = "AIC"
  }
  
  if ((is.null(formula) == TRUE) | (is.null(data) == TRUE)){
    if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
      stop("Data series x and y both must be entered in a vector format!")
    }
    res = finiteDLMauto.main(formula = NULL, data = NULL, x = x, y = y, q.min = q.min, q.max = q.max, k.order = k.order, 
                             model.type = model.type , error.type = error.type, trace = trace, type = 1)
  } else if ((is.null(formula) == FALSE) & (is.null(data) == FALSE)){
    if (plyr::is.formula(formula)== FALSE){
      stop("You must enter a formula object since you do not specify dependent and independent series!")
    }
    if (is.data.frame(data)== FALSE){
      stop("Data must be entered as a data.frame object!")
    }
    
    if (model.type == "poly"){
      stop("Please use the arguments x and y for polynomial DLMs!")
    }
    res = finiteDLMauto.main(formula = formula, data = data, x = NULL, y = NULL, q.min = q.min, q.max = q.max, k.order = k.order, 
                             model.type = model.type , error.type = error.type, trace = trace, type = 2)
  }
  # res$call = match.call()
  # class(res) = c("finiteDLMauto" , "dLagM")
  res

}

