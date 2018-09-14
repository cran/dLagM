#' @export
ardlDlm.default <-
  function( formula = NULL , data = NULL , x = NULL, y = NULL , p = 1 , q = 1 , remove = NULL ){
    
    options(warn=-1)
    remove.p = remove$p
    remove.q = remove$q
    q = round(q)
    if (q < 0){
      stop("Lag order q must be a positive integer!")
    }
    p = round(p)
    if (p < 0){
      stop("Lag order p must be a positive integer!")
    }
    
    if ((is.null(formula) == TRUE) | (is.null(data) == TRUE)){
      if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
        stop("Data series x and y both must be in a vector format!")
      }
      if (is.null(remove.p) == FALSE){
        if (is.list(remove.p) == TRUE){
          stop("You must enter a vector or scalar to remove autoregressive parameters since you have only one independent series!")
        }
        if (length(remove.p) > p){
          stop("The number of autoregressive lags you want to remove is greater than the lag length!")
        }
      }
      if (is.null(remove.q) == FALSE){
        if (is.matrix(remove.q) == TRUE){
          stop("You must enter a vector or scalar for lags to remove lags from the dependent series!")
        }
        if (length(remove.q) > q){
          stop("The number of lags you want to remove is greater than the lag length!")
        }
      }
      res = ardlDlm.main( x = x , y = y , p = p , q = q , 
                          remove = remove , type = 1)
      
    } else if ((is.null(formula) == FALSE) & (is.null(data) == FALSE)){
      if (is.formula(formula)== FALSE){
        stop("You must enter a model object since you do not specify dependent and independent series!")
      }
      if (is.data.frame(data)== FALSE){
        stop("Data must be entered as a data.frame object!")
      }
      
      if (is.null(remove.p) == FALSE){
        if (is.list(remove.p) == FALSE){
          stop("You must enter a list to remove autoregressive parameters!")
        }
        if (length(remove.p) > p){
          stop("The number of autoregressive lags you want to remove is greater than the lag length!")
        }
      }
      if (is.null(remove.q) == FALSE){
        if (is.matrix(remove.q) == TRUE){
          stop("You must enter a vector or scalar to remove lags from the dependent series!")
        }
        if (length(remove.q) > q){
          stop("The number of lags you want to remove is greater than the lag length!")
        }
      }
      
      res = ardlDlm.main( formula = formula , data = data , p = p , q = q , 
                          remove = remove , type = 2)
      
    } else if ((is.null(formula) == TRUE) & (is.null(data) == FALSE)){
      if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
        stop("You must enter dependent and independent series since the data argument is missing!")
      }
    } else if ((is.null(formula) == FALSE) & (is.null(data) == TRUE)){
      if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
        stop("You must enter dependent and independent series since the formula argument is missing!")
      }
    }
    
    res$call = match.call()
    class(res) = c("ardlDlm" , "dLagM")
    res
    
  }