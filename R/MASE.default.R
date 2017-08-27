MASE.default <-
  function( observed , fitted ){
    
    options(warn=-1)
    
    if (length(observed) < 2){
      stop("The number of observations must be greater than two!")
    }
    
    if (length(fitted) < 2){
      stop("The number of fitted values must be greater than two!")
    }
  
    if (is.numeric(observed) == FALSE){
      stop("Observations must be composed of numeric values!")
    }
    
    if (is.numeric(fitted) == FALSE){
      stop("Fitted values must be composed of numeric values!")
    }
    
    res = MASE.main(observed , fitted )
    res$call = match.call()
    class(res) = c("MASE" , "dLagM")
    res
    
  }