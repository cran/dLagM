dlm.default <-
  function(x , y , q , show.summary = TRUE){
    
    options(warn=-1)
    
    if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
      stop("Data series x and y both must be in a vector format!")
    }
    q = round(q)
    if (q < 0){
      stop("Lag order must be a positive integer!")
    }
    res = dlm.main(x , y , q , show.summary)
    res$call = match.call()
    class(res) = c("dlm" , "dLagM")
    res
    
  }