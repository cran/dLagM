ardlDlm.default <-
  function( x , y , p = 1 , q = 1 , show.summary = TRUE){
    
    options(warn=-1)
    
    if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
      stop("Data series x and y both must be in a vector format!")
    }
    q = round(q)
    if (q < 0){
      stop("Lag order q must be a positive integer!")
    }
    p = round(p)
    if (p < 0){
      stop("Lag order p must be a positive integer!")
    }
    res = ardlDlm.main( x , y , p , q , show.summary )
    res$call = match.call()
    class(res) = c("ardlDlm" , "dLagM")
    res
    
  }