koyckDlm.default <-
  function(x , y ,  show.summary = TRUE){
    
    options(warn=-1)
    
    if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
      stop("Data series x and y both must be in a vector format!")
    }
  
    res = koyckDlm.main(x , y ,  show.summary)
    res$call = match.call()
    class(res) = c("koyckDlm" , "dLagM")
    res
    
  }