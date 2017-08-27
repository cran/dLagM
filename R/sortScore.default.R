sortScore.default <- 
  function(x, score = c("bic", "aic") ){
    
    options(warn=-1)
    
    res = sortScore.main(x, score)
    res$call = match.call()
    class(res) = c("sortScore" , "dLagM")
    res
    
  }