sortScore.default <- 
  function(x, score = c("bic", "aic", "mase") ){
    
    options(warn=-1)
    
    res = sortScore.main(x = x, score = score)
    res$call = match.call()
    class(res) = c("sortScore" , "dLagM")
    res
    
  }