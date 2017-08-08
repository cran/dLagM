dlmForecast.default <-  function(model , x , h = 1 ){
    options(warn=-1)
    h = round(h)
    if (h < 0){
      stop("The number of forecasts must be a positive integer!")
    }
    if (length(x) < h){
      stop("The number of new observations must be greater than or equal to the value of h!")
    }
    
    res = dlmForecast.main(model , x , h )
    res$call = match.call()
    class(res) = c("dlmForecast" , "dLagM")
    res
    
  }