#' @export
forecast.koyckDlm <-
  function( model , x , h = 1 , interval = FALSE, level = 0.95 , nSim = 500 ){
    
    options(warn=-1)
    h <- round(h)
    alpha <- 1 - level
    if (h < 0){
      stop("The number of forecasts must be a positive integer!")
    }
    if (length(x) < h){
      stop("The number of new observations must be greater than or equal to the value of h!")
    }
    if (interval == TRUE){
      if ((level < 0.0001) | (level > 0.9999)){
        stop("Confidence level must be in betweeen 0 and 1!")
      }
      if ((floor(nSim*alpha)<=1) | (ceiling(nSim*alpha)>= (nSim-2))){
        stop("The value of level is not suitable for computations!")
      }
    }
    if (interval == FALSE){
      res <- koyckDlmForecast.main( model , x , h )
    } else {
      CI <- data.frame(array(NA, dim = c(nSim , h) ))
      for ( i in 1:nSim){
        eps <- rnorm(h , 0 , sqrt(sum(residuals(model$model)^2, na.rm = TRUE)/df.residual(model$model)))
        CI[ i , ] <- koyckDlmForecast.main(model = model , x = x , h = h, epsilon = eps)$forecasts
      }
      
      limits <- matrix(NA, nrow = h , ncol = 2)
      for (j in 1:h){
        limits[j , ] <- quantile(CI[ , j],type=8,prob=c(alpha/2,(1-alpha/2)))
      }
      frc <- koyckDlmForecast.main(model = model , x = x , h = h , epsilon = NULL)  
      forecasts <- data.frame(limits[ , 1], frc , limits[ , 2])
      colnames(forecasts) <- c("Lower","Estimate","Upper")
      
      if ((sum(forecasts[, 1 ] < forecasts[ , 2 ]) != nrow(forecasts)) | (sum(forecasts[, 3 ] > forecasts[ , 2 ]) != nrow(forecasts)) ){
        stop("Monte Carlo approach used to find confidence intervals produced inappropriate resutls please increase
             the number of simulattions and run the function again.")
      }
      res <- list(forecasts = forecasts)
      }
    
    res$call <- match.call()
    class(res) <- c("forecast.koyckDlm" , "dLagM")
    res
    
  }