#' @importFrom MASS mvrnorm
#' @importFrom roll roll_cor
#' @importFrom grDevices rgb
#' @export
sdPercentiles <- function(n = 150, cor = 0.5, width = 5, N = 500, percentiles = c(.05, .95)){
  # library(roll)
  # library('MASS')
  x <- array(NA, dim = c(n,2,N))
  rollCorSd <- array(NA, N)
  for (i in 1:N){
    x <- mvrnorm(n=100, mu=c(0, 0), Sigma=matrix(c(1, cor, cor, 1), nrow=2), empirical=TRUE)
    rollCorSd[i] <- sqrt(var(na.omit(roll_cor(x=as.matrix(x[,1]) , y =as.matrix(x[,2]),  width = width)[1,,])))
  }
  rollCorSd.limits <- quantile(rollCorSd, percentiles)                   
  return(rollCorSd.limits)
}