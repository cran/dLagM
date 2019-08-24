#' @importFrom roll roll_cor
#' @export
rolCorPlot <- function(x , y , width, start = 1, level = 0.95, main = NULL, SDtest = TRUE, N = 500){
  rolCor <- matrix(NA, nrow = length(x), ncol = length(width))
  SDrolCor <- array(NA, length(width))
  for ( i in 1:length(width)){
    rolCor[ , i] <- roll_cor(x=as.matrix(x, ncol=1) , y =as.matrix(y, ncol=1),  width = width[i])[1,,]
    SDrolCor[i] <- sqrt(var(rolCor[ , i] , na.rm = TRUE ))
    if ( i == 1){
      plot(ts(rolCor[ , i], start = start), ylim=c(min(rolCor[ , i] , na.rm = TRUE) - 0.2 , max(rolCor[ , i] , na.rm = TRUE) + 0.05),
           lty = 1, main = main, ylab = "Rolling correlation")
    } else {
      lines(ts(rolCor[ , i], start = start), lty = 1+i)
    }
  }
  
  legend("bottom",xpd = TRUE, horiz = TRUE, inset=c(0,0),bty = "n", lty=c(seq(2,length(width)+1,1)), legend = width, title="Width", cex = .8)
  
  rolcCor_avr_org <- apply(rolCor,1,mean, na.rm = TRUE)
  rolcCor_avr <- rolcCor_avr_org
  rolcCor_avr[which(is.na(rolcCor_avr_org) == FALSE)] <- runmed(na.omit(rolcCor_avr_org), k = 3) # Running median nonlinear filter with minimal robust smoothing eliminating isolated outliers
  lines(ts(rolcCor_avr, start = start),lty=1,lwd=1.5)
  abline(h = mean(rolcCor_avr, na.rm = TRUE))
  abline(h = t.test(rolcCor_avr, conf.level = level, na.rm = TRUE)$conf.int, lty=2, col = "red")
  cor.xy <- cor(x,y)
  sdPer <- array(NA, dim = c(length(width), 2))
  for ( i in 1:length(width)){
    sdPer[i,] <- sdPercentiles(n=length(x) , cor = cor.xy , width = width[i], percentiles = c(level, 1-level), N = N)
  }
  sdPerComp <- data.frame(SDrolCor, sdPer)
  colnames(sdPerComp) <- c("SDrolCor", paste0(level, "%"), paste0(1-level, "%"))
  if (SDtest){ print(SDtest) }
  return(list(rolCor = rolCor , rolcCor.avr.filtered = rolcCor_avr, rolcCor.avr.raw = rolcCor_avr_org, 
              rolCor.sd = SDrolCor, rawCor = cor.xy, sdPercentiles = sdPer, test = sdPerComp ))
}