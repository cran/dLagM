#' @import stats
#' @import graphics
#' @importFrom lmtest dwtest
#' @importFrom lmtest bptest
#' @importFrom lmtest waldtest
#' @importFrom strucchange efp
#' @importFrom grDevices dev.new
#' @importFrom grDevices graphics.off
#' @export
ardlBound <- function(data = NULL , formula = NULL , case = 3 , p = NULL , k = NULL , autoOrder = FALSE , ic = c("AIC" , "BIC") , 
                      max.p = 15,  max.q = 15, ECM = TRUE, stability = TRUE){
  if (is.null(data)) stop("Enter data by data argument.")
  if (is.null(formula)) stop("A formula object showing the dependent and indepdenent series must be entered.")
  
  vars <- all.vars(formula)
  NumVar <- length(vars)
  
  if (is.null(p) | (autoOrder == TRUE) ){
    cat("Orders being calculated with max.p =", max.p , "and max.q =", max.q, "...\n\n")
    orders <- ardlBoundOrders(data = data , formula = formula, ic = ic , max.p = max.p,  max.q = max.q )
    cat("Autoregressive order:" , orders$q + 1, "and p-orders:" , unlist(orders$p) + 1 , "\n")
    cat("------------------------------------------------------", "\n")
    p <- data.frame(orders$q , orders$p ) + 1 
   
  }
  if (!is.data.frame(p)){
    p <- array(p , NumVar)
    p <- data.frame(t(p))
    colnames(p) <- vars
  } else {
    colnames(p) <- vars
    if (length(p) < NumVar) stop("Enter an integer or and array of lenght equal to the number of variables in the formula!")
  }
  
  caseType <- switch(case, "no intercept, no trend", "restricted intercert, no trend (not supported)", 
                           "unrestricted intercert, no trend", "unrestricted intercept, restricted trend (not supported)", 
                           "unrestricted intercept, unrestricted trend")
  
  diffData <- apply(data , 2 , diff)
  colnames(diffData) <- paste0("d" , colnames(diffData))
  
  data <- cbind(data[2:nrow(data),],diffData)
  
  max.p <- max(p[2:NumVar])  
  rem.p <- list()
  for (i in 1:NumVar){
    if (max.p > 2){ 
      rem.p[[vars[i] ]] <- c(0,2:(max.p-1))  
    }else {
      rem.p[[vars[i] ]] <- c(0)
    }
  }
  
  if (sum((p[2:NumVar] - max.p) == 0) == (NumVar-1)){
    removeP <- NULL
    if (ECM == TRUE) {
      remP <- list()
      if ((max.p-1) >= 2){
        remP[["ec"]] <- c(0, 2:(max.p-1))
      } else if ((max.p-1) == 1){
        remP[["ec"]] <- c(0)
      }
      removeP = list(p = remP )  
    }
  } else {
    remP <- list()
    reduce <- which((p[2:NumVar] - max.p) != 0) + 1 
    for ( j in reduce){
      remP[[paste0("d" ,vars[j])]] <- c((max.p-1):p[[j]])
      rem.p[[paste0("d" ,vars[j])]] <- c((max.p-1):p[[j]])
    }
    if (ECM == TRUE) {
      if ((max.p-1) >= 2){
        remP[["ec"]] <- c(0, 2:(max.p-1))
      } else if ((max.p-1) == 1){
        remP[["ec"]] <- c(0)
      }
    }
    # if (((max.p-1) >= 2) & (ECM == TRUE)) remP[["ec"]] <- c(0, 2:(max.p-1))
    removeP = list(p = remP )  
  }
  
  formula1 <- as.formula(paste0("d" , vars[1] , " ~ " , paste(c(vars , 
                                                                paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))   
  if (case == 1){
    formula1 <- update(formula1, ~. -1) # Case 1 requires this
    formula2 <- as.formula(paste0("d" , vars[1] , " ~ - 1 + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = list(p = rem.p )) 
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[2:NumVar] / modelFull$model$coefficients[1])
      ec <- data[,1] - as.vector(mult %*% t(data[,2:NumVar]))
      data.ecm <- data.frame(cbind(data, ec))
      formula3 <- as.formula(paste0("d" , vars[1] , " ~ - 1 + ec + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
      modelECM <- ardlDlm(formula = formula3, data = data.ecm , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
      ecm.beta <- modelECM$model$coefficients["ec.1"]
    }
  } else if (case == 2){
    formula1 <- update(formula1, ~. +1) 
    formula2 <- as.formula(paste0("d" , vars[1] , " ~ - 1 + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = list(p = rem.p )) 
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[3:(NumVar+1)] / modelFull$model$coefficients[2])
      ec <- data[,1] - as.vector(mult %*% t(data[,2:NumVar])) - modelFull$model$coefficients[1] / modelFull$model$coefficients[2]
      data.ecm <- data.frame(cbind(data, ec))
      formula3 <- as.formula(paste0("d" , vars[1] , " ~ - 1 + ec + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
      modelECM <- ardlDlm(formula = formula3, data = data.ecm , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
      ecm.beta <- modelECM$model$coefficients["ec.1"]
    }
  } else if (case == 3){
    formula2 <- as.formula(paste0("d" , vars[1] , " ~ +1 + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = list(p = rem.p )) 
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[3:(NumVar+1)] / modelFull$model$coefficients[2])
      ec <- data[,1] - as.vector(mult %*% t(data[,2:NumVar]))
      data.ecm <- data.frame(cbind(data, ec))
      formula3 <- as.formula(paste0("d" , vars[1] , " ~ +1 + ec + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
      modelECM <- ardlDlm(formula = formula3, data = data.ecm , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
      ecm.beta <- modelECM$model$coefficients["ec.1"]
    }
  } else if (case == 4){  
    trend <- c(1:nrow(data))
    data <- cbind(data , trend)
    formula1 <- as.formula(paste0(formula1 , " + trend")) 
    formula2 <- as.formula(paste0("d" , vars[1] , " ~ " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
    rem.p[["trend"]] <- c(1:(max.p-1))
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = list(p = rem.p )) 
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[3:(NumVar+1)] / modelFull$model$coefficients[2])
      ec <- data[,1] - as.vector(mult %*% t(data[,2:NumVar])) - modelFull$model$coefficients["trend.t"] * trend / modelFull$model$coefficients[2]
      data.ecm <- data.frame(cbind(data, ec))
      formula3 <- as.formula(paste0("d" , vars[1] , " ~ ec + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
      modelECM <- ardlDlm(formula = formula3, data = data.ecm , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP )
      ecm.beta <- modelECM$model$coefficients["ec.1"]
    }
  } else if (case == 5){
    trend <- c(1:nrow(data))
    data <- cbind(data , trend)
    formula1 <- as.formula(paste0(formula1 , " + trend")) 
    formula2 <- as.formula(paste0("d" , vars[1] , " ~ " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") , " + trend" ))
    removeP$p[["trend"]] <- c(1:(max.p-1))
    rem.p[["trend"]] <- c(1:(max.p-1))
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = list(p = rem.p )) 
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[3:(NumVar+1)] / modelFull$model$coefficients[2])
      ec <- data[,1] - as.vector(mult %*% t(data[,2:NumVar]))
      data.ecm <- data.frame(cbind(data, ec))
      formula3 <- as.formula(paste0("d" , vars[1] , " ~ ec + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") , " + trend" ))
      modelECM <- ardlDlm(formula = formula3, data = data.ecm , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
      print(removeP)
      ecm.beta <- modelECM$model$coefficients["ec.1"]
    }
  }

  # modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = list(p = rem.p )) 
  
  bg <- NULL
  bg <- lmtest::bgtest(modelFull$model)
  cat("\n Breusch-Godfrey Test for the autocorrelation in residuals:\n")
  print(bg)
  if (bg$p.value < 0.05){
    cat("The p-value of Breusch-Godfrey test for the autocorrelation in residuals: ", bg$p.value , "< 0.05!\n" )
  }
  cat("------------------------------------------------------", "\n")
  
  res <- modelFull$model$residuals
  cat("\n Ljung-Box Test for the autocorrelation in residuals:\n")
  lb <- Box.test(res, type = c("Ljung-Box"))
  print(lb)
  if (lb$p.value < 0.05){
    cat("The p-value of Ljung-Box test for the autocorrelation in residuals: ", lb$p.value , "< 0.05!\n" )
  }
  cat("------------------------------------------------------", "\n")
  
  bp <- NULL
  bp <- lmtest::bptest(modelFull$model) 
  cat("\n Breusch-Pagan Test for the homoskedasticity of residuals:\n")
  print(bp)
  if (bp$p.value < 0.05){
    cat("the p-value of Breusch-Pagan test for the homoskedasticity of residuals: ", bp$p.value , "< 0.05!\n" )
    # stop("Bounds test procedure is stopped due to significant heteroskedasticity in residuals.")
  }
  cat("------------------------------------------------------", "\n")

  Fvalue <- lmtest::waldtest( modelNull$model , modelFull$model)$F[2]
  
  if (is.null(k)) k = (NumVar - 1)
  
  pssbounds(obs = nrow(data) , fstat = Fvalue , case = case, k = k )
  
  if ((stability == TRUE) & (ECM == TRUE)){
    ff = list(x = as.matrix(model.matrix(terms(modelECM$model), model.frame(modelECM$model)))
                        , y = as.vector(model.response(model.frame(modelECM$model))))
    cusum.test <- efp(formula = ff, data = ts(data.ecm), type = "Rec-CUSUM")
    mosum.test <- efp(formula = ff, data = ts(data.ecm), type = "Rec-MOSUM")
    graphics.off()
    plot.new()
    par(oma = c(4, 1, 1, 1))
    par(mfrow = c(1,2))
    if (!any(is.nan(cusum.test$process))){
      plot(cusum.test)
    }
    if(!any(is.nan(mosum.test$process))){
      plot(mosum.test)
    }
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend("bottom", c("Recursive residuals", "5% limits"), xpd = TRUE, horiz = TRUE, inset = 0, bty = "n", 
           col = c("black" , "red"), lwd=2, cex = 1)
  }
  
  if (ECM){
    cat("------------------------------------------------------", "\n")
    cat("Error Correction Model Output: \n")
    summary(modelECM)
  }
  if (ECM){
    return(list(model = list(modelNull = modelNull, modelFull = modelFull), F.stat = Fvalue , p = p , q = q, k = k, bg = bg, lb = lb , bp = bp,
             ECM = list(EC.t = ec, EC.model = modelECM$model , EC.beta = ecm.beta) ) )  
  } else {
    return(list(model = list(modelNull = modelNull, modelFull = modelFull), F.stat = Fvalue , p = p , q = q, k = k, bg = bg, lb = lb , bp = bp ) )  
  }
  
}
