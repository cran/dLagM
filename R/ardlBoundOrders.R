#' @export
ardlBoundOrders <- function(data = NULL , formula = NULL, ic = c("AIC", "BIC"), max.p = 15,  max.q = 15  ){
  if (is.null(data)) stop("Enter data by data argument.")
  if (is.null(formula)) stop("A formula object showing the dependent and indepdenent series must be entered.")
  
  vars <- all.vars(formula)
  NumVar <- length(vars)
  
  diffData <- apply(data , 2 , diff)
  colnames(diffData) <- paste0("d" , colnames(diffData))
  
  data <- cbind(data[2:nrow(data),],diffData)
  
  formula1 <- as.formula(paste0("d" , vars[1] , " ~ " , paste(c(vars , 
                                                                paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))   
  
  # Find q and p that is equal across all the variables
  run.p <- TRUE
  p <- 1
  crit <- matrix(NA, max.p, max.q)
  while ((run.p) & (p <= max.p)){
    rem.p <- list()
    for (i in 1:NumVar){
      if (p > 2){ 
        rem.p[[vars[i] ]] <- c(0,2:(p-1))  
      }else {
        rem.p[[vars[i] ]] <- c(0)
      }
    }
    q <- 1
    run.q <- TRUE
    while ((run.q) & (q <= max.q)){
      modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = p , q = q, remove = list(p = rem.p ))  
      IC <- sum(ic == "AIC") * AIC(modelFull$model) + (1 - sum(ic == "AIC") ) * BIC(modelFull$model)
      if ( !is.nan(IC) ){
        crit[p,q] <- IC
        q <- q + 1
      } else {
        run.q <- FALSE
        if ( (p == 1) & (q == 1) ) run.p <- FALSE
      }
    }
    p <- p + 1
  }
  
  crit.pq <- data.frame(crit[rowSums(is.na(crit))<nrow(crit),])
  colnames(crit.pq) <- paste0("q = " , c(1:ncol(crit.pq)))
  rownames(crit.pq) <- paste0("p = " , c(1:nrow(crit.pq)))
  
  min.IC <- which(crit == min(crit, na.rm = TRUE), arr.ind = TRUE)
  p <- min.IC[1]
  q <- min.IC[2]

  # Find a different p for each of the variables
  if ((p > 1) & (NumVar>2)){
    
    combs <- data.frame(1:p)
    for ( i in 2:(NumVar - 1)){
      combs <- data.frame(combs, 1:p)
    }
    combs <- expand.grid(combs)
    colnames(combs) <- vars[2:NumVar]
    
    crit <- array(NA, nrow(combs)-1)
    for ( i in 1:(nrow(combs)-1)){
      max.p <- max(combs[i,])
      rem.p <- list()
      for (j in 1:NumVar){
        if (max.p >= 2){ 
          rem.p[[vars[j] ]] <- c(0,2:(max.p))  
        }else {
          rem.p[[vars[j] ]] <- c(0)
        }
      }
      remP <- list()
      if (sum((combs[i,] - max.p) == 0) != (NumVar - 1)){
        reduce <- which((combs[i,] - max.p) != 0) 
        for ( j in reduce){
          remP[[paste0("d" ,vars[j + 1])]] <- c((combs[i,j] + 1):max.p)
        }
      }
      # print(combs[i,])
      # print(c(rem.p , remP))
      modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = max.p , q = q , remove = list(p = c(rem.p , remP) ))  
      crit[i] <- sum(ic == "AIC") * AIC(modelFull$model) + (1 - sum(ic == "AIC") ) * BIC(modelFull$model)
      # print(AIC(modelFull$model))
    }
    combs.p <- data.frame(combs[1:(nrow(combs)-1),],crit)
    colnames(combs.p)[ncol(combs.p)] <- "IC"
    p <- data.frame(combs[which(crit == min(crit) , arr.ind = TRUE), ])
    colnames(p) <- vars[2:NumVar] 
    return( list(p = p , q = q , IC.table = crit.pq , min.IC = min(crit.pq , na.rm = TRUE) , IC.p = combs.p[order(combs.p$IC), ]) )
  } else {
    p <- as.data.frame(t(rep(p , NumVar - 1)))
    colnames(p) <- vars[2:NumVar] 
    return( list(p = p , q = q , IC.table = crit.pq, min.IC = min(crit.pq , na.rm = TRUE)) )
  }
}



# run.p <- TRUE
# p <- 1
# critOld <- 10000
# # aAIC <- array(NA, dim= c(max.p, max.q))
# # aBIC <- array(NA, dim= c(max.p, max.q))
# while ((run.p) & (p < max.p)){#for (p in 1:max.p){#
#   for (i in 1:NumVar){
#     if (p > 2){ 
#       rem.p[[vars[i] ]] <- c(0,2:(p-1))  
#     }else {
#       rem.p[[vars[i] ]] <- c(0)
#     }
#   }
#   run.q <- TRUE
#   q <- 1
#   while ((run.q) & (q < max.q)){#for ( q in 1:max.q){#
#     modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = p , q = q, remove = list(p = rem.p ))  
#     if (ic == "AIC"){
#       AICq <- AIC(modelFull$model)#aAIC[p,q] <- AIC(modelFull$model)
#       if ( (AICq < critOld) & !is.infinite(AICq) ){
#         critOld <- AIC(modelFull$model)
#         print(critOld)
#         save.q <- q
#         q <- q + 1
#       } else {
#         run.q <- FALSE
#         print(q)
#       }
#     } else if (ic == "BIC"){
#       BICq <- BIC(modelFull$model)#aBIC[p,q] <- BIC(modelFull$model)
#       if ( (BICq < critOld) & !is.infinite(BICq) ){
#         critOld <- BIC(modelFull$model)
#         save.q <- q
#         q <- q + 1
#       } else {
#         run.q <- FALSE
#       }
#     }
#   }
#   modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = p , q = save.q , remove = list(p = rem.p ))  
#   print(paste0("p part ", AIC(modelFull$model)))
#   if (ic == "AIC"){
#     AICp <- AIC(modelFull$model)#aAIC[p,q] <- AIC(modelFull$model)
#     if ( (AICp < critOld) & !is.infinite(AICp) ){
#       critOld <- AIC(modelFull$model)
#       save.p <- p
#       p <- p + 1
#     } else {
#       run.p <- FALSE
#     }
#   } else if (ic == "BIC"){
#     BICp <- BIC(modelFull$model) #aBIC[p,q] <- BIC(modelFull$model) 
#     if ( (BICp < critOld) & !is.infinite(BICp) ){
#       critOld <- BIC(modelFull$model)
#       save.p <- p
#       p <- p + 1
#     } else {
#       run.p <- FALSE
#     }
#   }
# }