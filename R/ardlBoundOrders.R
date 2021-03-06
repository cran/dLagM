#' @export
ardlBoundOrders <- function(data = NULL , formula = NULL, ic = c("AIC", "BIC", "MASE", "GMRAE"), max.p = 15,  max.q = 15, FullSearch = FALSE){
  if (is.null(data)) stop("Enter data by data argument.")
  if (is.null(formula)) stop("A formula object showing the dependent and indepdenent series must be entered.")
  vars <- all.vars(formula)
  NumVar <- length(vars)
  
  if ( NumVar < ncol(data)){ 
    data <- data[,vars]
  }
  
  if (NumVar == 2){FullSearch = FALSE}
  diffData <- apply(data , 2 , diff)
  colnames(diffData) <- paste0("d" , colnames(diffData))

  data <- cbind(data[2:nrow(data),],diffData)
  
  formula1 <- as.formula(paste0("d" , vars[1] , " ~ " , paste(c(vars , 
                                                                paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))   
  if (FullSearch == FALSE){
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
        if (ic == "AIC"){
          IC <- AIC(modelFull$model)
        } else if (ic == "BIC"){
          IC <- BIC(modelFull$model)
        } else if (ic == "MASE"){
          IC <- MASE(modelFull)$MASE
        } else if (ic == "GMRAE"){
          IC <- GMRAE(modelFull)$GMRAE
        } 
        if ( !is.nan(IC) & !is.infinite(IC) ){
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
    # if ((p > 1) & (NumVar>2)){
    if (p == 1) p <- max.p
    if ( (NumVar>2)){
     
      combs <- data.frame(0:p)
      for ( i in 2:(NumVar - 1)){
        combs <- data.frame(combs, 0:p)
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
        if (sum(combs[i,] == 0) == (NumVar - 1)){ # all zeros
          max.p <- 1
          for (j in 1:(NumVar-1)){
            remP[[paste0("d" ,vars[j + 1])]] <- 1
          }
        }
        modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = max.p , q = q , remove = list(p = c(rem.p , remP) ))  
        if (ic == "AIC"){
          crit[i] <- AIC(modelFull$model)
        } else if (ic == "BIC"){
          crit[i] <- BIC(modelFull$model)
        } else if (ic == "MASE"){
          crit[i] <- MASE(modelFull)$MASE
        } else if (ic == "GMRAE"){
          crit[i] <- GMRAE(modelFull)$GMRAE
        } 
      }
      # crit <- unlist(crit)
      combs.p <- data.frame(combs[1:(nrow(combs)-1),],crit)
      colnames(combs.p)[ncol(combs.p)] <- "Stat"
      p <- data.frame(combs[which(crit == min(crit, na.rm = TRUE) , arr.ind = TRUE), ])
      colnames(p) <- vars[2:NumVar] 
      
      return( list(p = p , q = q , Stat.table = crit.pq , min.Stat = min(crit , na.rm = TRUE) , Stat.p = combs.p[order(combs.p$Stat), ]) )
      
    } else {
      p <- as.data.frame(t(rep(p , NumVar - 1)))
      colnames(p) <- vars[2:NumVar] 
      return( list(p = p , q = q , Stat.table = crit.pq, min.Stat = min(crit.pq , na.rm = TRUE)) )
    }
  } else {

    crit <- array(NA, 500000)
    orders <- array(NA, dim =c(500000, NumVar))
    # for (p in 1:(max.p + 1)){  # max.p+1 is to include 0
      # combs <- data.frame(1:p)
      combs <- data.frame(0:max.p)
      if (NumVar > 2){
        for ( i in 2:(NumVar - 1)){
          # combs <- data.frame(combs, 1:p)
          combs <- data.frame(combs, 0:max.p)
        }
      }
      combs <- expand.grid(combs)
      colnames(combs) <- vars[2:NumVar]
      
      count <- 0
      for ( i in 1:(nrow(combs))){
        for (q in 0:max.q){ 
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
          if (sum(combs[i,] == 0) == (NumVar - 1)){ # all zeros
            max.p <- 1
            for (j in 1:(NumVar-1)){
              remP[[paste0("d" ,vars[j + 1])]] <- 1
            }
          }
          if ( q == 0){
            rem.p <- list(p = c(rem.p , remP), q = c(1) )
            modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = max.p , q = 1 , remove = rem.p)
          } else {
            rem.p <- list(p = c(rem.p , remP) )
            modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = max.p , q = q , remove = rem.p)
          }
            
          count <- count + 1
          orders[count,] <- c(unlist(combs[i,]),q)
          if (ic == "AIC"){
            crit[count] <- AIC(modelFull$model)
          } else if (ic == "BIC"){
            crit[count] <- BIC(modelFull$model)
          } else if (ic == "MASE"){
            crit[count] <- MASE(modelFull)$MASE
          } else if (ic == "GMRAE"){
            crit[count] <- GMRAE(modelFull)$GMRAE
          } 
        }
      }
    # }
    crit <- crit[1:count]
    if (!is.finite(min(crit))){
      stop("The minimum value of the criterion returns -inf! Please change the max.p and max.q and try again.")
    }
    orders <- orders[1:count,]

    if (sum(crit == min(crit)) > 1){
      best.order <- orders[which(crit == min(crit) , arr.ind = TRUE)[1],]
      p <- data.frame(best.order[1])
    } else {
      best.order <- orders[which(crit == min(crit) , arr.ind = TRUE),]
      p <- data.frame(t(best.order[1:(length(best.order)-1)]))
    }  
    
    colnames(p) <- vars[2:NumVar] 
    q <- best.order[length(best.order)]
    Stat.table <- data.frame(orders , crit)
    colnames(Stat.table) <- c(colnames(combs), "q", ic)
    return( list(p = p , q = q, Stat.table = Stat.table, min.Stat = min(crit, na.rm = TRUE)))
  }
}
