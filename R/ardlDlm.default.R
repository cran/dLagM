#' @export
ardlDlm.default <-
  function( formula = NULL , data = NULL , x = NULL, y = NULL , p = 1 , q = 1 , remove = NULL, type = NULL){
    
    options(warn=-1)
    remove.p <- remove$p
    remove.q <- remove$q
    q <- round(q)
    if (q <= 0){
      stop("Lag order q must be a positive integer!")
    }
    p <- round(p)
    if (p <= 0){
      stop("Lag order p must be a positive integer!")
    }
    
    if ((is.null(formula) == TRUE) | (is.null(data) == TRUE)){
      if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
        stop("Data series x and y both must be in a vector format!")
      }
      if (is.null(remove.p) == FALSE){
        if (is.list(remove.p) == TRUE){
          stop("You must enter a vector or scalar to remove finite lags since you have only one independent series!")
        }
        if (length(remove.p) > p){
          stop("The number of lags you want to remove is greater than the finite lag length!")
        }
        # Check if the biggest lag is removed from the independent series
        cont <- TRUE
        while (cont & (p > 0)){
          if (p %in% remove.p){
            remove.p <- remove.p[-which(remove.p == p)]
            p <- p - 1
          } else {
            cont <- FALSE
          }
        }
        remove$p <- remove.p
      }
      if (is.null(remove.q) == FALSE){
        if (is.matrix(remove.q) == TRUE){
          stop("You must enter a vector or scalar for autoregressive lags to remove from the dependent series!")
        }
        if (length(which(remove.q != -1)) > q){
          stop("The number of autoregressive lags you want to remove is greater than the lag length!")
        }
      }
      # Check if the biggest lag is removed from the dependent series
      cont <- TRUE
      while (cont & (q > 0)){
        if (q %in% remove.q){
          remove.q <- remove.q[-which(remove.q == q)]
          q <- q - 1
        } else {
          cont <- FALSE
        }
      }
      remove$q <- remove.q
      
      res <- ardlDlm.main( x = x , y = y , p = p , q = q , remove = remove , type = 1)
      
    } else if ((is.null(formula) == FALSE) & (is.null(data) == FALSE)){
      if (is.formula(formula)== FALSE){
        stop("You must enter a model object since you do not specify dependent and independent series!")
      }
      if (is.data.frame(data)== FALSE){
        stop("Data must be entered as a data.frame object!")
      }
      
      if (is.null(remove.p) == FALSE){
        if (is.list(remove.p) == FALSE){
          stop("You must enter a list to remove finite lags!")
        }
        if (sum(which(names(remove$p) %in% c(get.vars(formula) , "ec") == FALSE) > 0)){
          cat("The variable(s) called" , names(remove$p)[which(names(remove$p) %in% get.vars(formula) == FALSE)], 
              "in remove is not defined in formula!")
        }
        for ( i in 1:length(remove.p)){
          if (length(remove.p[[i]]) > p){
            stop(paste0("The number of lags you want to remove is greater than the lag length for the series ", names(remove.p)[i]) )
          }
        }
      }
      # Check if the biggest lag is removed from all the independent series
      cont <- TRUE
      while (cont & (p > 0)){
        if ( (sum(p == unlist(remove.p)) == (length(get.vars(formula)) - 1)) & (p != 1) )  {
          for ( i in 1:length(remove.p)){
            remove.p[[i]] <- remove.p[[i]][-which(remove.p[[i]] == p)]
          }
          p <- p - 1
        } else {
          cont <- FALSE
        }
      }
      remove$p <- remove.p

      if (is.null(remove.q) == FALSE){
        if (is.matrix(remove.q) == TRUE){
          stop("You must enter a vector or scalar to remove autoregressive lags from the dependent series!")
        }
        if (length(which(remove.q != -1)) > q){
          stop("The number of autoregressive lags you want to remove is greater than the lag length!")
        }
        # Check if the biggest lag is removed from the dependent series
        cont <- TRUE
        while (cont & (q > 1)){
          if (q %in% remove.q){
            remove.q <- remove.q[-which(remove.q == q)]
            q <- q - 1
          } else {
            cont <- FALSE
          }
        }
        remove$q <- remove.q
      }

      res <- ardlDlm.main( formula = formula , data = data , p = p , q = q , remove = remove , type = 2)
      
    } else if ((is.null(formula) == TRUE) & (is.null(data) == FALSE)){
      if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
        stop("You must enter dependent and independent series since the data argument is missing!")
      }
    } else if ((is.null(formula) == FALSE) & (is.null(data) == TRUE)){
      if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
        stop("You must enter dependent and independent series since the formula argument is missing!")
      }
    }
    
    res$call <- match.call()
    class(res) <- c("ardlDlm" , "dLagM")
    res
    
  }