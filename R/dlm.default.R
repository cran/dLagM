#' @export
dlm.default <-
  function(formula = NULL , data = NULL, x , y , q , remove = NULL , type = 2){
    
    options(warn=-1)
    q <- round(q)
    if (q < 0){
      stop("Lag order must be a positive integer!")
    }
    if ((is.null(formula) == TRUE) | (is.null(data) == TRUE)){
      if ((is.vector(x)== FALSE) | (is.vector(y)== FALSE)){
        stop("Data series x and y both must be entered in a vector format!")
      }
      if (is.null(remove) == FALSE){
        if (is.matrix(remove) == TRUE){
          stop("You must enter a vector or scalar for lags to remove since you have only one independent series!")
        }
        if (length(which(remove$x != -1)) > q){
          stop("The lag you want to remove is greater than the lag length!")
        }
      }
      res <- dlm.main(x = x , y = y , q = q , remove = remove , type = 1)
      
    } else if ((is.null(formula) == FALSE) & (is.null(data) == FALSE)){
      if (plyr::is.formula(formula)== FALSE){
        stop("You must enter a formula object since you do not specify dependent and independent series!")
      }
      if (is.data.frame(data)== FALSE){
        stop("Data must be entered as a data.frame object!")
      }
      if (is.null(remove) == FALSE){
        if (is.list(remove) == FALSE){
          stop("You must enter a list, elemts of which show the lags to be removed from each independent series on each row!")
        }
        
        if (sum(which(names(remove) %in% get.vars(formula) == FALSE) > 0)){
          cat("The variable(s) called" , names(remove)[which(names(remove) %in% get.vars(formula) == FALSE)], 
              "in remove is not defined in formula!")
        }
        
        # Check if the biggest lag is removed from all the independent series
        cont <- TRUE
        while (cont & (q > 0)){
          if (sum(q == unlist(remove)) == (length(formula) - 1)){
            for ( i in 1:length(remove)){
              remove[[i]] <- remove[[i]][-which(remove[[i]] == q)]
            }
            q <- q - 1
          } else {
            cont <- FALSE
          }
        }
        
      }
      res <- dlm.main(formula = formula , data = data , q = q , remove = remove , type = 2)
      
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
    class(res) <- c("dlm" , "dLagM")
    res
    
  }