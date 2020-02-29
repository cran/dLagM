#' @import stats
#' @import graphics
#' @importFrom lmtest dwtest
#' @importFrom lmtest bptest
#' @importFrom lmtest waldtest
#' @importFrom strucchange efp
#' @importFrom strucchange recresid
#' @importFrom grDevices dev.new
#' @importFrom grDevices graphics.off
#' @importFrom utils tail
 
cusumSq <- function(model, alpha = 0.05){
# The following table is Table 1 of 
# Tests for Serial Correlation in Regression Analysis Based on the Periodogram of LeastSquares Residuals
# Author(s): J. Durbin
# Source: Biometrika, Vol. 56, No. 1 (Mar., 1969), pp. 1-15
# Published by: Oxford University Press on behalf of Biometrika Trust
# Stable URL: https://www.jstor.org/stable/2334686
# Accessed: 13-08-2019 01:44 UTC
  table <- matrix( c(NA, 0.1 , 0.05, 0.025, 0.01, 0.005,
          1	,	0.4	,	0.45	,	0.475	,	0.49	,	0.495	,
          2	,	0.35044	,	0.44306	,	0.50855	,	0.56667	,	0.59596	,
          3	,	0.35477	,	0.41811	,	0.46702	,	53456	,	0.579	,
          4	,	0.33435	,	0.39075	,	0.44641	,	0.50495	,	0.5421	,
          5	,	0.31556	,	0.37359	,	0.42174	,	0.47692	,	0.51576	,
          6	,	0.30244	,	0.35522	,	0.40045	,	0.4544	,	0.48988	,
          7	,	0.28991	,	33905	,	0.38294	,	0.43337	,	0.46761	,
          8	,	0.27828	,	0.32538	,	0.36697	,	0.41522	,	0.44819	,
          9	,	0.26794	,	0.31325	,	0.35277	,	0.39922	,	0.43071	,
          10	,	0.25884	,	0.30221	,	0.34022	,	0.38481	,	0.41517	,
          11	,	0.25071	,	0.29227	,	0.32894	,	0.37187	,	0.40122	,
          12	,	0.24325	,	0.2833	,	0.31869	,	0.36019	,	0.38856	,
          13	,	0.23639	,	0.27515	,	0.30935	,	0.34954	,	0.37703	,
          14	,	0.2301	,	0.26767	,	0.30081	,	0.3398	,	0.36649	,
          15	,	0.2243	,	0.26077	,	0.29296	,	0.33083	,	0.35679	,
          16	,	0.21895	,	0.25439	,	0.2857	,	0.32256	,	0.34784	,
          17	,	0.21397	,	0.24847	,	0.27897	,	0.31489	,	0.33953	,
          18	,	0.20933	,	0.24296	,	0.2727	,	0.30775	,	0.33181	,
          19	,	0.20498	,	23781	,	0.26685	,	0.30108	,	0.32459	,
          20	,	0.20089	,	0.23298	,	0.26137	,	0.29484	,	0.31784	,
          21	,	0.19705	,	0.22844	,	0.25622	,	0.28898	,	0.31149	,
          22	,	0.19343	,	0.22416	,	0.25136	,	0.28346	,	0.30552	,
          23	,	0.19001	,	0.22012	,	0.24679	,	0.27825	,	0.29989	,
          24	,	18677	,	0.2163	,	0.24245	,	0.27333	,	0.29456	,
          25	,	18370	,	0.21268	,	0.23835	,	0.26866	,	0.28951	,
          26	,	0.18077	,	0.20924	,	0.23445	,	0.26423	,	0.28472	,
          27	,	0.17799	,	20596	,	0.23074	,	0.26001	,	0.28016	,
          28	,	0.17533	,	0.20283	,	0.22721	,	0.256	,	0.27582	,
          29	,	0.1728	,	19985	,	0.22383	,	0.25217	,	0.27168	,
          30	,	0.17037	,	0.197	,	0.22061	,	0.24851	,	0.26772	,
          31	,	0.16805	,	0.19427	,	0.21752	,	0.24501	,	0.26393	,
          32	,	0.16582	,	19166	,	0.21457	,	0.24165	,	0.2603	,
          33	,	0.16368	,	0.18915	,	0.21173	,	0.23843	,	0.25683	,
          34	,	0.16162	,	0.18674	,	0.20901	,	0.23534	,	0.25348	,
          35	,	0.15964	,	0.18442	,	0.20639	,	0.23237	,	0.25027	,
          36	,	0.15774	,	18218	,	0.20387	,	0.22951	,	0.24718	,
          37	,	0.1559	,	0.18003	,	0.20144	,	0.22676	,	0.24421	,
          38	,	0.15413	,	0.17796	,	0.1991	,	0.2241	,	0.24134	,
          39	,	0.15242	,	17595	,	0.19684	,	0.22154	,	0.23857	,
          40	,	0.15076	,	0.17402	,	0.19465	,	0.21906	,	0.23589	,
          42	,	0.14761	,	0.17034	,	0.1905	,	0.21436	,	0.23081	,
          43	,	0.14611	,	0.16858	,	0.18852	,	0.21212	,	0.22839	,
          44	,	0.14466	,	0.16688	,	0.18661	,	0.20995	,	0.22605	,
          45	,	0.14325	,	0.16524	,	0.18475	,	20785	,	0.22377	,
          46	,	0.14188	,	0.16364	,	0.18295	,	0.20581	,	0.22157	,
          47	,	0.14055	,	0.16208	,	0.1812	,	0.20383	,	0.21943	,
          48	,	0.13926	,	0.16058	,	0.1795	,	0.2019	,	0.21735	,
          49	,	0.138	,	0.15911	,	0.17785	,	0.20003	,	0.21534	,
          50	,	13678	,	0.15769	,	0.17624	,	0.19822	,	0.21337	,
          51	,	0.13559	,	0.1563	,	0.17468	,	0.19645	,	0.21146	,
          52	,	0.13443	,	0.15495	,	0.17316	,	0.19473	,	0.20961	,
          53	,	0.1333	,	0.15363	,	0.17168	,	0.19305	,	0.2078	,
          54	,	0.13221	,	0.15235	,	0.17024	,	0.19142	,	0.20604	,
          55	,	0.13113	,	0.1511	,	0.16884	,	0.18983	,	0.20432	,
          56	,	0.13009	,	0.14989	,	0.16746	,	0.18828	,	0.20265	,
          57	,	0.12907	,	0.1487	,	0.16613	,	0.18677	,	0.20101	,
          58	,	0.12807	,	0.14754	,	0.16482	,	0.18529	,	0.19942	,
          59	,	0.1271	,	0.14641	,	0.16355	,	0.18385	,	0.19786	,
          60	,	0.12615	,	0.1453	,	0.1623	,	0.18245	,	0.19635	,
          62	,	0.12431	,	0.14316	,	0.1599	,	0.17973	,	0.19341	,
          64	,	0.12255	,	0.14112	,	0.1576	,	0.17713	,	0.19061	,
          66	,	0.12087	,	0.13916	,	0.1554	,	0.17464	,	0.18792	,
          68	,	0.11926	,	0.13728	,	0.15329	,	0.17226	,	0.18535	,
          70	,	0.11771	,	0.13548	,	0.15127	,	0.16997	,	0.18288	,
          72	,	0.11622	,	0.13375	,	0.14932	,	0.16777	,	0.18051	,
          74	,	0.11479	,	0.13208	,	0.14745	,	0.16566	,	0.17823	,
          76	,	0.11341	,	0.13048	,	0.14565	,	0.16363	,	0.17604	,
          78	,	0.11208	,	0.12894	,	0.14392	,	16167	,	0.17392	,
          80	,	0.11079	,	0.12745	,	0.14224	,	0.15978	,	0.17188	,
          82	,	0.10955	,	0.12601	,	0.14063	,	0.15795	,	0.16992	,
          84	,	0.10835	,	0.12462	,	0.13907	,	0.15619	,	0.16802	,
          86	,	0.10719	,	0.12327	,	0.13756	,	0.15449	,	0.16618	,
          88	,	10607	,	0.12197	,	0.1361	,	15284	,	0.1644	,
          90	,	0.10499	,	0.12071	,	0.13468	,	0.15124	,	0.16268	,
          92	,	0.10393	,	0.11949	,	0.13331	,	0.1497	,	0.16101	,
          94	,	0.10291	,	0.11831	,	0.13198	,	0.1482	,	0.594	,
          96	,	0.10192	,	0.11716	,	0.1307	,	0.14674	,	0.15783	,
          98	,	0.10096	,	0.11604	,	0.12944	,	0.14533	,	0.15631	,
          100	,	0.10002	,	0.11496	,	0.12823	,	0.14396	,	0.15483	), nrow = 80, ncol = 6, byrow = TRUE)
  # model = model1.1$ECM$EC.model
  
  Wr <- recresid(model, data = model$model, type = "Rec-CUSUM")

  k <- start(Wr)[1] - 1 #length(model$coefficients)
  t <- end(Wr)[1] + 1
  if (((t-k) %% 2) != 0){
    n1 <- min( 0.5 * (t-k) - 1.5, 100)
    n2 <- min( 0.5 * (t-k) - 0.5, 100)
    c01 <- table[which((table[,1] == n1)), which(table[1,] == alpha/2)]
    c02 <- table[which((table[,1] == n2)), which(table[1,] == alpha/2)]
    c0 <- mean(c(c01,c02))
  } else {
    n <- min ( 0.5 * (t-k)-1, 100)
    c0 <- table[which((table[,1] == n)), which(table[1,] == alpha/2)]
  }
  WrSq <- array(NA, length(Wr))
  lb <- array(NA, length(Wr))
  ub <- array(NA, length(Wr))
  for (i in 1:length(Wr)){
    WrSq[i] <- sum(Wr[1:i]^2)/sum(Wr^2)
    lb[i] <- -c0 + (start(Wr)[1] + i - 1 - k)/(t-k)
    ub[i] <- c0 + (start(Wr)[1] + i - 1 - k)/(t-k)
  }
  plot(ts(WrSq, start = start(Wr)), ylim = c(min(lb)-0.1,max(ub)+0.1), ylab="CUSUM of squared residuals", main ="Recursive CUSUM of squares test")
  lines(ts(lb, start = start(Wr)), col = "red")
  lines(ts(ub, start = start(Wr)), col = "red")
  abline(h=0)
}

appendList <- function(list1, list2){
  for (i in names(list1)[which(names(list1) == names(list2))]){
    list1[[i]] = sort(c(list1[[i]], list2[[i]]))
  }
  
  for (i in names(list2)[which(names(list2) != names(list1))]){
    list1[[i]] =  list2[[i]]
  }
  return(list1)
}

#' @export
ardlBound <- function(data = NULL , formula = NULL , case = 3 , p = NULL , remove = NULL, autoOrder = FALSE , 
                      ic = c("AIC", "BIC", "MASE", "GMRAE") , 
                      max.p = 15,  max.q = 15, ECM = TRUE, stability = TRUE){
  if (is.null(data)) stop("Enter data by data argument.")
  if (is.null(formula)) stop("A formula object showing the dependent and indepdenent series must be entered.")

  vars <- all.vars(formula)
  NumVar <- length(vars)
  
  if (is.null(p) | (autoOrder == TRUE) ){
    cat(" ", "\n")
    cat("Orders being calculated with max.p =", max.p , "and max.q =", max.q, "...\n\n")
    orders <- ardlBoundOrders(data = data , formula = formula, ic = "AIC" , max.p = max.p,  max.q = max.q )
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

  if (max(p[2:NumVar]) == 1){
    max.p <- 2
  } else {
    max.p <- max(p[2:NumVar])  
  }
  
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
  
  removeP$p <- appendList(removeP$p, remove$p)
  removeP$q <- remove$q
  rem.p <- appendList(rem.p, remove$p)
  removeP2 <- list(p = rem.p, q = remove$q )

  formula1 <- as.formula(paste0("d" , vars[1] , " ~ " , paste(c(vars , 
                                                                paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))   
  
  if (case == 1){
    formula1 <- update(formula1, ~. -1) # Case 1 requires this
    formula2 <- as.formula(paste0("d" , vars[1] , " ~ - 1 + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
    print(formula1)
    print(formula2)
    if (p[[vars[1]]] == 1){
      p[[vars[1]]] <- 2
      removeP <- appendList(removeP, list( q = c(1)))
      removeP2$q <- c(1)
    }
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP2) 
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP )
    if (length(modelFull$model$residuals) != length(modelNull$model$residuals)){
      strt <- nrow(data) - length(modelNull$model$residuals) 
      modelNull <- ardlDlm(formula = formula2, data = data.frame(tail(data,(length(modelFull$model$residuals) + strt))) , 
                           p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP )
    }
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[2:NumVar] / modelFull$model$coefficients[1])
      ec <- data[,1] + as.vector(mult %*% t(data[,2:NumVar]))
      data.ecm <- data.frame(cbind(data, ec))
      formula3 <- as.formula(paste0("d" , vars[1] , " ~ - 1 + ec + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
      modelECM <- ardlDlm(formula = formula3, data = data.ecm , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
      ecm.beta <- modelECM$model$coefficients["ec.1"]
    }
  } else if (case == 2){
    formula1 <- update(formula1, ~. +1) 
    formula2 <- as.formula(paste0("d" , vars[1] , " ~ - 1 + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
    if (p[[vars[1]]] == 1){
      p[[vars[1]]] <- 2
      removeP <- appendList(removeP, list( q = c(1)))
      removeP2$q <- c(1)
    }
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP2) 
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    if (length(modelFull$model$residuals) != length(modelNull$model$residuals)){
      strt <- nrow(data) - length(modelNull$model$residuals)
      modelNull <- ardlDlm(formula = formula2, data = data.frame(tail(data,(length(modelFull$model$residuals) + strt))) , 
                           p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    }
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[3:(NumVar+1)] / modelFull$model$coefficients[2])
      ec <- data[,1] + as.vector(mult %*% t(data[,2:NumVar])) + (modelFull$model$coefficients[1] / modelFull$model$coefficients[2])
      data.ecm <- data.frame(cbind(data, ec))
      formula3 <- as.formula(paste0("d" , vars[1] , " ~ - 1 + ec + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
      modelECM <- ardlDlm(formula = formula3, data = data.ecm , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
      ecm.beta <- modelECM$model$coefficients["ec.1"]
    }
  } else if (case == 3){
    formula2 <- as.formula(paste0("d" , vars[1] , " ~ +1 + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") ))
    if (p[[vars[1]]] == 1){
      p[[vars[1]]] <- 2
      removeP <- appendList(removeP, list( q = c(1)))
      removeP2$q <- c(1)
    }
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP2) 
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    if (length(modelFull$model$residuals) != length(modelNull$model$residuals)){
      strt <- nrow(data) - length(modelNull$model$residuals)
      modelNull <- ardlDlm(formula = formula2, data = data.frame(tail(data,(length(modelFull$model$residuals) + strt))) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    }
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[3:(NumVar+1)] / modelFull$model$coefficients[2])
      ec <- data[,1] + as.vector(mult %*% t(data[,2:NumVar]))
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
    if (p[[vars[1]]] == 1){
      p[[vars[1]]] <- 2
      removeP <- appendList(removeP, list( q = c(1)))
      removeP2$q <- c(1)
    }
    rem.p[["trend"]] <- c(1:(max.p-1))
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP2) 
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    if (length(modelFull$model$residuals) != length(modelNull$model$residuals)){
      strt <- nrow(data) - length(modelNull$model$residuals)
      modelNull <- ardlDlm(formula = formula2, data = data.frame(tail(data,(length(modelFull$model$residuals) + strt))) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    }
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[3:(NumVar+1)] / modelFull$model$coefficients[2])
      ec <- data[,1] + as.vector(mult %*% t(data[,2:NumVar])) + modelFull$model$coefficients["trend.t"] * trend / modelFull$model$coefficients[2]
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
    if (p[[vars[1]]] == 1){
      p[[vars[1]]] <- 2
      removeP <- appendList(removeP, list( q = c(1)))
      removeP2$q <- c(1)
    }
    removeP$p[["trend"]] <- c(1:(max.p-1))
    removeP2$p[["trend"]] <- c(1:(max.p-1))
    rem.p[["trend"]] <- c(1:(max.p-1))
    modelFull <- ardlDlm(formula = formula1, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP2 ) 
    modelNull <- ardlDlm(formula = formula2, data = data.frame(data) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    if (length(modelFull$model$residuals) != length(modelNull$model$residuals)){
      strt <- nrow(data) - length(modelNull$model$residuals)
      modelNull <- ardlDlm(formula = formula2, data = data.frame(tail(data,(length(modelFull$model$residuals) + strt))) , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
    }
    if ( ECM == TRUE){
      mult <- as.vector(modelFull$model$coefficients[3:(NumVar+1)] / modelFull$model$coefficients[2])
      ec <- data[,1] + as.vector(mult %*% t(data[,2:NumVar]))
      data.ecm <- data.frame(cbind(data, ec))
      formula3 <- as.formula(paste0("d" , vars[1] , " ~ ec + " , paste(c(paste0("d" , vars[2:NumVar] )) , collapse=" + ") , " + trend" ))
      
      modelECM <- ardlDlm(formula = formula3, data = data.ecm , p = (max.p-1) , q = (p[[vars[1]]]-1) , remove = removeP ) 
      ecm.beta <- modelECM$model$coefficients["ec.1"]
    }
  }

  bg <- NULL
  bg <- lmtest::bgtest(modelFull$model, type = "F")
  cat("\n Breusch-Godfrey Test for the autocorrelation in residuals:\n")
  print(bg)
  if (!is.na(bg$p.value)){
    if (bg$p.value < 0.05){
      cat("The p-value of Breusch-Godfrey test for the autocorrelation in residuals: ", bg$p.value , "< 0.05!\n" )
    }
  }
  cat("------------------------------------------------------", "\n")
  
  res <- modelFull$model$residuals
  cat("\n Ljung-Box Test for the autocorrelation in residuals:\n")
  lb <- Box.test(res, type = c("Ljung-Box"))
  print(lb)
  if (!is.na(lb$p.value)){
    if (lb$p.value < 0.05){
      cat("The p-value of Ljung-Box test for the autocorrelation in residuals: ", lb$p.value , "< 0.05!\n" )
    }
  }
  cat("------------------------------------------------------", "\n")
  
  bp <- NULL
  bp <- lmtest::bptest(modelFull$model) 
  cat("\n Breusch-Pagan Test for the homoskedasticity of residuals:\n")
  print(bp)
  if (!is.na(bp$p.value)){
    if (bp$p.value < 0.05){
      cat("The p-value of Breusch-Pagan test for the homoskedasticity of residuals: ", bp$p.value , "< 0.05!\n" )
      # stop("Bounds test procedure is stopped due to significant heteroskedasticity in residuals.")
    }
  }
  cat("------------------------------------------------------", "\n")
  
  sp <- NULL
  sp <- shapiro.test(modelFull$model$residual) 
  cat("\n Shapiro-Wilk test of normality of residuals:\n")
  print(sp)
  if (!is.na(sp$p.value)){
    if (sp$p.value < 0.05){
      cat("The p-value of Shapiro-Wilk test normality of residuals: ", sp$p.value , "< 0.05!\n" )
    }
  }
  cat("------------------------------------------------------", "\n")
  Fvalue <- lmtest::waldtest( modelNull$model , modelFull$model)$F[2]
  
  # if (is.null(k)) k = (NumVar - 1)
  k = (NumVar - 1)
  
  pssbounds(obs = nrow(data) , fstat = Fvalue , case = case, k = k )
  
  if ((stability == TRUE) & (ECM == TRUE)){
    cat("------------------------------------------------------", "\n")
    reset <- NULL
    reset <- lmtest::resettest(modelECM$model) 
    cat("\n Ramsey's RESET Test for model specification:\n")
    print(reset)
    if (reset$p.value < 0.05){
      cat("the p-value of RESET test: ", reset$p.value , "< 0.05!\n" )
    }
    cat("------------------------------------------------------", "\n")
    
    cusum.test <- efp(modelECM$model, data = modelECM$model$model, type = "Rec-CUSUM")
    mosum.test <- efp(modelECM$model, data = modelECM$model$model, type = "Rec-MOSUM")
    graphics.off()
    plot.new()
    par(oma = c(4, 1, 1, 1))
    two <- FALSE
    if (sum(c(any(is.nan(cusum.test$process)), any(is.nan(mosum.test$process)))) >0){
      par(mfrow = c(1,2))  
      two <- TRUE
    } else {
      par(mfrow = c(2,2))
    }
    if (!any(is.nan(cusum.test$process))){
      plot(cusum.test)
    }
    cusumSq(modelECM$model)
    if(!any(is.nan(mosum.test$process))){
      plot(mosum.test)
    }
    if (two){
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot.new()
      legend("bottom", c("Recursive residuals", "5% limits"), xpd = TRUE, horiz = TRUE, inset = 0, bty = "n", 
             col = c("black" , "red"), lwd=2, cex = 1)
    }  else {
      # plot(0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      plot.new()
      legend("top", c("Recursive residuals", "5% limits"), col = c("black" , "red"), 
           xpd = TRUE, bty = "o", lwd = 2, cex = 1) 
    }
  }
  
  if (ECM){
    cat("------------------------------------------------------", "\n")
    cat("Error Correction Model Output: \n")
    summary(modelECM)
    
    cat("------------------------------------------------------", "\n")
    cat("Long-run coefficients: \n")
    if (case == 1){
      print(modelFull$model$coefficients[1:NumVar])
    } else {
      print(modelFull$model$coefficients[2:(NumVar+1)])
    }
    cat(" ", "\n")
    # text <- paste0("EC = " , names(modelFull$model$coefficients[2]), " - (")
    # for ( i in 3:(NumVar+1)){
    #   text <- paste0(text, "(", modelFull$model$coefficients[i]/modelFull$model$coefficients[2], " * ",
    #                  names(modelFull$model$coefficients[i]) , ") + ")
    # }
    # cat(text, "\n")
  }
  if (ECM){
    return(list(model = list(modelNull = modelNull, modelFull = modelFull), F.stat = Fvalue , p = p , q = q, k = k, bg = bg, lb = lb , bp = bp, sp = sp,
             ECM = list(EC.t = ec, EC.model = modelECM$model , EC.beta = ecm.beta, EC.data = data.ecm), ARDL.model = modelFull$model ) )  
  } else {
    return(list(model = list(modelNull = modelNull, modelFull = modelFull), ARDL.model = modelFull$model, F.stat = Fvalue , p = p , q = q, k = k, bg = bg, lb = lb , bp = bp, sp = sp ) )  
  }
  
}
