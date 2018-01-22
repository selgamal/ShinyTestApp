fnEfficientFrontier_tseries <-
function(
    Rets, # A numeric xts object containing returnes.
    Rf = 0.000, # A numeric for risk free rate to use, should be adjusted to Rets frequency,
    # for example, if Rets is monthly returns and annual risk free rate is 6% 
    # then Rf = 0.005 (6% /12)
    MinWt = NULL, # A numeric indicating contstraint for minimum weight for each asset, if NULL then assets
    # are allowed to have 0 weights
    MaxWt = NULL, # A numeric indicating contstraint for maximum weight for each asset, if NULL then one asset
    # is allowed to have 100% weight if it is feasible solution
    Npoints = 2000, # A non negative  Int representing number of points between TrgRetsLevels (inclusive) to be used for optimization
    TrgtRetsLevels = NULL  # A numeric vector of 2 elements representing the upper and lower bounds of return levels
    # to otimize the portifolio at, Npoints are produced by a seq() between upper and lower bounds
    # inclusive, if left NULL, will try to generate Npoints between max and min of Rets means that
    # can produce a feasible solution
  ) {
  library(tseries)
  library(Matrix)
  library(matrixcalc)
    # validate args
    if(
      !is.xts(Rets) || !all(unlist(lapply(Rets, is.numeric)))
    ) {
      stop("Rets must be numeric xts or dataframe containing assets returns")
    }
    
    assets <- names(Rets)
    n.assets <- length(assets)
    Rets <- na.fill(Rets, 0)
    
    cov.mat <- if(is.positive.definite(cov(Rets))) {cov(Rets)
      } else {as.matrix(nearPD(cov(Rets))$mat)}
    
    if(!is.numeric(Rf)) {
      stop("Rf must be numeric")
    }
    
    if (!missing(MinWt) && !is.null(MinWt) && !missing(MaxWt) && !is.null(MaxWt) && MinWt>=MaxWt ){
      stop("When defined, MinWt and MaxWt must be single numeric values with MaxWt > MinWt")
    }
    
    
    if (!missing(MinWt) && !is.null(MinWt)){
      if (!is.numeric(MinWt) || length(MinWt) != 1) {
        stop("When defined, MinWt and MaxWt must be single numeric values with MaxWt > MinWt")
      }
      
      if ((n.assets * MinWt) >= 1) {
        stop("MinWt * number of asset must be <1 to produce set of feasible solutions")
      }
    }
    
    if (!missing(MaxWt) && !is.null(MaxWt)){
      if (!is.numeric(MaxWt) || length(MaxWt) != 1) {
        stop("When defined, MinWt and MaxWt must be single numeric values with MaxWt > MinWt")
      }
      
      if ((n.assets * MaxWt) <= 1) {
        stop("MaxWt * number of asset must be > 1 to produce set of feasible solutions")
      }
      
    }
    
    
    if (!is.integer(as.integer(abs(Npoints)))) {
      stop("Npoints must be a non negative integer")
    } else {
      n.points <- as.integer(abs(Npoints))
    }
    
    if (!missing(TrgtRetsLevels) && !is.null(TrgtRetsLevels)) {
      # Validate TrgRetsLevels if Entered
      if(!is.numeric(TrgtRetsLevels) || length(TrgtRetsLevels) !=2) {
        stop("TrgtRetsLevels must be numeric vector of 2 elements")
      } else if (TrgtRetsLevels[1] == TrgtRetsLevels[2]) {
        stop("TargtRets elements must not equal each other")
      } else {
        # Set the lower and upper return levels to for the range to calculate efficient frontiers
        strt.retslvl <- min(TrgtRetsLevels)*(1 + (.Machine$double.eps^0.5))
        end.retslvls <- max(TrgtRetsLevels)*(1 - (.Machine$double.eps^0.5))
      }
      
    } else {
      # Figure out the range of return levels between min(colMeans(Rets)) and max(colMeans(Rets)) that has
      # with feasible solution given contstraints
      
      strt.retslvl.means <- min(colMeans(Rets))*(1 + (.Machine$double.eps^0.5))
      end.retslvls.means <- max(colMeans(Rets))*(1 - (.Machine$double.eps^0.5))
      retsvec.means <- seq(from = strt.retslvl.means, to = end.retslvls.means, length.out = Npoints)
      
      # A dataframe to hold the start and end of of feasible range
      result.df <- data.frame(
        pos = as.character(),
        ret = as.numeric(),
        index = as.numeric(),
        stringsAsFactors = F
      )
      
      # loop One will stop on first feasible solution from retsvec.means and return its index
      
      for(i in  retsvec.means) {
        #print(i)
        resMin <-
          tryCatch(
            portfolio.optim(x =  Rets,
                            pm = i,
                            shorts = F,
                            reslow = rep(MinWt, n.assets),
                            reshigh = rep(MaxWt, n.assets),
                            covmat = cov.mat
            ),
            error = function(e) NULL
          )
        
        if (!is.null(resMin)) {
          result.df[1, "pos"]<- "First"
          result.df[1, "ret"]<-  i
          result.df[1, "index"]<- which(retsvec.means==i)
          break()
        }
      }
      
      #loop two will stop on first feasible solution from retsvec.means in revers order and return its index
      for(i in  retsvec.means[Npoints:1]) {
        resMax <-
          tryCatch(
            portfolio.optim(x = Rets,
                            pm = i,
                            shorts = F,
                            reslow = rep(MinWt, n.assets),
                            reshigh = rep(MaxWt, n.assets),
                            covmat = cov.mat
            ),
            error = function(e) NULL
          )
        
        if (!is.null(resMax)) {
          result.df[2, "pos"]<- "Last"
          result.df[2, "ret"]<-  i
          result.df[2, "index"]<- which(retsvec.means==i)
          break()
        }
      }
      
      if (nrow(result.df)<=0 || (result.df[2,"index"] - result.df[1,"index"])<=0 ) {
        stop("No feasible solution found, try to change constraints")
      } else {
        strt.retslvl <- (retsvec.means[result.df[1,"index"]])*(1 + (.Machine$double.eps^0.5))
        end.retslvls <- (retsvec.means[result.df[2,"index"]])*(1 - (.Machine$double.eps^0.5))
      }
    } 
    
    retsvec <- seq(from = strt.retslvl,
                   to = end.retslvls,
                   length.out = Npoints)
    
    
    #n.loops <- length(retsvec)
    a.pos <- 1
    noFeasRets <- c()
    
    frontiers.df <- 
      matrix(nrow = length(retsvec),ncol=n.assets +5) 
    
    colnames(frontiers.df) <- c(colnames(Rets), "stdDev", "expReturn", "sharpe", "retLevel", "indx")
    
    for(a in retsvec) {
      frontiers.df[a.pos, "indx"] <- which(retsvec==a)
      frontier.pt <- tryCatch(
        portfolio.optim(x = Rets,
                        pm = a,
                        shorts = F,
                        reslow = rep(MinWt, n.assets),
                        reshigh = rep(MaxWt, n.assets),
                        covmat = cov.mat
        ),
        error = function(e) NULL
      )
      
      if (is.null(frontier.pt)) {
        noFeasRets <- c(noFeasRets, a)
        next()
      }
      
      frontiers.df[a.pos,"stdDev"] <- frontier.pt$ps
      frontiers.df[a.pos,"expReturn"] <- frontier.pt$pm
      frontiers.df[a.pos,"sharpe"] <- (frontiers.df[a.pos,"expReturn"] - Rf) / frontiers.df[a.pos,"stdDev"]
      frontiers.df[a.pos,"retLevel"] <- a # for testing and checking
      frontiers.df[a.pos,1:n.assets] <- frontier.pt$pw
      a.pos <- a.pos+1
    }
    frontiers.df <- na.omit(frontiers.df)
    frontiers.df <- as.data.frame(frontiers.df)
    if(nrow(frontiers.df)<1) {
      stop("No feasible solution found, try to change constraints")
    }

    ### Weight data for optimal portfolio 
    
    frontiers.df.optimal <- frontiers.df[frontiers.df$sharpe==max(frontiers.df$sharpe),]
    
    alloc.optim <- suppressMessages(reshape2::melt(round(frontiers.df.optimal[,1:(ncol(frontiers.df.optimal)-5)],4)
                                                   , variable.name = "symbol"
                                                   , value.name = "wt"
    )) #%>% dplyr::filter(wt != 0.000)
    alloc.optim$symbol <- as.character(alloc.optim$symbol)
    
    ### Weight data for min risk portfolio 
    
    frontiers.df.minrisk <- frontiers.df[frontiers.df$stdDev==min(frontiers.df$stdDev),]
    
    alloc.minrisk <- suppressMessages(reshape2::melt(round(frontiers.df.minrisk[,1:(ncol(frontiers.df.optimal)-5)],4)
                                                   , variable.name = "symbol"
                                                   , value.name = "wt"
    )) #%>% dplyr::filter(wt != 0.000)
    alloc.minrisk$symbol <- as.character(alloc.minrisk$symbol)
    
    
    # Prep 10 points on CAL line, with Optimal portfolio sharep ratio as slope and Rf as constant
    # hopfully passes through the optimal portfolio, as it should
    x.cal = seq(from = 0, to = frontiers.df.optimal$stdDev *(1.5), length.out = 10)
    y.cal = c(x.cal*(frontiers.df.optimal$sharpe))+Rf
    
    frontiers.df.cal <- data.frame(Risk = x.cal,
                                   Return = y.cal,
                                   stringsAsFactors = F
    )
    
    singles.df <- data.frame( Risk = apply(na.fill(Rets,0), 2, sd), #timeSeries::colStdevs(Rets) "filter()" masks dplyr filter
                              Return = colMeans(Rets), 
                              symbol = assets,
                              wtsOptim = alloc.optim$wt,
                              wtsMinrsk = alloc.minrisk$wt,
                              stringsAsFactors = F)
    # Equal Weights portfolio
    equiWeightsPort <- list()
    equiWeightsPort.ret <- tryCatch(
      Return.portfolio(R = Rets, geometric = F),
      error = function (e) NULL
    )
    
    equiWeightsPort$pw <- rep((1/n.assets),n.assets)
    equiWeightsPort$ps <- sd(equiWeightsPort.ret)
    equiWeightsPort$pm <- mean(equiWeightsPort.ret)
    
    
    if(is.null(equiWeightsPort.ret)) {
      equiWeightsPort <- "No Solution!"
      alloc.equiWts <- data.frame(symbol = NA,
                                  wt = NA,
                                  stringsAsFactors = F
      )
    } else {
      equiWeightsPort <- c(equiWeightsPort$pw, 
                           equiWeightsPort$ps, 
                           equiWeightsPort$pm,
                           equiWeightsPort$pm/equiWeightsPort$ps, 
                           equiWeightsPort$pm, 0)
      names(equiWeightsPort) <- names(frontiers.df)
      ### Weight data for equal weights portfolio 
      
      alloc.equiWts <- data.frame(symbol = names(frontiers.df)[1:(ncol(frontiers.df)-5)],
                                  wt = round(equiWeightsPort[1:(length(equiWeightsPort)-5)],4),
                                  stringsAsFactors = F
      ) 
      alloc.equiWts$symbol <- as.character(alloc.equiWts$symbol)
    }
    
    
    return(list(
      `Efficient Frontier Points` = frontiers.df,
      `Optimal Portfolio` = list(`Solver Result` = frontiers.df.optimal,
                                 `Weights Allocation` = alloc.optim),
      `Min. Risk Portfolio` = list(`Solver Result` = frontiers.df.minrisk,
                                   `Weights Allocation` = alloc.minrisk
                                   ),
      `Equal Weights Portfolio` = list(`Solver Result` = equiWeightsPort,
                                       `Weights Allocation` = alloc.equiWts
                                       ),
      `Capital Allocation Line` = frontiers.df.cal,
      `Individual Risk And Return` = singles.df,
      `Return Levels With No Solution` = noFeasRets,
      `Number of Points Used` = Npoints
    ))
    
    
}
