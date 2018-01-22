fnRefresh <-
  function(rdata,
           Indexes
           ,
           Tickers
           ,
           Session_Gainer
           ,
           Session_Loser
           ,
           Maxlimit
           ,
           MinLimit
           ,
           Current_SessionGnr_Lsr_Names
           ,
           ColorScale_df)
  {
    source("Global/fnFunctions/fnGetRTDataDump.R", local = T)$value
    source("Global/fnFunctions/GetIntradayGoogleDump.R", local = T)$value
    
    
    rdata <- rdata[rdata$Name != "N/A", ]
    
    #ignore this, random numbers for testing when market is closed
    #######################rnum#################
    # rdata$Last <- round(abs(rnorm(nrow(rdata))),4)*100  #
    # rdata$LastSizeNum <- round(abs(rnorm(nrow(rdata))),4)*50000 #
    # rdata$changeNum1 <- round(rnorm(nrow(rdata)),4) #
    # rdata$`% Change` <- rdata$changeNum1 #
    ###########################################
    #browser()
    Indxdta <- rdata[(1:length(Indexes)), ]
    tkrdta <-   rdata[-(1:length(Indexes)), ]
    gainer <- filter(tkrdta, changeNum1 > 0)
    gainer <- gainer[which.max(gainer$changeNum1), ]
    loser <- filter(tkrdta, changeNum1 < 0)
    loser <- loser[which.min(loser$changeNum1), ]
    fn_sessiongainer <-
      if (nrow(gainer)) {
        if (!nrow(Session_Gainer)) {
          gainer
        } else {
          if (gainer$changeNum1[1] > Session_Gainer$changeNum1[1]) {
            gainer
          } else {
            Session_Gainer
          }
        }
      } else {
        Session_Gainer
      }
    fn_sessionloser <-
      if (nrow(loser)) {
        if (!nrow(Session_Loser)) {
          loser
        } else {
          if (loser$changeNum1[1] < Session_Loser$changeNum1[1]) {
            loser
          } else {
            Session_Loser
          }
        }
      } else {
        Session_Loser
      }
    fn_maxlim <-
      if (nrow(fn_sessiongainer)) {
        max(fn_sessiongainer$changeNum1 + 0.001, Maxlimit)
      } else {
        Maxlimit
      }
    fn_minlim <-
      if (nrow(fn_sessionloser)) {
        min(fn_sessionloser$changeNum1 - 0.001, MinLimit)
      } else {
        MinLimit
      }
    Gainer_Loser <-
      c(Gainer = fn_sessiongainer$Symbol[1], Loser = fn_sessionloser$Symbol[1])
    fn_sessionGnrLsrNames <-
      unique(c(if (nrow(fn_sessiongainer)) {
        fn_sessiongainer$Symbol[1]
      } , if (nrow(fn_sessionloser)) {
        fn_sessionloser$Symbol[1]
      }, Current_SessionGnr_Lsr_Names))
    
    options(scipen = 999999)
    
    seq <-
      unique(c(
        seq(fn_minlim, 0, length.out = 4),
        seq(0, fn_maxlim, length.out = 4)
      ))
    
    bblz <- tkrdta
    
    bblz$bins <-
      cut(
        bblz$changeNum1,
        breaks = seq,
        include.lowest = T,
        right = F
      )
    bblz$colorid <-
      cut(
        bblz$changeNum1,
        breaks = seq,
        include.lowest = T,
        right = F,
        labels = F
      )
    bblz$upperlim <-
      as.numeric(sub("[^,]*,([^]]*)(\\)|])", "\\1", bblz$bins))
    bblz <-
      merge.data.frame(
        x = bblz,
        y = ColorScale_df,
        by.y = "colorid",
        by.x = "colorid",
        all.x = T,
        sort = F
      )
    
    bblz <- bblz[with(bblz, order(-LastSizeNum,-abs(changeNum1))), ]
    
    tz1 <- gsub("\\^GSPC", ".INX", Indexes)
    tz1 <- gsub("\\^IXIC", ".IXIC", tz1)
    tz1 <- gsub("\\^DJI", ".DJI", tz1)
    
    tz <- c(tz1, fn_sessionGnrLsrNames)
    BxIntraDta <- mapply(
      FUN = function(x) {
        intras <-
          suppressWarnings(tryCatch(
            GetIntradayGoogle(x) ,
            error = function(e)
              NULL
          ))
        spkln <-
          if (is.null(intras)) {
            NULL
          } else {
            a <- to.minutes(round(intras, 2), k = 10, OHLC = F)
            a <- data.frame(a)
            a <-
              spk_chr(a$AdjClose)
          }
        return(list(intra = intras, spark = spkln))
      }
      
      ,
      x = tz,
      SIMPLIFY = F
    )
    
    names(BxIntraDta) <- gsub(".INX", "\\^GSPC", names(BxIntraDta))
    names(BxIntraDta) <- gsub(".IXIC", "\\^IXIC", names(BxIntraDta))
    names(BxIntraDta) <- gsub(".DJI", "\\^DJI", names(BxIntraDta))
    
    return(
      list(
        Indxdta = Indxdta
        ,
        tkrdta = tkrdta
        ,
        sessiongainer = fn_sessiongainer
        ,
        sessionloser = fn_sessionloser
        ,
        maxlim = fn_maxlim
        ,
        minlim = fn_minlim
        ,
        sessionGnrLsrNames = Gainer_Loser
        ,
        AllGainerLoserS = fn_sessionGnrLsrNames
        ,
        seq = seq
        ,
        bblz = bblz
        ,
        Boxesintras = BxIntraDta
      )
    )
    
    
  }
