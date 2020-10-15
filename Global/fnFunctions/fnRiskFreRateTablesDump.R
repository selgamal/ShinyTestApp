fnRiskFreRateTables <-
  function(x, from, to) {
    # Read daily yield curve historty, rds contains data form 2007 to 2016 to
    # enhance performance by not having to download the whole table again from treasury.gov
    # this rds is given as an input to this function and all needed datasets are returned update
    # up to last update available on treasuy.us
    
    YCHistory <- x
    names(YCHistory)[-1] <-
      ifelse(grepl(" mo", names(YCHistory)[-1]),
             gsub(" mo", "M", names(YCHistory)[-1]),
             ifelse(
               grepl(" yr", names(YCHistory)[-1]),
               gsub(" yr", "Y", names(YCHistory)[-1]),
               names(YCHistory)[-1]
             ))
    
    
    # Calculate years needed to update data in the yield curve history
    
    YCYearsUpdt <-
      as.character(c((max(
        year(YCHistory$Date)
      ) + 1):year(Sys.Date())))
    
    
    
    #Url string to pull the table from treasury.gov
    urlYC <-
      "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year="
    
    # Download update data and clean it
    YC_update <- mapply(function (x) {
      urlx <- paste(urlYC, x, sep = "")
      a <- read_html(urlx) %>%
        html_node('.t-chart') %>%
        html_table()
      return(a)
    },
    x = YCYearsUpdt,
    SIMPLIFY = F)
    
    YC_update <-
      do.call(rbind.data.frame, c(YC_update, make.row.names = F))
    
    YC_update[YC_update == "N/A"] <- NA
    
    
    
    YCDates <- as.Date(YC_update$Date, "%m/%d/%y")
    
    YC_update <- lapply(YC_update[, -1], as.numeric)
    
    YC_update <- do.call(cbind.data.frame, YC_update)
    
    YC_update$Date <- YCDates
    
    YC_update <- YC_update[, c(ncol(YC_update), 1:(ncol(YC_update)-1))]
    
  
    names(YC_update)[-1] <-
      ifelse(grepl(" mo", names(YC_update)[-1]),
             gsub(" mo", "M", names(YC_update)[-1]),
             ifelse(
               grepl(" yr", names(YC_update)[-1]),
               gsub(" yr", "Y", names(YC_update)[-1]),
               names(YC_update)[-1]
             ))
    
    
    # Bind update date to history
    YC_update_ <- YC_update[,names(x)]
    
    YC_tbl <-
      rbind.data.frame(YCHistory, YC_update_, make.row.names = F)
    
    if (missing(from)) {
      f <- min(YC_tbl$Date)
    } else {
      f <- from
    }
    
    if (missing(from)) {
      t <- max(YC_tbl$Date)
    } else {
      t <- to
    }
    
    YC_tbl <- filter(YC_tbl, Date >= f & Date <= t)
    # Prep data to be used in charts
    
    YC_melt <- melt(YC_tbl, id = "Date", stringsAsFactors = F)
    
    YC_melt$variable <- as.character(YC_melt$variable)
    
    YC_melt$DateMDY <- format(YC_melt$Date, "%b %d, %Y")
    
    YC_xts.main <- xts(YC_tbl[, -1], order.by = YC_tbl$Date)
    
    YC_xts.main <- suppressWarnings(to.monthly(YC_xts.main, OHLC = F))
    
    YCdf.mon <-
      data.frame(
        Date = gsub(" 20", "-", as.character(index(YC_xts.main))),
        coredata(YC_xts.main),
        stringsAsFactors = F,
        check.names = F
      )
    
    YCdf.mon$sort <- as.numeric(rownames(YCdf.mon))
    
    YC_monthly.df <- data.frame(
      Date = index(YC_xts.main),
      coredata(YC_xts.main),
      stringsAsFactors = F,
      check.names = F
    )
    
    YC_monthly.df$Date <-
      as.Date(YC_monthly.df$Date) + months(1) - days(1)
    
    YC_monthly.df <- melt(YC_monthly.df, id = "Date")
    
    YC_monthly.df$variable <- as.character(YC_monthly.df$variable)
    
    YC_xts <- YC_xts.main
    
    
    
    maxYCT_date <- max(YC_monthly.df$Date)
    Max_Rf <- ceiling(max(YC_melt$value, na.rm = T))
    closeAllConnections()
    
    return(
      list(
        YC_melt = YC_melt,
        YC_monthly.df = YC_monthly.df,
        YC_xts = YC_xts,
        YCdf.mon = YCdf.mon,
        maxYCT_date = maxYCT_date,
        Max_Rf = Max_Rf
      )
    )
  }
