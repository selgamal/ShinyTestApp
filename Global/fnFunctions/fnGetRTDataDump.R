GetQuoteData <-
  function(tickers) {
    library(jsonlite)
    library(stringr)
    x = unique(tickers)
    fields <-
      c(
        "regularMarketTime"
        ,
        "shortName"
        ,
        "symbol"
        ,
        "regularMarketPrice"
        ,
        "regularMarketChangePercent"
        ,
        "regularMarketDayLow"
        ,
        "regularMarketDayHigh"
        ,
        "fiftyTwoWeekLow"
        ,
        "fiftyTwoWeekHigh"
        ,
        "marketCap"
        ,
        "fiftyTwoWeekLowChangePercent"
        ,
        "fiftyTwoWeekHighChangePercent"
        ,
        "bidSize"
        ,
        "fiftyDayAverage"
        ,
        "twoHundredDayAverage"
        ,
        "regularMarketOpen"
        ,
        "trailingPE"
        ,
        "regularMarketPreviousClose"
      )
    
    x <-
      fromJSON(
        paste(
          "https://query1.finance.yahoo.com/v7/finance/quote?",
          "formatted=false&symbols=",
          paste(x, collapse = ","),
          sep = ""
        )
      )
    x = as.data.frame(x$quoteResponse$result, stringsAsFactors = F)
    x$EBITDA <- NA
    #x$`Last Trade Size` <- NA
    x$`PEG Ratio` <- NA
    missing <- setdiff(fields, names(x))
    x[missing] <- NA
    newNames <-
      c(
        "regularMarketTime"
        ,
        "shortName"
        ,
        "symbol"
        ,
        "regularMarketPrice"
        ,
        "regularMarketChangePercent"
        ,
        "regularMarketDayLow"
        ,
        "regularMarketDayHigh"
        ,
        "fiftyTwoWeekLow"
        ,
        "fiftyTwoWeekHigh"
        ,
        "marketCap"
        ,
        "EBITDA"
        ,
        "fiftyTwoWeekLowChangePercent"
        ,
        "fiftyTwoWeekHighChangePercent"
        ,
        "bidSize"
        ,
        "fiftyDayAverage"
        ,
        "twoHundredDayAverage"
        ,
        "regularMarketOpen"
        ,
        "trailingPE"
        ,
        "PEG Ratio"
        ,
        "regularMarketPreviousClose"
      )
    
    oldNames <- c(
      "Trade Time"
      ,
      "Name"
      ,
      "Symbol"
      ,
      "Last"
      ,
      "% Change"
      ,
      "Low"
      ,
      "High"
      ,
      "52-week Low"
      ,
      "52-week High"
      ,
      "Market Capitalization"
      ,
      "EBITDA"
      ,
      "% Change From 52-week Low"
      ,
      "% Change From 52-week High"
      ,
      "Last Size"
      ,
      "50-day MA"
      ,
      "200-day MA"
      ,
      "Open"
      ,
      "P/E Ratio"
      ,
      "PEG Ratio"
      ,
      "P. Close"
    )
    
    x <- x[, c(newNames)]
    
    names(x) <- oldNames
    
    rownames(x) = NULL
    
    x$`Trade Time` = as.character(as.POSIXct(x$`Trade Time`, origin = '1970-01-01 00:00:00', tz = "EST"))
    
    x$changeNum1 = round(as.numeric(gsub("%", "", x$`% Change`)), 3)
    
    x$`% Change` = paste0(ifelse(!grepl("-", x$`% Change`), "+", ""),
                          format(round(as.numeric(
                            gsub("%", "", x$`% Change`)
                          ), 3), trim = T, nsmall = 3),
                          "%")
    
    x$changeNum52l = round(as.numeric(gsub("%", "", x$`% Change From 52-week Low`)), 3)
    
    x$`% Change From 52-week Low` = paste0(ifelse(!grepl("-", x$`% Change From 52-week Low`), "+", ""),
                                           format(round(as.numeric(
                                             gsub("%", "", x$`% Change From 52-week Low`)
                                           ), 3), trim = T, nsmall = 3),
                                           "%")
    
    x$changeNum52h = round(as.numeric(gsub("%", "", x$`% Change From 52-week High`)), 3)
    
    x$`% Change From 52-week High` = paste0(ifelse(!grepl("-", x$`% Change From 52-week High`), "+", ""),
                                            format(round(as.numeric(
                                              gsub("%", "", x$`% Change From 52-week High`)
                                            ), 3), trim = T, nsmall = 3),
                                            "%")
    
    x$Last = prettyNum(format(round(as.numeric(x$Last), 2), nsmall = 2),
                       big.mark = ",",
                       preserve.width = "none")
    
    x$Low = prettyNum(format(round(as.numeric(x$Low), 2), nsmall = 2),
                      big.mark = ",",
                      preserve.width = "none")
    
    x$High = prettyNum(format(round(as.numeric(x$High), 2), nsmall = 2),
                       big.mark = ",",
                       preserve.width = "none")
    
    x$`52-week Low` = prettyNum(format(round(as.numeric(x$`52-week Low`), 2), nsmall = 2),
                                big.mark = ",",
                                preserve.width = "none")
    
    x$`52-week High` = prettyNum(format(round(as.numeric(x$`52-week High`), 2), nsmall = 2),
                                 big.mark = ",",
                                 preserve.width = "none")
    
    x$`Last Size` = prettyNum(round(as.numeric(x$`Last Size`) * 100, 2),
                              big.mark = ",",
                              preserve.width = "none")
    
    x$`50-day MA` = prettyNum(format(round(as.numeric(
      gsub(",", "", x$`50-day MA`)
    ), 2), nsmall = 2),
    big.mark = ",",
    preserve.width = "none")
    
    x$`200-day MA` = prettyNum(format(round(as.numeric(
      gsub(",", "", x$`200-day MA`)
    ), 2), nsmall = 2),
    big.mark = ",",
    preserve.width = "none")
    
    x$Open = prettyNum(format(round(as.numeric(x$Open), 2), nsmall = 2),
                       big.mark = ",",
                       preserve.width = "none")
    
    x$`P. Close` = prettyNum(format(round(as.numeric(x$`P. Close`), 2), nsmall = 2),
                             big.mark = ",",
                             preserve.width = "none")
    
    x$`P/E Ratio` = round(as.numeric(x$`P/E Ratio`), 2)
    
    x$`PEG Ratio` = round(as.numeric(x$`PEG Ratio`), 2)
    
    x$Marqhtml = paste0('<span class ="marq">'
                        ,
                        x$Symbol
                        ,
                        " "
                        ,
                        ifelse(
                          grepl("\\+", x$`% Change`)
                          ,
                          paste0(
                            '<span style="color:green;">'
                            ,
                            " "
                            ,
                            x$Last
                            ,
                            ' &#9650</span></span>'
                          )
                          ,
                          paste0(
                            '<span style="color:red;">'
                            ,
                            " "
                            ,
                            x$Last
                            ,
                            ' &#9660</span></span>'
                          )
                        ))
    
    x$UpDn = paste0(ifelse(
      grepl("\\+", x$`% Change`)
      ,
      paste0('\u25B2')
      ,
      paste0('\U25BC')
    )
    , " "
    , x$`% Change`)
    
    
    x$`% Chg frm 52wk Lo` = paste0(ifelse(
      grepl("\\+", x$`% Change From 52-week Low`)
      ,
      paste0('\u25B2')
      ,
      paste0('\U25BC')
    )
    ,
    " "
    ,
    x$`% Change From 52-week Low`)
    
    
    x$`% Chg frm 52wk Hi` = paste0(ifelse(
      grepl("\\+", x$`% Change From 52-week High`)
      ,
      paste0('\u25B2')
      ,
      paste0('\U25BC')
    )
    ,
    " "
    ,
    x$`% Change From 52-week High`)
    
    
    
    
    
    x$xUpDn = paste0(x$`% Change`
                     , " "
                     , ifelse(
                       grepl("\\+", x$`% Change`)
                       ,
                       paste0('\u25B2')
                       ,
                       paste0('\U25BC')
                     ))
    
    x = x[, c(1:20, 24:28, 21:23)]
    
    colnames(x)[8:10] = c("52wk Lo", "52wk Hi", "Market Cap")
    x$LastSizeNum <-
      as.numeric(gsub(",", "", x$`Last Size`)) # x$changeNum1))
    x$lastPriceNum <- round(as.numeric(gsub(",", "", x$Last)), 2)
    x$SMA50Num <- round(as.numeric(gsub(",", "", x$`50-day MA`)), 2)
    x$SMA200Num <-
      round(as.numeric(gsub(",", "", x$`200-day MA`)), 2)
    x$MktCapNum <- suppressWarnings(as.numeric(x$`Market Cap`))
    x$`Market Cap` <-
      paste(round(x$`Market Cap` / 10 ^ 9, 3), "B", sep = "")
    
    return(x)
  }
