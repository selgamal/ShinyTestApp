fnGetHistory <-
  function(tickers,
           From = Sys.Date() - years(10),
           To = Sys.Date(),
           pb = F) {
    if (isTRUE(pb)) {
      progress <-
        Progress$new(session = getDefaultReactiveDomain(),
                     min = 1,
                     max = length(tickers))
      on.exit(progress$close())
    }
    tkrhist <- mapply(
      FUN = function(x) {
        tkr <- tryCatch({
          # Get ticker 10 years Price History
          tkr <-
            suppressWarnings(getSymbols(
              x,
              from = From,
              to = To,
              auto.assign = F
            ))
          tkr <-  na.omit(tkr)
          if (nrow(tkr[paste(max(index(tkr)) - years(1), "/")]) < 240) {
            stop()
          }
          
          # Get splits history
          s <- tryCatch(
            getSplits(x, from = "1900-01-01"),
            error = function(e) {
              NULL
            }
          )
          try(names(s) <- "splits", silent = T)
          
          # Get Dividends history
          d <- tryCatch(
            getDividends(x , from = "1900-01-01"),
            error = function(e) {
              NULL
            }
          )
          try(names(d) <- "dvd", silent = T)
          
          # Adapt for changes in yahoo
          # Update: use prices as is from yahoo (OHLC adjusted for split from source)
          #tkr <- suppressWarnings(fnReversAdj(x = tkr, s = s))
          
          # Adjust price history for splits and dividends
          # Update : will adjust only for dividends
          #tkr.a = fnAdjustOHLC.mod(tkr, d = d, s = s)
          
          #Adjusting for dividends
          tkr.a <- tkr
          if (is.xts(d) && nrow(d) > 0) {
            adjratio <- adjRatios(dividends = d, close = tkr[, 4])
            tkr.a[, 1:4] <- tkr.a[, 1:4] * drop(adjratio$Div)
          }
          
          # Get split adjustment ratio to apply to volumes
          # Update: will not adjust volumes
          #splt.r <- adjRatios(splits = s,close = tkr.a[,5])
          
          # Adjust volume for split only
          # Update: will not adjust volumes
          #tkr.a[,5] = tkr[,5]/splt.r$Split
          
          names(tkr.a) <-
            c("Open", "High", "Low", "Close", "Volume", "AdjClose")
          
          tkr.a <- round(tkr.a, 2)
          
          names(tkr) <-
            c("Open", "High", "Low", "Close", "Volume", "AdjClose")
          tkr <- round(tkr, 2)
          df <- tkr$AdjClose %>%
            xts::last("52 weeks") %>%
            to.weekly(OHLC = F) %>%
            data.frame()
          spkln <-
            c(df$AdjClose)
          
          
          fs_tkr <-
            tryCatch({
              tq_get(x, get = "financials")
            }, error = function(e)
              NULL)
          
          FS_yr <-
            if (is.null(fs_tkr) || is.na(fs_tkr)) {
              NULL
            } else {
              fs_tkr %>%
                select(type , annual) %>%
                unnest()
            }
          
          FS_qr <-
            if (is.null(fs_tkr) || is.na(fs_tkr)) {
              NULL
            } else {
              fs_tkr %>%
                select(type , quarter) %>%
                unnest()
            }
          
          FS_yr$Period <-
            if (is.null(fs_tkr) || is.na(fs_tkr)) {
              NULL
            } else {
              "Annual"
            }
          FS_qr$Period <-
            if (is.null(fs_tkr) || is.na(fs_tkr)) {
              NULL
            } else {
              "Quarter"
            }
          
          a <- tryCatch(
            bind_rows(FS_qr, FS_yr),
            error = function(e)
              NULL
          )
          rm(fs_tkr, FS_yr, FS_qr)
          ##
          a <- if (length(a)) {
            b <- a[a$type == "BS" & a$group %in% c(10, 17, 23, 31, 39, 40, 42, 27) |
                     a$type == "CF" &
                     a$group %in% c(7, 10, 15, 16, 17, 18, 19) |
                     a$type == "IS" &
                     a$group %in% c(3, 4, 5, 13, 17:21, 25, 31, 49)
                   , ]
          } else {
            NULL
          }
          ##
          Ratios_tkr <-
            tryCatch({
              tq_get(x, get = "key.ratios")
            }, error = function(e)
              NULL)
          
          Ratios_tkr <-
            if (is.null(Ratios_tkr) || is.na(Ratios_tkr)) {
              NULL
            } else {
              Ratios_tkr %>%
                unnest()
            }
          ##
          Ratios_tkr <-
            if (is.null(Ratios_tkr) || is.na(Ratios_tkr)) {
              NULL
            } else {
              filter(
                Ratios_tkr
                ,
                group %in% c(
                  13,
                  14,
                  52,
                  53,
                  17,
                  18,
                  19,
                  20,
                  22,
                  24,
                  26,
                  28,
                  30,
                  31,
                  74,
                  75,
                  76,
                  77,
                  78,
                  79,
                  80,
                  81,
                  82,
                  83,
                  84,
                  85,
                  86,
                  87,
                  88,
                  89
                )
                &
                  !is.na(value)
              ) #[,c(3:ncol(Ratios_tkr))]
            }
          
          URL <-
            paste0("https://finance.yahoo.com/quote/",
                   toupper(x),
                   "/history",
                   sep = "")
          xp.name <-
            '//*[@id="quote-header-info"]/div[2]/div[1]/div[1]/h1'
          xp.curr <-
            '//*[@id="quote-header-info"]/div[2]/div[1]/div[2]/span'
          page <- read_html(URL)
          currencyx <-
            html_nodes(page, xpath = xp.curr) %>% html_text()
          currency <- tryCatch(
            substr(
              currencyx,
              grepRaw("Currency in", currencyx),
              nchar(currencyx)
            ),
            error = function(e)
              NA
          )
          namex <- html_nodes(page, xpath = xp.name) %>% html_text()
          CoName <- tryCatch(
            gsub(paste(x, " - ", sep = ""), "", namex),
            error = function(e)
              NA
          )
          ##
          if (isTRUE(pb)) {
            progress$set(
              value = match(x, tickers)
              ,
              message  = "Getting Data"
              ,
              detail = paste0(toupper(x), "...")
            )
          }
          return(
            list(
              History = tkr
              ,
              AdjHistory = tkr.a
              ,
              spark52wks = spkln
              ,
              splits = s
              ,
              dividends = d
              ,
              fs = a
              ,
              ratios = Ratios_tkr
              ,
              currency = currency
              ,
              CoName = CoName
              
            )
          )
        }, error = function(e)
          NULL)
        
        if (isTRUE(pb)) {
          progress$set(
            value = match(x, tickers)
            ,
            message = "Getting Data"
            ,
            detail = paste0(x, if (is.null(tkr)) {
              " NO DATA OR DATA NOT ENOUGH"
            } else {
              "..."
            })
          )
        }
      }
      ,
      x = tickers
      ,
      SIMPLIFY = F
    )
    
    
    return(tkrhist)
    
  }

#save(fnGetHistory, file = "Global/fnFunctions/fnGetHistory.R")
