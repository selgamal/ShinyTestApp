### Create "pre-loaded" datasets

# Date Range

strt <- as.Date("2008-01-01")
end <- as.Date("2018-01-01")

### first list of tickers of choice:

companies <- readRDS("RDSs/CompaniesList.rds") # A list of companies I prepared (S&P100)

tkrs <- companies$symbol[!grepl("[[:punct:]]", companies$symbol )] # extract ticker symbols excluding those with special characters

indx <- c("^GSPC", "^IXIC", "SPY", "^DJI") # those are hard coded in the app -->> constants

### load functions 
source("Global/fnFunctions/fnGetHistoryDump.R", local = T)$value # Gets price history

source("Global/fnFunctions/fnAllTkrsAllPeriodsDump.R", local = T)$value # Creates retunrs data set (monthly, weekly, daily)

source("Global/fnFunctions/fnRiskFreRateTablesDump.R", local = T)$value

# Get price tkrs history
x <- fnGetHistory(tickers = sort(tkrs),From = strt, To = end )

# Check downloaded downloaded data for completeness
# function to check every list item see if it has the required data
Chktkrs <- function(tkrs) {
  chkTbl <- data.frame(
    tkr = character(),
    Name = character(),
    RowsInHistory = numeric(),
    RowsInAdjHistory = numeric(),
    startD = as.Date(numeric()),
    EndD = as.Date(numeric()),
    RowsIn52Wks = numeric(),
    FS = character(),
    Ratios = character(),
    Currency = character(),
    stringsAsFactors = F)
  rn <- 1
  for(t in names(tkrs)) {
  NumRows.Hist <- if(is.null(tkrs[[t]]$History)) {NA} else {nrow(tkrs[[t]]$History)}
  NumRows.AdjHist <- if(is.null(tkrs[[t]]$AdjHistory)) {NA} else {nrow(tkrs[[t]]$AdjHistory)}
  s.d <-  if(is.null(tkrs[[t]]$AdjHistory)) {NA} else {min(index(tkrs[[t]]$AdjHistory))}
  e.d <-  if(is.null(tkrs[[t]]$AdjHistory)) {NA} else {max(index(tkrs[[t]]$AdjHistory))}
  NumRows.52Wks <- if(is.null(tkrs[[t]]$spark52wks)) {NA} else {length(tkrs[[t]]$spark52wks)}
  fs <- if(is.null(tkrs[[t]]$fs)) { NA} else {"OK"}
  ratios <- if(is.null(tkrs[[t]]$ratios)) { NA} else {"OK"}
  curr <- if(is.null(tkrs[[t]]$currency)) {NA} else {tkrs[[t]]$currency}
  nme <- if(is.null(tkrs[[t]]$CoName)) {NA} else {tkrs[[t]]$CoName}
  
  chkTbl[rn,1] <- t
  chkTbl[rn,2] <- nme
  chkTbl[rn,3] <- NumRows.Hist
  chkTbl[rn,4] <- NumRows.AdjHist
  chkTbl[rn,5] <- s.d
  chkTbl[rn,6] <- e.d
  chkTbl[rn,7] <- NumRows.52Wks
  chkTbl[rn,8] <- fs
  chkTbl[rn,9] <- ratios
  chkTbl[rn,10] <- curr
  rn <- rn+1
                 
  }
  return(chkTbl)
}

chk.x <- Chktkrs(x)

# Find incomplete cases
incomp.x <- chk.x[!complete.cases(chk.x),]

#Extract tickers with complete data only
x.final <- x[setdiff(chk.x$tkr, incomp.x$tkr)]


saveRDS(object = x.final,file = "RDSs/tkrsHistoryDataset.rds") # Save as RDS to be used in app

x <- readRDS("RDSs/tkrsHistoryDataset.rds")


# Prep companies Table

tkrs.tbl <- filter(companies, symbol %in% names(x)) # Extract tickers only in dataset

names(tkrs.tbl) <- c("Name", "Ticker", "Sector", "Exchange")

saveRDS(object = tkrs.tbl,file = "RDSs/TableOfCompanies.rds") # Save as RDS to be used in app


# prep monthly, weekly and daily rets and closings dataset to save calculation time while app is running
x.rets <- fnAllTkrsAllPeriods(x = x, type = "AdjHistory")

saveRDS(object = x.rets,file = "RDSs/tkrsReturns.rds") # Save as RDS to be used in app

# Get price indexes history

x.i <- mapply(
  FUN = function(x, y, z) {
    closeAllConnections()
    tkr <- tryCatch({
      tkr <-  suppressWarnings(getSymbols(x, from = y, to = z, auto.assign = F))
      tkr <- na.omit(tkr)
      names(tkr) <-
        c("Open", "High", "Low", "Close", "Volume", "AdjClose")
      tkr <- round(tkr, 2)
      return(
        list(History = tkr)
      )
    }, error = function(e)
      NULL)}
  ,
  x = indx
  ,
  y = strt
  ,
  z = end
  ,
  SIMPLIFY = F)

saveRDS(object = x.i,file = "RDSs/indxHistoryDataset.rds")

# prep monthly, weekly and daily rets and closings dataset to save calculation time while app is running
x.i.rets <- fnAllTkrsAllPeriods(x = x.i, type = "History")

saveRDS(object = x.i.rets,file = "RDSs/indxReturns.rds") # Save as RDS to be used in app




#### Dataset for Yield Curve with the same date range as price histories and returns

## get data (see function "Global/fnFunctions/fnRiskFreRateTablesDump.R")
rf.dta <- fnRiskFreRateTables(x = readRDS("RDSs/YCHistory.rds"),from = strt, to = end )

saveRDS(object =rf.dta,file = "RDSs/YieldCurveRatesDataset.rds") # Save as RDS to be used in app


#### The purpose of this is to enhance performance on shinyapps.io, when "UsePreLoadedTkrData" is set to TRUE 
#### then the above 2 RDSs are loaded when the app starts to save time on download and calculations when multiple
#### sessions are working at the same time


