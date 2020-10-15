GetIntradayGoogle <-
# function (symbol) {
#   library(zoo)
#   library(xts)
#   URL <- paste0("https://finance.google.com/finance/getprices?i=60&p=1d&f=d,c&df=cpct&q=",
#                 toupper(symbol))
#   a <-  read.csv(paste(URL, collapse=""), sep=",", header=FALSE,
#                                skip=7)
#   a$V1 <- as.numeric(gsub('\\D','',a$V1))
#   
#   a$V1 <- c(a$V1[1], tail(c(a$V1[1] + a$V1*60),-1) ) 
#   
#   a$V1 <- as.POSIXct(a$V1, origin = '1970-01-01', tz='EST')
#   
#   intraday <- as.xts(a$V2, order.by = a$V1)
#   
#   colnames(intraday)<- "AdjClose"
#   
#   intraday <- round(intraday,2)
#   
#   return(intraday)
# }

function (symbol) {
  library(zoo)
  library(xts)
  url <-
    paste(
      "https://query1.finance.yahoo.com/v8/finance/chart/",
      symbol,"?interval=1m",
      sep = ""
    )
  x <- read_json(url)
  
  
  t <- as.POSIXct(unlist(x$chart$result[[1]]$timestamp),
                  origin = '1970-01-01' 
                  ,tz='US/Eastern'
  )
  c <- unlist(x$chart$result[[1]]$indicators$quote[[1]]$close)     
  len <- min(c(length(t),length(c)))
  intraday <- na.omit(xts(c[1:len], order.by = t[1:len]))
  
  
  colnames(intraday)<- "AdjClose"
  
  intraday <- round(intraday,2)
  
  return(intraday)
}

