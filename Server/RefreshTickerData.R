#refreshintraObs$suspend(); #refreshObs$suspend()

# Get quotes table from yahoo and clean it up
rdatax <-tryCatch(
  suppressWarnings(GetQuoteData(c(RVs$indx,sort(toupper(RVs$t)))))
  , error = function(e) {warning("WARNING!!!!"); return(NULL)})

if (!is.null(rdatax)) { 
#### fix company name
  rdatax$Name <- mapply(
    function(t) {
      if(is.null(RVs$tkrHistoryData[[t]]$CoName) || is.na(RVs$tkrHistoryData[[t]]$CoName)) {
        rdatax$Name[which(rdatax$Symbol==t)]
      } else {
        RVs$tkrHistoryData[[t]]$CoName
      }
    }, t = rdatax$Symbol, USE.NAMES = F
  )  

Refresh_otime <- fnRefresh(
  rdata = rdatax
  ,
  Indexes = RVs$indx
  ,
  Tickers = RVs$t
  ,
  Session_Gainer = RVs$sessiongainer
  ,
  Session_Loser = RVs$sessionloser
  ,
  Maxlimit = RVs$maxlim
  ,
  MinLimit = RVs$minlim
  ,
  Current_SessionGnr_Lsr_Names = RVs$sessionGnrLsrNames
  ,
  ColorScale_df = colscaldf
)

RVs$sessiongainer <- Refresh_otime$sessiongainer
RVs$sessionloser <- Refresh_otime$sessionloser
RVs$maxlim <- Refresh_otime$maxlim
RVs$minlim <- Refresh_otime$minlim
RVs$sessionGnrLsrNames <- Refresh_otime$AllGainerLoserS
RVs$seq <- Refresh_otime$seq
RVs$Indxdta <- Refresh_otime$Indxdta
RVs$tkrdta <- Refresh_otime$tkrdta
RVs$bblz <- Refresh_otime$bblz
RVs$indxIntraday <-
  Refresh_otime$Boxesintras[which(names(Refresh_otime$Boxesintras) %in% RVs$indx)]
RVs$tkrIntraday <-
  Refresh_otime$Boxesintras[which(!names(Refresh_otime$Boxesintras) %in% RVs$indx)]

}