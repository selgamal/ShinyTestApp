#### When Clicking "Get Data" button Download selected tickers data ####
#### (see "Global/fnFunctions/fnGetRTData.R" & "Global/fnFunctions/fnGetHistory.R") ##


  # store tickers selected/typed in RVs$t and convert to uppercase for consistency
  RVs$t <- c(toupper(input$input_tkr))
  
  # Offload history data to intermidate var to avoid changes to the RV until all processing is done
  tkrHistoryData <- RVs$tkrHistoryData
  

  # Detect tickers deselected from previous selection and filter for only new tickers
  # to be downloaded, considering that we need the gainer and loser tickers to remain
  # even if removed because those represent the highest loss/gain through the session
  
  removed <-
    setdiff(names(tkrHistoryData), c(isolate(RVs$t), isolate(RVs$sessionGnrLsrNames)))
  
  # Remove deselected from variable storing all price history data
  tkrHistoryData[which(names(tkrHistoryData) %in% removed)] <-
    NULL
  
  # Determine new tickers to download
  slcttkrs <-
    unique(setdiff(isolate(RVs$t), names(tkrHistoryData)))
  
  # New tickers vector should have length

  # Download history: 10 years daily close or get from preloaded dataset
  m <- 
    if (length(slcttkrs)) {
      if(UsePreLoadedTkrData && exists("Preloaded_TkrsHistory")){
        Preloaded_TkrsHistory[slcttkrs]
      } else {
        fnGetHistory(tickers = slcttkrs, pb = T)
      }
      
    } else {
        NULL
      }
  
  # Add newely downloaded data to existing data
  tkrHistoryData <- c(tkrHistoryData, Filter(Negate(is.null), m))
  
  
  # Store names of tickers with no data
  RVs$natkrstxt <-
    paste(toupper(names(Filter(is.null, m))), collapse = ", ")
  
  Sys.sleep(1)
  
  # Notification for tickers with no data
  if (length(Filter(is.null, m))) {
    natkrsnotice <-
      showNotification(
        ui = tagList(
          tags$span("No Data or Data not Enough For:", style = "font-weight:bold;")
          ,
          tags$br()
          ,
          tags$span(RVs$natkrstxt, style = "color:red;")
        ),
        #paste("No Data Found For: ", paste(toupper(names(Filter(is.null, m))), collapse = ", "), sep = "<br>"),
        duration = 5,
        type = "warning",
        id = "natkrsnotification"
      )
    
    # update the text for the nav-bar alert for tickers with no data
    output$dropdowntxt <- renderText(RVs$natkrstxt)
    # In case of tickers with no data, make the alert visible in the nav-bar
    runjs(
      '
      $( document ).ready(function() {
      var a = document.getElementById("alert1");
      $(a).css("visibility", "visible");
      });
      '
    )
    
    
    # Trigger for the next process ????????????????????
    RVs$rmnotify <- RVs$rmnotify + 1
    }
  # Refresh quote data
  RVs$t.all <- RVs$t
  RVs$t <- RVs$t[!RVs$t %in% names(Filter(is.null, m))]
  rdatay <-tryCatch(
    suppressWarnings(GetQuoteData(c(RVs$indx,sort(toupper(RVs$t)))))
    , error = function(e) {warning("WARNING!!!!"); return(NULL)})
  if (!is.null(rdatay)) { 
    rdatay$Name <- mapply(
      function(t) {
        if(is.null(tkrHistoryData[[t]]$CoName) || is.na(tkrHistoryData[[t]]$CoName)) {
          rdatay$Name[which(rdatay$Symbol==t)]
        } else {
          tkrHistoryData[[t]]$CoName
        }
      }, t = rdatay$Symbol, USE.NAMES = F
    )  
    
  Refresh_obtn <-
    withProgress(message = "Getting Quotes...", value = 1, {
      fnRefresh(
        rdata = rdatay
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
    })
  # Store results in RVs
  RVs$sessiongainer <- Refresh_obtn$sessiongainer #
  RVs$sessionloser <- Refresh_obtn$sessionloser #
  RVs$maxlim <- Refresh_obtn$maxlim
  RVs$minlim <- Refresh_obtn$minlim
  RVs$sessionGnrLsrNames <- Refresh_obtn$AllGainerLoserS
  RVs$seq <- Refresh_obtn$seq
  RVs$Indxdta <- Refresh_obtn$Indxdta
  RVs$tkrdta <- Refresh_obtn$tkrdta
  RVs$bblz <- Refresh_obtn$bblz
  RVs$indxIntraday <-
    Refresh_obtn$Boxesintras[which(names(Refresh_obtn$Boxesintras) %in% RVs$indx)]
  RVs$tkrIntraday <-
    Refresh_obtn$Boxesintras[which(!names(Refresh_obtn$Boxesintras) %in% RVs$indx)]
  RVs$tkrHistoryData <- tkrHistoryData
}