# Get displayed date range for the 10 years history plot
## This is to help identify the dsipayed date range and
## update the high/low prices for that date range
observeEvent(input$dytenyears_date_window, {
  # Get the start and end dates array and add 1 day to both dates
  lims1 <-
    as.Date(strftime(req(input$dytenyears_date_window), format = "%Y-%m-%d")) + c(0, 1)
  # Store the start and end dates (as an rv) in a format that can be passed to other functions
  RVs$lims <- as.character(paste(lims1, collapse = "/"))
})

# same as above but for the intraday chart
observeEvent(input$dyintra_date_window, {
  lims2 <-
    as_datetime(
      strptime(
        input$dyintra_date_window,
        format = "%Y-%m-%dT%H:%M:%S",
        tz = "UTC"
      ),
      tz = "EST"
    ) 
  RVs$lims2 <- as.character(paste(lims2, collapse = "/"))
  
})

# Opening modal and rendering chart on clicking any of the boxes

observeEvent({
  input$js_symbl_clkd
}, {
  
  # initially hide previous charts if any
  hide("dytenyears")
  hide("dyintra")
  
  # Store ticker symbol from the clicked UI
  RVs$slctdbxsym <- input$js_symbl_clkd[1]
  
  RVs$slctdbxsymIntra <- switch(
    RVs$slctdbxsym
    , "^GSPC" = ".INX"
    , "^IXIC" = ".IXIC"
    , "^DJI" = ".DJI"
    , RVs$slctdbxsym
  
  )
  
  # Get intra day data for the the intraday chart
   RVs$slctd_symb_intraday <- tryCatch(GetIntradayGoogle( RVs$slctdbxsymIntra), error = function(e) xts())

  
  req(length(RVs$slctdbxsym) > 0)
  
  # Render chart title (in modal header) containing Company name, ticker symbol, last price and date
  # if it is an index, then get the data from existing RVs, if not then download the data using the 
  # stored ticker symbol and update every 10 secs
  output$charttitle <-
    if (RVs$slctdbxsym %in% RVs$indx) {
      renderText(paste(
        RVs$Indxdta$Name[RVs$Indxdta$Symbol == isolate(RVs$slctdbxsym)] ,
        "(",
        isolate(RVs$slctdbxsym),
        ")",
        ": ",
        RVs$Indxdta[RVs$Indxdta$Symbol == isolate(RVs$slctdbxsym), 4] ,
        " On ",
        strftime(RVs$Indxdta[RVs$Indxdta$Symbol == isolate(RVs$slctdbxsym), 1], format = "%b %d, %Y at %H:%M")
      ))
    } else {
      RVs$q1 <- RVs$tkrdta[RVs$tkrdta$Symbol == isolate(RVs$slctdbxsym), ]
      
      renderText(paste(
        RVs$q1$Name[1] ,
        "(",
        isolate(RVs$slctdbxsym),
        ")",
        ":",
        gsub("Currency in", "", RVs$tkrHistoryData[[RVs$slctdbxsym]]$currency),
        RVs$q1[1, 4],
        " On ",
        strftime(RVs$q1[1, 1], format = "%b %d, %Y at %H:%M")
      ))
      
    }
  
  ## Render 2 dygraphs ########
  
  
  output$dyintra <- bxpltd()
  output$dytenyears <-  bxplty()
  
  
  
  observeEvent(input$slctprd, {
    if (input$slctprd != "1 Day") {
      showElement("dytenyears")
      hideElement("dyintra")
      hideElement(selector = HTML("#dycontainer > .shiny-output-error"))
    } else {
      showElement("dyintra")
      showElement(selector = HTML("#dycontainer > .shiny-output-error"))
      hide("dytenyears")
    }
  })
  
  
  
  ##### Chart Functions ######

  # Update the selected period to show intraday (always start with intraday)
  updateSelectInput(session, inputId = "slctprd", selected = "1 Day")
  
  toggleModal(session = session,
              modalId = "plotmodal",
              toggle = "open")
})

################
bxpltd <- function () {
  renderDygraph({
    todayx <- RVs$slctd_symb_intraday
    
    x <- todayx
    
    lims <- if (length(RVs$lims2)) {
      RVs$lims2
    } else {
      ""
    }
    
    maxlimlabel <-
      try(paste("Max: ",
                suppressWarnings(prettyNum(max(
                  x$AdjClose[lims], na.rm = T
                ), big.mark = ",")),
                "At" ,
                format(index(x[lims][suppressWarnings(which.max(x$AdjClose[lims]))]), "%T")))
    
    minlimlabel <-
      try(paste("Min: ",
                suppressWarnings(prettyNum(min(
                  x$AdjClose[lims], na.rm = T
                ), big.mark = ",")),
                "At" ,
                format(index(x[lims][suppressWarnings(which.min(x$AdjClose[lims]))]), "%T")))
    
    shiny::validate(
      need(nrow(x) > 0, "Missing Data Please Try Again"),
      need(length(lims) > 0, "Missing Data Please Try Again")
    )
    RVs$dy <- dygraph(x$AdjClose) %>%
      dyOptions(
        rightGap = 20,
        gridLineColor = "#DEDEDE",
        axisLineColor = "transparent",
        colors = c("#6495ED"),
        useDataTimezone = T
      )    %>%
      dyAxis(
        "x",
        drawGrid = F,
        valueFormatter = ' function (d) {
        x = moment.tz(d, "America/New_York").format(\'MMM, D YYYY H:mm z\')
        return x
  }'
) %>%
      dyAxis(
        "y",
        valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
        axisLabelFormatter = 'function(d){return d.toFixed(2).replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
        gridLineColor = "#DEDEDE"
      ) %>%
      dyRangeSelector(retainDateWindow = T, dateWindow = NULL) %>%
      dyLimit(
        suppressWarnings(as.numeric(max(
          x$AdjClose[lims], na.rm = T
        ))),
        color = "green",
        label = maxlimlabel,
        labelLoc = "right"
      ) %>%
      dyLimit(
        suppressWarnings(as.numeric(min(
          x$AdjClose[lims], na.rm = T
        ))),
        color = "red",
        label = minlimlabel,
        labelLoc = "right"
      ) %>%
      dyLegend(dygraph,
               show = "auto",
               width = 250,
               labelsDiv = "dylabelesDiv")
    
    
})
}


bxplty <- function () {
  renderDygraph({
    tenyears <- if (RVs$slctdbxsym %in% RVs$indx) {
      RVs$indxHistoryData[[RVs$slctdbxsym]]$History
    } else  {
      RVs$tkrHistoryData[[RVs$slctdbxsym]]$History
    }
    
    x <- switch(
      input$slctprd
      ,
      "1 Day" = NULL
      ,
      "3 Months" = xts::last(tenyears, "3 months")
      ,
      "6 Months" = xts::last(tenyears, "6 months")
      ,
      "1 Year" = xts::last(tenyears, "12 months")
      ,
      "3 Years" = xts::last(tenyears, "3 years")
      ,
      "5 Years" = xts::last(tenyears, "5 years")
      ,
      "10 Years" = tenyears
    )
    
    lims <- if (length(RVs$lims)) {
      RVs$lims
    } else {
      ""
    }
    
    maxlimlabel <- try(paste("Max: ",
                             suppressWarnings(prettyNum(max(
                               x$AdjClose[lims], na.rm = T
                             ), big.mark = ",")),
                             "On" ,
                             format(as.Date(index(x[lims][suppressWarnings(which.max(x$AdjClose[lims]))])), "%b %d, %Y")))
    minlimlabel <-  try(paste("Min: ",
                              suppressWarnings(prettyNum(min(
                                x$AdjClose[lims], na.rm = T
                              ), big.mark = ",")),
                              "On" ,
                              format(as.Date(index(x[lims][suppressWarnings(which.min(x$AdjClose[lims]))])), "%b %d, %Y")))
    
    shiny::validate(
      need(nrow(x) > 0, "Missing Data Please Try Again"),
      need(length(lims) > 0, "Missing Data Please Try Again")
    )
    RVs$dy <- dygraph(x$AdjClose) %>%
      dyOptions(
        rightGap = 20,
        gridLineColor = "#DEDEDE",
        axisLineColor = "transparent",
        colors = c("#6495ED"),
        useDataTimezone = T
      )    %>%
      dyAxis(
        "x",
        drawGrid = F,
        valueFormatter = ' function (d) {
        x = moment.tz(d, "America/New_York").format(\'MMM, D YYYY\')
        return x
  }'
) %>%
      dyAxis(
        "y",
        valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
        axisLabelFormatter = 'function(d){return d.toFixed(2).replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
        gridLineColor = "#DEDEDE"
      ) %>%
      dyRangeSelector(retainDateWindow = T, dateWindow = NULL) %>%
      dyLimit(
        suppressWarnings(as.numeric(max(
          x$AdjClose[lims], na.rm = T
        ))),
        color = "green",
        label = maxlimlabel,
        labelLoc = "right"
      ) %>%
      dyLimit(
        suppressWarnings(as.numeric(min(
          x$AdjClose[lims], na.rm = T
        ))),
        color = "red",
        label = minlimlabel,
        labelLoc = "right"
      ) %>%
      dyLegend(dygraph,
               show = "auto",
               width = 250,
               labelsDiv = "dylabelesDiv")
    
    
})
}

