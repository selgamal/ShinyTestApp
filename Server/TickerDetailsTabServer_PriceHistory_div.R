# Server side for Price History

updateNumericInput(
  session = session,
  inputId = "priceHistory_perf_slct_Rf",
  max = Max_Rf/100
)


# Saving indicators inputs to overcome error when user leaves an input blank
# in this case the last value will be used and the blank is ignored to ensure
# inputs will always have value
observeEvent(
  {
    input$BBnPeriods
    input$BBsd
    input$smafast
    input$smaslow
    input$MACDFastPeroids
    input$MACDSlowPeroids
    input$MACDSignalPeriods
    input$RSIperiods
  },
  {
    if (!is.na(input$BBnPeriods)) {RVs$BBnPeriods <- input$BBnPeriods}
    if (!is.na(input$BBsd)) {RVs$BBsd <- input$BBsd}
    if (!is.na(input$smafast)) {RVs$smafast <- input$smafast}
    if (!is.na(input$smaslow)) {RVs$smaslow <-input$smaslow}
    if (!is.na(input$MACDFastPeroids)) {RVs$MACDFastPeroids <- input$MACDFastPeroids}
    if (!is.na(input$MACDSlowPeroids)) {RVs$MACDSlowPeroids <-input$MACDSlowPeroids}
    if (!is.na(input$MACDSignalPeriods)) {RVs$MACDSignalPeriods <-input$MACDSignalPeriods}
    if (!is.na(input$RSIperiods)) {RVs$RSIperiods <-input$RSIperiods}
  }
)


RVs$priceHistory_perf_slct_ErrMsg1x <- ""
RVs$priceHistory_perf_slct_ErrMsg2x <- ""
RVs$priceHistory_perf_slct_ErrMsg3x <- ""

observeEvent(
  {
    input$priceHistory_perf_slct_Rf
  },
  if(input$priceHistory_perf_slct_Rf < 0 || input$priceHistory_perf_slct_Rf > Max_Rf/100 || is.na(input$priceHistory_perf_slct_Rf)) {
    RVs$sharpeRf <- NA
    runjs(HTML('$("#priceHistory_perf_slct_Rf").css("background-color", "tomato")'))
    RVs$priceHistory_perf_slct_ErrMsg1x <- paste("<i class=\"fa fa-exclamation-triangle\"></i> For realistic result annual Risk Free rate must be a decimal between 0 and", Max_Rf/100, "(Max Risk Free Rate)")
  } else {
    RVs$priceHistory_perf_slct_ErrMsg1x <- ""
    runjs(HTML('$("#priceHistory_perf_slct_Rf").css("background-color", "")'))
    RVs$sharpeRf <-input$priceHistory_perf_slct_Rf
    
  }
)

observeEvent(
  {
    input$priceHistory_perf_slct_ConfLvl
  },
  if(input$priceHistory_perf_slct_ConfLvl < .8 || input$priceHistory_perf_slct_ConfLvl >= 1 || is.na(input$priceHistory_perf_slct_ConfLvl)) {
    RVs$conflvl <- NA
    runjs(HTML('$("#priceHistory_perf_slct_ConfLvl").css("background-color", "tomato")'))
    RVs$priceHistory_perf_slct_ErrMsg2x <- "<i class=\"fa fa-exclamation-triangle\"></i> For realistic result confidence level must be a decimal >= 0.8 and <1"
  } else {
    RVs$priceHistory_perf_slct_ErrMsg2x <- ""
    runjs(HTML('$("#priceHistory_perf_slct_ConfLvl").css("background-color", "")'))
    RVs$conflvl <-input$priceHistory_perf_slct_ConfLvl
    
  }
)

output$priceHistory_perf_slct_ErrMsg1 <- renderUI({
  tags$span(
    HTML(RVs$priceHistory_perf_slct_ErrMsg1x),
    style = "color:red;"
  )
})


output$priceHistory_perf_slct_ErrMsg2 <- renderUI({
  tags$span(
    HTML(RVs$priceHistory_perf_slct_ErrMsg2x),
    style = "color:red;"
  )
})

output$priceHistory_perf_slct_ErrMsg3 <- renderUI({
  tags$span(
    HTML(RVs$priceHistory_perf_slct_ErrMsg3x),
    style = "color:red;"
  )
})



# alert user to click to update chart

observeEvent(
  {
    input$BBnPeriods
    input$BBsd
    input$smafast
    input$smaslow
    input$MACDFastPeroids
    input$MACDSlowPeroids
    input$MACDSignalPeriods
    input$RSIperiods 
  },
  {
    if(!is.na(RVs$fs_ratio_slctd_tkr)) {
      runjs(HTML('$("#btn_UpdateChart1").css("background-color", "tomato")'))
    }
  }, ignoreInit = T
)


observeEvent(
  input$priceHistory_perf_slct_freq_inpt
  ,
  {
    a <- switch(
      input$priceHistory_perf_slct_freq_inpt,
      "1" = 252,
      "2" = 52,
      "3" = 12
    )
    RVs$PriceHitory_prd_factor <- a
  }
)

# properly formating date range used for qm chartseries to subset xts
observeEvent(
  input$candlestick_qm_daterange,
  {
    if (all(!is.na(input$candlestick_qm_daterange))) {
    RVs$Candlestick_qm_range <- paste(format(input$candlestick_qm_daterange[1]),format(input$candlestick_qm_daterange[2]) ,sep = "/")
    }
  }
)

observeEvent(
  {
    input$priceHistory_perf_slct_dateRng
    input$priceHistory_perf_slct_freq_inpt
    }
  ,
  {
     if(anyNA(input$priceHistory_perf_slct_dateRng) || !is.Date(try(as.Date(input$priceHistory_perf_slct_dateRng),silent = T))
        || input$priceHistory_perf_slct_dateRng[1]>input$priceHistory_perf_slct_dateRng[2] ) {
       
       RVs$priceHistory_perf_dateRng <- NA
       runjs(HTML("$('#priceHistory_perf_slct_dateRng > div > input').css('background-color', 'rgb(250, 139, 119)')"))
       RVs$priceHistory_perf_slct_ErrMsg3x <- "<i class=\"fa fa-exclamation-triangle\"></i> From date must be before To date with sufficient difference to produce more than 1 period according to selected Return Frequency"
     } else {
       
       RVs$priceHistory_perf_slct_ErrMsg3x <- "" 
       runjs(HTML("$('#priceHistory_perf_slct_dateRng > div > input').css('background-color', 'white')"))
      AdjDateRange <-
        switch(
          input$priceHistory_perf_slct_freq_inpt,
          "1" = input$priceHistory_perf_slct_dateRng,
          "2" = c(as.Date(ceiling_date(ymd(input$priceHistory_perf_slct_dateRng[1]), unit = "week")-days(1)),
                  as.Date(ceiling_date(ymd(input$priceHistory_perf_slct_dateRng[2]), unit = "week")-days(1))
          ),
          "3" = c(as.Date(ceiling_date(ymd(input$priceHistory_perf_slct_dateRng[1]), unit = "month")-days(1)),
                  as.Date(ceiling_date(ymd(input$priceHistory_perf_slct_dateRng[2]), unit = "month")-days(1))
          )
        )
      RVs$priceHistory_perf_dateRng <- paste(format(AdjDateRange[1]),format(AdjDateRange[2]) ,sep = "/")
    }
    
    
  }
)


observeEvent({
  RVs$conflvl
  RVs$sharpeRf
  RVs$priceHistory_perf_dateRng
},
{
  if(anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng))) {
    hideElement("pefromanceResultsContainer")
  } else {
    showElement("pefromanceResultsContainer")
  }
})



# extracting and properly formating the date range selected in highcharts chart
observeEvent(
  input$hc_candlestickSelection,
  {
    RVs$candlestick_hc_daterange <- paste(input$hc_candlestickSelection, collapse = "/")
  }
)

# Indicators inputs modal draggable
jqui_draggable("#Chart1Opts")

# show/hide charts based on selection
observeEvent(
  input$slct_chartpkg,
  {
    switch(
      input$slct_chartpkg,
     "1" = {
       hideElement("candlestickmain_hc_container")
       showElement("candlestickmain_qm_container")
       
      },
      "2" = {
        hideElement("candlestickmain_qm_container")
        showElement("candlestickmain_hc_container")
      }
    )
  }
)

#show hide divs when no selections are made to clean up the page

observeEvent(
  RVs$t,
  {
    if (!length(RVs$t)) {
      disable("btn_IndicatorsOpts")
      runjs(HTML('$("#btn_IndicatorsOpts").css("cursor", "no-drop")'))
      hideElement("CandlStickTableRow")
      hideElement("PriceHistoryPerformanceDiv")
      hideElement("valRatiosContainerDiv")
    } else {
      enable("btn_IndicatorsOpts")
      runjs(HTML('$("#btn_IndicatorsOpts").css("cursor", "pointer")'))
      showElement("CandlStickTableRow")
      showElement("PriceHistoryPerformanceDiv")
      showElement("valRatiosContainerDiv")
      runjs(HTML("$('.input-daterange > .form-control').prop('readonly', true)"))
      runjs(HTML("$('.input-daterange > .form-control').css('background-color', 'white')"))
    }
  }
)


observeEvent(RVs$PriceHistory,
             {
               req(nrow(RVs$PriceHistory)>=RVs$smaslow,
                   nrow(RVs$PriceHistory)>=RVs$smafast,
                   nrow(RVs$PriceHistory)>=14,
                   nrow(RVs$PriceHistory)>=26,
                   nrow(RVs$PriceHistory)>=RVs$BBnPeriods
                   ) ## Ensure enough data available for the calcs
               RVs$tkr.SMA.Slow <- SMA(RVs$PriceHistory[, 4], n = isolate(RVs$smaslow))
               RVs$tkr.SMA.Fast <- SMA(RVs$PriceHistory[, 4], n = isolate(RVs$smafast))
               RVs$tkr.RSI <- round(RSI(RVs$PriceHistory[, 4], n = 14),3)
               RVs$tkr.MACD <- round(MACD(RVs$PriceHistory[, 4], nFast = 12, nSlow = 26, nSig = 9),3)
               tkr.BBands <- round(BBands(RVs$PriceHistory[,2:4], n = isolate(RVs$BBnPeriods) , sd = isolate(RVs$BBsd)),3)
               tkr.Bbands <- data.frame(time = time(tkr.BBands), up = as.vector(tkr.BBands$up), dn =as.vector(tkr.BBands$dn) )
               tkr.Bbands$time <- datetime_to_timestamp(tkr.Bbands$time)
               RVs$tkr.Bbands_j = toJSON(x = as.matrix(na.omit(tkr.Bbands)))

             }

)



observeEvent(
  RVs$t,
  {
    updateSelectInput(session = session, inputId = "slct_CompareTkrSlct", choices = sort(RVs$t), selected = RVs$fs_ratio_slctd_tkr)
    RVs$CompareChartTkrsSlctd <- sort(RVs$t)[1]
  }
)


observeEvent(
  input$btn_updateCompareChart,
  {
    RVs$CompareChartTkrsSlctd <- input$slct_CompareTkrSlct
  }
)



#### updates charts on click ####

observeEvent(
  input$btn_UpdateChart1,
  {

    req(nrow(RVs$PriceHistory)>1, cancelOutput = T)
    RVs$tkr.SMA.Slow <- SMA(RVs$PriceHistory[, 4], n = isolate(RVs$smaslow))
    RVs$tkr.SMA.Fast <- SMA(RVs$PriceHistory[, 4], n = isolate(RVs$smafast))
    tkr.BBands <- round(BBands(RVs$PriceHistory[,2:4], n = isolate(RVs$BBnPeriods) , sd = isolate(RVs$BBsd)),3)
    tkr.Bbands <- data.frame(time = time(tkr.BBands), up = as.vector(tkr.BBands$up), dn =as.vector(tkr.BBands$dn) )
    tkr.Bbands$time <- datetime_to_timestamp(tkr.Bbands$time)
    RVs$tkr.Bbands_j = toJSON(as.matrix(na.omit(tkr.Bbands)))
    runjs(HTML('$("#btn_UpdateChart1").css("background-color", "")'))
  }
)


observeEvent(
  input$btn_UpdateChart1,
  {
    req(nrow(RVs$PriceHistory)>RVs$MACDSlowPeroids,
        nrow(RVs$PriceHistory)>RVs$RSIperiods,
        cancelOutput = T)
    RVs$tkr.MACD <- round(MACD(RVs$PriceHistory[, 4],
                               nFast = isolate(RVs$MACDFastPeroids),
                               nSlow = isolate(RVs$MACDSlowPeroids),
                               nSig = isolate(RVs$MACDSignalPeriods)
                               ),
                          3)
    RVs$tkr.RSI <- round(RSI(RVs$PriceHistory[, 4],
                             n = isolate(RVs$RSIperiods))
                         ,3)
  }
)


#### Ticker and benchmark monthly returns ####
observeEvent(
  { 
    input$priceHistory_perf_slct_dateRng
    input$priceHistory_perf_slct_freq_inpt
    input$priceHistory_perf_slct_bnchmk
    RVs$fs_ratio_slctd_tkr
  },
  {

    req(!is.null(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$AdjHistory$Close))
    req(is.Date(try(as.Date(input$priceHistory_perf_slct_dateRng),silent = T)))
    req(length(RVs$rets_assts_allperiods)>0)
    req(length(RVs$rets_bm_allperiods)>0)
    req(!is.na(RVs$priceHistory_perf_dateRng))
    
    b <- switch(
      input$priceHistory_perf_slct_bnchmk,
      "^GSPC" = "S&P500",
      "^IXIC" = "NASDAQ",
      "^DJI" = "DJI",
      "SPY" = "SPY"
    )
    
    a <- merge.xts(
      RVs$rets_assts_allperiods$Rets[[as.numeric(input$priceHistory_perf_slct_freq_inpt)]][,RVs$fs_ratio_slctd_tkr],
      RVs$rets_bm_allperiods$Rets[[as.numeric(input$priceHistory_perf_slct_freq_inpt)]][,b]
    )
    names(a) <- c(
      names(RVs$rets_assts_allperiods$Rets[[as.numeric(input$priceHistory_perf_slct_freq_inpt)]][,RVs$fs_ratio_slctd_tkr]
            ), 
      names(RVs$rets_bm_allperiods$Rets[[as.numeric(input$priceHistory_perf_slct_freq_inpt)]][,b]
      )
    )
    
    a <- na.omit(a)
    RVs$TkrBenchmarkReturns <- a[RVs$priceHistory_perf_dateRng]

  }
)

observeEvent(
  input$candlechartloaded,
  {
  runjs(HTML('$("#candlestickmain_hc_container").removeClass("recalculating")'))
  }
)

#### Candlestick Chart ####

#### Using Quantmod ChartSeries ####

output$candlestickmain_qm <- renderPlot({
  shiny::validate(need(length(RVs$fs_ratio_slctd_tkr) > 0, "NO DATA QM"))
  input$btn_UpdateChart1
  shiny::validate(need(length(RVs$t) > 0, "NO DATA QM"))
  shiny::validate(need(length(RVs$PriceHistory) > 0, "NO DATA QM"))
  req(length(RVs$PriceHistory) > 0)
  showElement("slct_chartpkg")
  showElement("candlestick_qm_daterange")
  runjs(HTML("$('.input-daterange > .form-control').prop('readonly', true)"))
  runjs(HTML("$('.input-daterange > .form-control').css('background-color', 'white')"))
  shiny::validate(need(!anyNA(input$candlestick_qm_daterange), "Enter Valid Dates"))
  shiny::validate(need(input$candlestick_qm_daterange[1]<input$candlestick_qm_daterange[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
  
  runjs(HTML('$("#candlestickmain_qm").addClass("recalculating")'))
  candleChart(RVs$PriceHistory[,1:5], name = RVs$fs_ratio_slctd_tkr)
  reChart(subset = RVs$Candlestick_qm_range)
  reChart(theme = chartTheme(theme = "white"))
  if(nrow(RVs$PriceHistory)>=isolate(RVs$smaslow) & nrow(RVs$PriceHistory)>=isolate(RVs$smafast)) {
  `Fast SMA` <- SMA(RVs$PriceHistory[, 4], n= isolate(RVs$smafast))
  `Slow SMA` <- SMA(RVs$PriceHistory[, 4], n= isolate(RVs$smaslow))
  plot(addTA(`Fast SMA`, col = NULL, legend = NULL))
  plot(addTA(`Fast SMA`, on=3, col = "red", legend = NULL))
  plot(addTA(`Slow SMA`, on=3, col = "blue", legend = NULL))
  }
  
  if(nrow(RVs$PriceHistory)>=isolate(RVs$BBnPeriods)) {
  plot(addBBands(n = isolate(RVs$BBnPeriods), sd = isolate(RVs$BBsd)))
  }
  
  if(nrow(RVs$PriceHistory)>=isolate(RVs$MACDFastPeroids) & nrow(RVs$PriceHistory)>=isolate(RVs$MACDSlowPeroids)) {
  plot(addMACD(fast = isolate(RVs$MACDFastPeroids), slow = isolate(RVs$MACDSlowPeroids), signal = isolate(RVs$MACDSignalPeriods) ))
  }
  
  if(nrow(RVs$PriceHistory)>=isolate(RVs$RSIperiods)) {
  plot(addTA(RSI(RVs$PriceHistory[, 4], n = isolate(RVs$RSIperiods)),col = "darkred", legend = paste("RSI (",isolate(RVs$RSIperiods),")", sep = "")))
  }

})


#### Using highcharts packages ####

selectionx <- JS(HTML("function() {
                      var xmin = Highcharts.dateFormat('%Y-%m-%d', this.xAxis[0].min);
                      var xmax = Highcharts.dateFormat('%Y-%m-%d', this.xAxis[0].max);
                      Shiny.onInputChange('hc_candlestickSelection', [xmin, xmax]);}"))
loadx <- JS(HTML("function() {
                 var loaded = Math.random();
                 Shiny.onInputChange('candlechartloaded', loaded);}"))


output$candlestickmain_hc <- renderHighchart({
  shiny::validate(need(length(RVs$fs_ratio_slctd_tkr) > 0, "NO DATA HC"))
  shiny::validate(need(length(RVs$t) > 0, "NO DATA HC"))
  shiny::validate(need(length(RVs$PriceHistory) > 0, "NO DATA HC"))
  req(length(RVs$PriceHistory) > 0)
  runjs(HTML('$("#candlestickmain_hc_container").css("background-color", "#F0F0F0;")'))
  runjs(HTML('$("#candlestickmain_hc_container").addClass("recalculating")'))
 hcPrice <-
   highchart(type = "stock") %>%
    hc_xAxis(labels = list(style = list(fontSize = "8px"))) %>%
    hc_yAxis_multiples(
      create_yaxis(
        4,
        heights = c(2,1,1,1),
        sep = .01
        ,
        turnopposite = T
        ,
        labels = list(
          list(
            style = list(fontSize = "8px"),
            format = '{value:.,f}'),
          list(
            style = list(fontSize = "8px")
            , formatter = JS("function(){return this.value/1000000 + 'M'}")
            )
          ,
          list(
            format = '{value:.2f}',
            style = list(fontSize = "8px")
            )
          , list(
            format = '{value}%',
            style = list(fontSize = "8px")
            )
        )
        ,
        title = list(
          list(
            text = "Price"
          )
          ,
          list(
            text = "Volume"
          )
          ,
          list(
            text = "MACD"
            )
          ,
          list(
            text = "RSI"
            )
        )
        ,
        plotLines = list(
          NA,
          NA,
          NA,
          list(
            list(
              value = 70,
              color = 'red',
              dashStyle = 'Dot',
              width = 2,
              label = list(text = "overbought (70%)", style = list(fontWeight = 'bold')),
              zIndex = 10
            ),
            list(
              value = 30,
              color = 'green',
              dashStyle = 'Dot',
              width = 2,
              label = list(text = "oversold (30%)", style = list(fontWeight = 'bold')),
              zIndex = 10
            )
          )
        )
      )
    ) %>%
    hc_add_series(
      data = RVs$PriceHistory,
      name = RVs$fs_ratio_slctd_tkr ,
      color = hex_to_rgba("darkred", 0.7),
      upColor = hex_to_rgba("darkgreen", 0.7),
      lineColor = hex_to_rgba("darkred", 0.7),
      upLineColor = hex_to_rgba("darkgreen", 0.7),
      symbolHeight = 0,
      symbolWidth = 0,
      symbolRadius = 0,
      tooltip = list(valueDecimals = 2)
    ) %>%
    hc_add_series(
      data = RVs$tkr.Bbands_j,
      type = "arearange",
      name = paste("Bollinger Bands", "(Periods=",isolate(RVs$BBnPeriods),", St. Dev.=",isolate(RVs$BBsd),")",sep =""),
      fillOpacity = 0.3,
      lineWidth = 0,
      yAxis = 0,
      tooltip = list(valueDecimals = 2)
    ) %>%
    hc_add_series(
      data = RVs$tkr.SMA.Slow,
      yAxis = 0,
      name = paste("Slow SMA", "(",isolate(RVs$smaslow),")",sep =""),
      color = hex_to_rgba("blue", 0.7),
      lineWidth = 1,
      tooltip = list(valueDecimals = 2)
    ) %>%
    hc_add_series(
      data = RVs$tkr.SMA.Fast,
      yAxis = 0,
      name = paste("Fast SMA", "(",isolate(RVs$smafast),")",sep =""),
      color = hex_to_rgba("red", 0.7),
      lineWidth = 1,
      tooltip = list(valueDecimals = 2)
    ) %>%
    hc_add_series(
      data = RVs$PriceHistory$Volume,
      color = "lightblue",
      yAxis = 1,
      name = "Volume",
      type = "column",
      borderWidth = 0,
      tooltip = list(valueDecimals = 0)
    ) %>%
    hc_add_series(
      data = RVs$tkr.MACD[, 1],
      yAxis = 2,
      name = paste("MACD", " (Fast =", isolate(RVs$MACDFastPeroids), ", Slow=", isolate(RVs$MACDSlowPeroids),")", sep ="") ,
      tooltip = list(valueDecimals = 2),
      zIndex = 15
    ) %>%
    hc_add_series(
      data = RVs$tkr.MACD[, 2],
      yAxis = 2,
      dashStyle = 'Dot',
      lineWidth = 1,
      color = 'darkred',
      name = paste("Signal Line", " (",isolate(RVs$MACDSignalPeriods),")",sep = ""),
      tooltip = list(valueDecimals = 2),
      zIndex = 10) %>%
    hc_add_series(
      data = RVs$tkr.MACD$macd - RVs$tkr.MACD$signal,
      yAxis = 2,
      type = "column",
      name = "MACD Histogram",
      tooltip = list(valueDecimals = 2),
      zIndex = 1
    ) %>%
    hc_add_series(
      data = RVs$tkr.RSI,
      yAxis = 3,
      name = paste("RSI", " (", isolate(RVs$RSIperiods), ")", sep = ""),
      color = hex_to_rgba("darkred", 0.7),
      lineWidth = 1,
      tooltip = list(valueDecimals = 2)
      ) %>%
    hc_legend(
      title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
      style = list(fontStyle = 'italic'),
      enabled = T,
      layout = "horizontal",
      align = "left",
      verticalAlign = "top",
      symbolPadding =1,
      symbolWidth=10,
      itemStyle = list(fontSize = "9px")
    ) %>%
    hc_rangeSelector(buttons = list(
      list(type = 'month', count = 1, text = '1M'),
      list(type = 'month', count = 3, text = '3M'),
      list(type = 'month', count = 6, text = '6M'),
      list(type = 'year', count = 1, text = '1Y'),
      list(type = 'year', count = 5, text = '5Y'),
      list(type = 'year', count = 7, text = '7Y'),
      list(type = 'all', text = 'All')
    ),
    selected = 3
    ) %>%
    hc_add_theme(thm) %>%
     hc_chart(
       events = list(
         load = loadx
         ,
         redraw = selectionx
       )
     )
 hcPrice$x$hc_opts$yAxis[[1]]$opposite <- F
 hcPrice$x$hc_opts$yAxis[[2]]$opposite <- F
 hcPrice$x$hc_opts$yAxis[[3]]$opposite <- F
 hcPrice$x$hc_opts$yAxis[[4]]$opposite <- F
 return(hcPrice)

})


output$CandlstickTable <-renderTable({
  req(RVs$fs_ratio_slctd_tkr)
  req(length(RVs$PriceHistory) > 0)
  shiny::validate(need(!anyNA(input$candlestick_qm_daterange), "Enter Valid Dates"))
  req(length(input$hc_candlestickSelection) > 0 || length(input$candlestick_qm_daterange) > 0)
  lims <-
    if (input$slct_chartpkg == 1) {
      shiny::validate(need(input$candlestick_qm_daterange[1]<input$candlestick_qm_daterange[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
      RVs$Candlestick_qm_range
    } else {
      RVs$candlestick_hc_daterange
    }
  n <- RVs$PriceHistory[lims]
  nx <- data.frame(
    Measure = c("Open","Close","Max", "Min", "Average Volume"),
    Date = c(
    format(as.Date(index(
        head(n,1))), "%b %d, %Y"),
    format(as.Date(index(
      tail(n,1))), "%b %d, %Y"),
    format(as.Date(index(
      n[which.max(n[, 2]), ]
    )), "%b %d, %Y"),
    format(as.Date(index(
      n[which.min(n[, 3]), ]
    )), "%b %d, %Y"),
    paste(format(as.Date(gsub("/.*", "", lims)), "%b %d, %Y"),
          format(as.Date(gsub(".*/","" ,lims)), "%b %d, %Y"),
          sep = "-")
    )
    ,
    Value =
      c(coredata(head(n[,1],1)),
        coredata(tail(n[,4],1)),
        coredata(n[which.max(n[, 2])][, 2]),
        coredata(n[which.min(n[, 3])][, 3]),
        prettyNum(round(mean(n[,5]),0),big.mark = ",")
    )
  ,
    stringsAsFactors = F
  )
  return(nx)

},
striped = T, width = "100%", colnames = F)


observeEvent(
  {
    RVs$TkrBenchmarkReturns
  },
  {
    shiny::validate(need(length(RVs$TkrBenchmarkReturns)>0, "NO DATA"))
    req(length(RVs$TkrBenchmarkReturns)>0)
    req(!is.na(RVs$sharpeRf))
    req(!is.na(RVs$conflvl))
    req(input$priceHistory_perf_slct_dateRng[1]<input$priceHistory_perf_slct_dateRng[2])
    tkrBm <- RVs$TkrBenchmarkReturns
    index(tkrBm) <- as.POSIXct(index(tkrBm))
    RVs$tkrPriceHistoryCAPMtbl <-
      table.CAPM(
        Ra = RVs$TkrBenchmarkReturns[,1],
        Rb = RVs$TkrBenchmarkReturns[,2],
        Rf = RVs$sharpeRf/RVs$PriceHitory_prd_factor,
        digits = 4
      )
    
    RVs$tkrPriceHistoryCAPMtbl <-
      data.frame(
        Name = row.names(RVs$tkrPriceHistoryCAPMtbl),
        value.a = RVs$tkrPriceHistoryCAPMtbl[,1],
        stringsAsFactors = F
      )

    RVs$calculatedavgeDD <-
      tryCatch(round(
        AverageDrawdown(
          R = tkrBm #[,1] #RVs$TkrBenchmarkReturns[,1]
        )#[,1]
        ,3), error = function(e) NA)

    RVs$calculatedDDlength <-
      tryCatch(round(
        AverageLength(
          R = tkrBm #[,1] #RVs$TkrBenchmarkReturns[,1]
        )#[,1]
        ,3), error = function(e) NA)

    RVs$calculatedDDrecov <-
      tryCatch(round(
        AverageRecovery(
          R = tkrBm #[,1] #RVs$TkrBenchmarkReturns[,1]
        )#[,1]
        ,3), error = function(e) NA)
    
    
    ### getting breaks for comparability####
    
    bPort <- hist(as.vector(RVs$TkrBenchmarkReturns[,1]), plot = F)$breaks
    bIndx <- hist(as.vector(RVs$TkrBenchmarkReturns[,2]), plot = F)$breaks
    strt <- min(c(bPort, bIndx))
    end <- max(c(bPort, bIndx))
    breaks <- seq(from = strt, to = end, length.out = length(bPort))
    
    hist.data <-
      hist(as.vector(RVs$TkrBenchmarkReturns[,1]), breaks = breaks ,plot=F)
    hist.data.bm <-
      hist(as.vector(RVs$TkrBenchmarkReturns[,2]), breaks = breaks ,plot=F)
    
    RVs$tkr_returnDist_df <-
    data.frame(x = hist.data$breaks[-length(hist.data$breaks)],
               y = c(hist.data$counts), 
               z = c(hist.data.bm$counts),
               name = paste(hist.data$breaks[1:(length(hist.data$breaks)-1)], 
                            hist.data$breaks[2:length(hist.data$breaks)],
                            sep = " to "
                            ),
               stringsAsFactors = F
    )
    
    RVs$tkr_returnDist_df_interval <- (max(hist.data$breaks)- min(hist.data$breaks))/length(hist.data$counts)
    
    tkrDetailsoutliers.tkr <- outliersPositions(c(coredata(RVs$TkrBenchmarkReturns[,1])))
    tkrDetailsoutliers.Rb <- outliersPositions(c(coredata(RVs$TkrBenchmarkReturns[,2])))
    
    RVs$tkr_details_Outliers <- list(Tkr = tkrDetailsoutliers.tkr, Rb= tkrDetailsoutliers.Rb)
    
    tkrDetails_TkrMean <- mean(RVs$TkrBenchmarkReturns[,1])
    tkrDetails_TkrSD <- sd(RVs$TkrBenchmarkReturns[,1])
    if(length(tkrDetailsoutliers.tkr)!=0) {
      tkrDetails_TkrMean.clean <- mean(RVs$TkrBenchmarkReturns[-tkrDetailsoutliers.tkr,1])
      tkrDetails_TkrSD.clean <- sd(RVs$TkrBenchmarkReturns[-tkrDetailsoutliers.tkr,1])
    } else {
      tkrDetails_TkrMean.clean <- tkrDetails_TkrMean
      tkrDetails_TkrSD.clean <- tkrDetails_TkrSD
      }

    tkrDetails_RbMean <- mean(RVs$TkrBenchmarkReturns[,2])
    tkrDetails_RbSD <- sd(RVs$TkrBenchmarkReturns[,2])
    if(length(tkrDetailsoutliers.Rb) != 0) {
      tkrDetails_RbMean.clean <- mean(RVs$TkrBenchmarkReturns[-tkrDetailsoutliers.Rb,2])
      tkrDetails_RbSD.clean <- sd(RVs$TkrBenchmarkReturns[-tkrDetailsoutliers.Rb,2])
    } else {
      tkrDetails_RbMean.clean <- tkrDetails_RbMean
      tkrDetails_RbSD.clean <- tkrDetails_RbSD
    }

    RVs$tkrDetails_MeanSD <- data.frame(
      Name = c(
        "Returns Mean Including Outliers",
        "Returns Standard Deviation Including Outliers",
        "Returns Mean Excluding Outliers",
        "Returns Standard Deviation Excluding Outliers"
      ),
      value.r = c(
          tkrDetails_RbMean,
          tkrDetails_RbSD,
          tkrDetails_RbMean.clean,
          tkrDetails_RbSD.clean
      ),
      value.a = c(
          tkrDetails_TkrMean,
          tkrDetails_TkrSD,
          tkrDetails_TkrMean.clean,
          tkrDetails_TkrSD.clean
      ),
      stringsAsFactors = F
    )
    
  }
)

output$perfmetricstbl <- renderTable({
  shiny::validate(need(!anyNA(input$priceHistory_perf_slct_dateRng), "Enter Valid Dates"))
  shiny::validate(need(input$priceHistory_perf_slct_dateRng[1]<input$priceHistory_perf_slct_dateRng[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
  req(length(RVs$TkrBenchmarkReturns)>0, cancelOutput = T)
  req(!anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng)))
  a <- switch(
    input$priceHistory_perf_slct_freq_inpt,
    "1" = "(in days)",
    "2" = "(in weeks)",
    "3" = "(in months)"
  )
  df <- data.frame(
    Name = c(
             "Avg. Drawdown",
             paste("Drawdown Duaration",a, sep = " "),
             paste("Drawdown recovery",a,sep = " ") 
             ),
    value.r = c(
                RVs$calculatedavgeDD[2],
                RVs$calculatedDDlength[2],
                RVs$calculatedDDrecov[2]
    ),
    value.a = c(
              RVs$calculatedavgeDD[1],
              RVs$calculatedDDlength[1],
              RVs$calculatedDDrecov[1]
              
              ),
    stringsAsFactors = F
  )
  
  df <- bind_rows(
    df,
    RVs$tkrPriceHistoryCAPMtbl,
    RVs$tkrDetails_MeanSD
  )
  
  df <- filter(df, !Name %in% c("Tracking Error", "Correlation p-value", "Beta+", "Beta-", "Annualized Alpha") )
  
  names(df) <- c("",names(RVs$TkrBenchmarkReturns)[2], names(RVs$TkrBenchmarkReturns)[1])
  
  return(df)
  
}, striped = T, width = "100%", colnames = T, digits = 3, na = "")

output$calculatedsharpetbl <- renderTable({
  req(RVs$fs_ratio_slctd_tkr)
  req(!anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng)))
  req(!is.na(RVs$sharpeRf))
  req(!is.na(RVs$conflvl))
  
  shiny::validate(need(!anyNA(input$priceHistory_perf_slct_dateRng), "Enter Valid Dates"))
  shiny::validate(need(input$priceHistory_perf_slct_dateRng[1]<input$priceHistory_perf_slct_dateRng[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
  shiny::validate(need(length(RVs$TkrBenchmarkReturns)>0, "NO DATA"))
  req(length(RVs$TkrBenchmarkReturns)>0)
  a <- round(
            SharpeRatio(
              R = RVs$TkrBenchmarkReturns, #[,1],
              Rf = RVs$sharpeRf/RVs$PriceHitory_prd_factor,
              p = RVs$conflvl
            )
            ,3)
  a <- data.frame(gsub(":","", 
                       rownames(a)), a[,2], a[,1], stringsAsFactors = F)
  names(a) <- c("",names(RVs$TkrBenchmarkReturns)[2], names(RVs$TkrBenchmarkReturns)[1])
  return(a)
}, striped = T, width = "100%", colnames = F, digits = 3, na = "")

output$tkrDetails_VaR <- renderTable({
  req(RVs$fs_ratio_slctd_tkr)
  req(!anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng)))
  req(!is.na(RVs$conflvl))
  shiny::validate(need(length(RVs$TkrBenchmarkReturns)>0, "NO DATA"))
  shiny::validate(need(!anyNA(input$priceHistory_perf_slct_dateRng), "Enter Valid Dates"))
  shiny::validate(need(input$priceHistory_perf_slct_dateRng[1]<input$priceHistory_perf_slct_dateRng[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
  req(length(RVs$TkrBenchmarkReturns)>0)
  
  VaRs <-
    mapply(function (x,y) {
      a <- VaR(R = RVs$TkrBenchmarkReturns,
               p = RVs$conflvl,
               method = x)
      a <- data.frame(method = paste(y, " (", "p=", RVs$conflvl *100,"%",")", sep = ""), 
                      value.b = a[1,2],
                      value.a = a[1,1],
                      stringsAsFactors = F)
      
    }, 
    x= c("modified", "gaussian", "historical"),
    y = c("Modified Method", "Gaussian Method", "Historical Method"),
    SIMPLIFY = F)
  VaRs <- do.call(rbind.data.frame, VaRs)
  rownames(VaRs) <- NULL
  names(VaRs) <- c("",names(RVs$TkrBenchmarkReturns)[2], names(RVs$TkrBenchmarkReturns)[1])
  return(VaRs)
}, striped = T, width = "100%", colnames = F, digits = 3, na = "")

output$tkrDetails_ES <- renderTable({
  req(RVs$fs_ratio_slctd_tkr)
  req(!anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng)))
  req(!is.na(RVs$conflvl))
  shiny::validate(need(length(RVs$TkrBenchmarkReturns)>0, "NO DATA"))
  shiny::validate(need(!anyNA(input$priceHistory_perf_slct_dateRng), "Enter Valid Dates"))
  shiny::validate(need(input$priceHistory_perf_slct_dateRng[1]<input$priceHistory_perf_slct_dateRng[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
  req(length(RVs$TkrBenchmarkReturns)>0)
  
  CVaRs <-
    mapply(function (x,y) {
      a <-suppressMessages(CVaR(R = RVs$TkrBenchmarkReturns,
               p = RVs$conflvl,
               method = x))
      a <- data.frame(method = paste(y, " (", "p=", RVs$conflvl *100,"%",")", sep = ""), 
                      value.b = a[1,2],
                      value.a = a[1,1],
                      stringsAsFactors = F)
      
    }, 
    x= c("modified", "gaussian", "historical"),
    y = c("Modified Method", "Gaussian Method", "Historical Method"),
    SIMPLIFY = F)
  CVaRs <- do.call(rbind.data.frame, CVaRs)
  rownames(CVaRs) <- NULL
  names(CVaRs) <- c("",names(RVs$TkrBenchmarkReturns)[2], names(RVs$TkrBenchmarkReturns)[1])
  return(CVaRs)
}, striped = T, width = "100%", colnames = F, digits = 3, na = "")



output$PerformanceChart <- renderPlot({
  shiny::validate(need(length(RVs$TkrBenchmarkReturns)>0, "NO DATA"))
  shiny::validate(need(input$priceHistory_perf_slct_dateRng[1]<input$priceHistory_perf_slct_dateRng[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
  req(RVs$fs_ratio_slctd_tkr)
  req(!anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng)))
  req(length(RVs$TkrBenchmarkReturns)>0)
  showElement("perfmetrics")
  showElement("betadiv")
  showElement("sharperatio")
  plot.new()
  par(bg = NA, mgp = c(2,.5,0),mar = c(0,0,0,0), mai = c(0,0,0.5,0))
  charts.PerformanceSummary(RVs$TkrBenchmarkReturns
                            , colorset= chartColors
                            , lwd=2
                            , ylog=TRUE
                            , cex.lab = 1.5
                            , cex.main = 1.5
                            , cex.axis = 1.2
                            , legend.loc = NULL
                            ,  main = ""
  )
  mtext(paste(
    format(as.Date(min(index(RVs$TkrBenchmarkReturns))), "%b %d, %Y"),
    format(as.Date(max(index(RVs$TkrBenchmarkReturns))), "%b %d, %Y"),
    sep = " to "
  ), cex = .75
  )
  title(main = paste(RVs$fs_ratio_slctd_tkr, " Performance ", sep = ""), line = 1, cex.main = 1)
})

output$PerformanceChartLegend <- renderPlot({
  req(RVs$fs_ratio_slctd_tkr)
  req(!anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng)))
  req(length(RVs$TkrBenchmarkReturns)>0)
  a <- names(RVs$TkrBenchmarkReturns)[1]
  b <- names(RVs$TkrBenchmarkReturns)[2]
  par(mar=c(0,0,0,0), bg = "#F0F0F0")
  plot.new()
  legend("bottom",
         legend = c(a, b),
         cex=1,pt.cex = 1,
         col = chartColors,
         horiz = T,
         bty = 'n',
         lty=c(1,1),
         lwd = 2
  )
})


output$tkrReturnBoxplot <-
  renderHighchart({
    req(!anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng)))
    shiny::validate(need(nrow(RVs$TkrBenchmarkReturns) > 0, "NO DATA"),
                    need(!is.null(RVs$fs_ratio_slctd_tkr), "Select Ticker"))
    shiny::validate(need(!anyNA(input$priceHistory_perf_slct_dateRng), "Enter Valid Dates"))
    shiny::validate(need(input$priceHistory_perf_slct_dateRng[1]<input$priceHistory_perf_slct_dateRng[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
    req(RVs$fs_ratio_slctd_tkr)
    bmname <- JS(HTML(names(RVs$TkrBenchmarkReturns)[2]))
    tkrRets <- melt(as.data.frame(RVs$TkrBenchmarkReturns), id.vars = NULL) %>% 
      mutate(variable = as.character(variable)) #c(coredata(RVs$TkrBenchmarkReturns[,1]))
    box <-hcboxplot.mod(
            x = round(tkrRets$value, 4),
            var = tkrRets$variable, #rep(RVs$fs_ratio_slctd_tkr, length(tkrRets)),
            name = " ",
            fillColor = 'rgb(169,169,169)',
            medianColor = 'white',
            medianWidth = 3,
            stemColor = 'red',
            stemDashStyle = 'dot',
            stemWidth = 1,
            whiskerColor = 'red',
            whiskerLength = '20%',
            whiskerWidth = 1
          ) %>%
      hc_add_theme(thm) %>%
            hc_tooltip(
              headerFormat = "<b>{point.key}'s Metrics</b><br/>",
              pointFormat= "
                             Maximum: {point.high}<br/>
                             Upper Quartile: {point.q3}<br/>
                             Median: {point.median}<br/>
                             Lower Quartile: {point.q1}<br/>
                             Minimum: {point.low}<br/>"
              ) %>%
            hc_yAxis(labels = list(format = '{value:,.2f}'), gridLineDashStyle = "Dash", gridLineColor = "rgb(211, 211, 211)") %>%
      hc_xAxis(gridLineDashStyle = "Dash", gridLineColor = "rgb(211, 211, 211)",
               labels = list(
                 formatter = JS(HTML(paste(
                   "function () {
                    if (\"",bmname, "\"=== this.value) {
                    return '<span style=\"fill: #3ec224;\">' + this.value + '</span>';
                    } else {
                    return this.value;
                    }
                    }
                   ", sep ="")
                 ))
                 )
               
               ) %>%
      hc_title(
        text = paste(RVs$fs_ratio_slctd_tkr, " Returns Boxplot", sep = "")
      )  %>%
      hc_subtitle(
        text = paste(
          format(as.Date(min(index(RVs$TkrBenchmarkReturns))), "%b %d, %Y"),
          format(as.Date(max(index(RVs$TkrBenchmarkReturns))), "%b %d, %Y"),
          sep = " to "
        )
      )
    try({
          box$x$hc_opts$series[[2]]$tooltip$pointFormat <- "<b>{point.name}'s Outliers:</b> {point.y:.4f}<br/>"
          box$x$hc_opts$series[[2]]$tooltip$headerFormat <- ""
          box$x$hc_opts$series[[2]]$color <- "red"
    }, silent = T)
    
    datasublist <-box$x$hc_opts$series[[1]]$data
    
    for(i in 1:length(datasublist)) {
      bm <- datasublist[[i]]$name
      if (bm == bmname ) {break();}
    }
    
    
    sublistbm <- datasublist[[i]]
    
    datasublist[[i]] <- NULL
    
    datasublistx <- append(list(sublistbm), datasublist)
    
    
    box$x$hc_opts$series[[1]]$data <- datasublistx

    return(box)
  })

output$tkrReturnDistributionChart <-
  renderHighchart(
    {
      req(!anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng)))
      shiny::validate(need(nrow(RVs$TkrBenchmarkReturns)>0, "NO DATA"),
                      need(!is.null(RVs$fs_ratio_slctd_tkr), "Select Ticker"),
                      need(nrow(RVs$tkr_returnDist_df)>0, "NO DATA"))
      shiny::validate(need(!anyNA(input$priceHistory_perf_slct_dateRng), "Enter Valid Dates"))
      shiny::validate(need(input$priceHistory_perf_slct_dateRng[1]<input$priceHistory_perf_slct_dateRng[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
      req(RVs$fs_ratio_slctd_tkr)
      req(!is.na(RVs$conflvl))
      xVaRs <- VaR(R = RVs$TkrBenchmarkReturns, p = RVs$conflvl, method = "modified" )
      
      highchart() %>%
        hc_add_theme(thm) %>%
        hc_add_series(
          data = RVs$tkr_returnDist_df,
          hcaes(x = x, y = y, name = name),
          type = "column",
          pointPadding = 0,
          groupPadding = 0,
          pointRange = RVs$tkr_returnDist_df_interval,
          pointPlacement = 'between',
          name = paste(names(RVs$TkrBenchmarkReturns)[1]," Returns")
        ) %>%
        hc_plotOptions(column = list(borderWidth = 1)) %>%
        hc_add_series(
          #RVs$tkr_returnDist_df_norm,
          data = RVs$tkr_returnDist_df,
          hcaes(x=x, y=z, name = name),
          marker = list(enabled = F),
          type = "spline",
          color = "rgb(144, 237, 125)",
          name = paste(names(RVs$TkrBenchmarkReturns)[2], " Returns"),
          tooltip = list(valueDecimals = 0)
        ) %>%
        hc_xAxis(labels = list(format = '{value:,.2f}'),
                 tickInterval= RVs$tkr_returnDist_df_interval,
                 gridLineDashStyle = "Dash", gridLineColor = "rgb(211, 211, 211)",
                 plotLines = list(
                   list(
                     label = list(text = paste(names(RVs$TkrBenchmarkReturns)[1],
                                               " Mod. VaR ",
                                               RVs$conflvl *100, 
                                               "%", sep = ""),
                                  style = list(`font-size` = "10px")
                     ), 
                     color = "red",
                     width = 1,
                     value = as.numeric(xVaRs[1,1]),
                     zIndex = 5,
                     dashStyle = "ShortDot"),
                   list(
                     label = list(text = paste(names(RVs$TkrBenchmarkReturns)[2],
                                               " Mod. VaR ",
                                               RVs$conflvl *100, 
                                               "%", sep = "" ),
                                  style = list(`font-size` = "10px")
                     ), 
                     color = "black",
                     width = 1,
                     value = as.numeric(xVaRs[1,2]),
                     zIndex = 5,
                     dashStyle = "ShortDot"
                   )
                 )
                 
                 
                 )  %>%
        hc_yAxis(gridLineDashStyle = "Dash",
                 gridLineColor = "rgb(211, 211, 211)")  %>%
        hc_title(
          text = paste(names(RVs$TkrBenchmarkReturns)[1], " Returns Distribution", sep = "")
        ) %>%
        hc_subtitle(
          text = paste(
            format(as.Date(min(index(RVs$TkrBenchmarkReturns))), "%b %d, %Y"),
            format(as.Date(max(index(RVs$TkrBenchmarkReturns))), "%b %d, %Y"),
            sep = " to "
            )
        ) %>%
        hc_tooltip(shared = T)
    }
  )


output$tkrDetails_VaRsens <- renderHighchart(
  {
    req(!anyNA(c(RVs$conflvl,RVs$sharpeRf, RVs$priceHistory_perf_dateRng)))
    shiny::validate(need(nrow(RVs$TkrBenchmarkReturns)>0, "NO DATA"),
                    need(!is.null(RVs$fs_ratio_slctd_tkr), "Select Ticker"))
    shiny::validate(need(!anyNA(input$priceHistory_perf_slct_dateRng), "Enter Valid Dates"))
    shiny::validate(need(input$priceHistory_perf_slct_dateRng[1]<input$priceHistory_perf_slct_dateRng[2], "From date MUST be before TO date and sufficient to produce MORE than one period according to selected periodicity"))
    req(RVs$fs_ratio_slctd_tkr)
    Pseq <-seq(0.99, 0.89, by = -0.005)
    VaRsens <-
      mapply(function (x) {
        xx <- data.frame(method=c(), name = c(), p = c(), VaR = c(), stringsAsFactors = F )
        for(i in Pseq) {
          a <- VaR(R = RVs$TkrBenchmarkReturns[,1],
                   p = i,
                   method = x)
          a <- data.frame(method = paste("VaR",x),
                          name = paste("p=", format(i*100, nsmall = 2), "%", 
                                       sep = ""),
                          p = i, VaR = round(a[1,1],4), 
                          stringsAsFactors = F )
          xx <- rbind.data.frame(xx, a)
        }
        return(xx)
      }, 
      x= c("modified", "gaussian", "historical"),
      SIMPLIFY = F)
    
    
    VaRsens.df = do.call(rbind.data.frame, VaRsens)
    
    rownames(VaRsens.df) <- NULL
    
    
    CVaRsens <-
      mapply(function (x) {
        xx <- data.frame(method=c(), name = c(), p = c(), CVaR = c(), stringsAsFactors = F )
        for(i in Pseq) {
          a <- suppressMessages(CVaR(R = RVs$TkrBenchmarkReturns[,1],
                    p = i,
                    method = x))
          a <- data.frame(method = paste("ES",x),
                          name = paste("p=", format(i*100, nsmall = 2), "%", 
                                       sep = ""),
                          p = i, CVaR = round(a[1,1],4), 
                          stringsAsFactors = F )
          xx <- rbind.data.frame(xx, a)
        }
        return(xx)
      }, 
      x= c("modified", "gaussian", "historical"),
      SIMPLIFY = F)
    
    
    CVaRsens.df = do.call(rbind.data.frame, CVaRsens)
    
    rownames(CVaRsens.df) <- NULL
    
    CVaRsens.df.percent <- CVaRsens.df
    CVaRsens.df.percent$p <- round(CVaRsens.df.percent$p*100,2)
    CVaRsens.df.percent$CVaR <- round(CVaRsens.df.percent$CVaR*100,5)
    
    VaRsens.df.percent <- VaRsens.df
    VaRsens.df.percent$p <- round(VaRsens.df.percent$p*100,2)
    VaRsens.df.percent$VaR <- round(VaRsens.df.percent$VaR*100,5)
    
    
    highchart() %>%
      hc_add_series(data = VaRsens.df.percent, 
                    hcaes(x=p, y=VaR,group= method), 
                    type="line", 
                    marker = list(enabled=F),
                    lineWidth = 1,
                    dashStyle = c("ShortDash","ShortDot", "Solid")
      ) %>%
      hc_add_series(data = CVaRsens.df.percent, 
                    hcaes(x=p, y=CVaR,group= method), 
                    type="line", 
                    marker = list(enabled=F),
                    lineWidth = 1,
                    dashStyle = c("ShortDash","ShortDot", "Solid")
      ) %>%
      hc_xAxis(
        labels = list(format = "{value:.1f}%", fontSize = "8px"),
        title = list(text = "Confidence Level", style = list(fontSize="9px")),
        gridLineDashStyle = "Dash", gridLineColor = "rgb(211, 211, 211)"
      )%>%
      hc_yAxis(
        labels = list(format = "{value}%", fontSize = "8px"),
        title = list(text = "VaR/ES", style = list(fontSize=9)),
        gridLineDashStyle = "Dash", gridLineColor = "rgb(211, 211, 211)",
        plotLines = list(
          list(
            label = list(text = "Unreliable Results",
                         style = list(`font-size` = "10px", color = "red")
            ), 
            color = "red",
            width = 1,
            value = -100,
            zIndex = 5,
            dashStyle = "Solid")
        )
      )%>%
      hc_tooltip(shared = T, valueSuffix = "%")%>%
      hc_add_theme(thm)%>%
      hc_title(
        text = paste(names(RVs$TkrBenchmarkReturns)[1], " VaR/ES Sensitivity", sep = "")
      ) %>%
      hc_legend(
        itemStyle = list(fontSize = "8px")
      ) %>%
      hc_subtitle(
        text = paste(
          format(as.Date(min(index(RVs$TkrBenchmarkReturns))), "%b %d, %Y"),
          format(as.Date(max(index(RVs$TkrBenchmarkReturns))), "%b %d, %Y"),
          sep = " to "
        )
      )
  }
)

output$tkrDetails_valutionRatios_tbl <- renderTable({
  shiny::validate(need(length(RVs$tkrHistoryData)>0, "NO DATA"),
                  need(!is.null(RVs$fs_ratio_slctd_tkr), "Select Ticker"),
                  need(length(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$ratios)>0, "NO DATA")
  )
  req(RVs$fs_ratio_slctd_tkr)
  a <-
    filter(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$ratios, group %in% 86:89)
  
  a <- a[, 3:ncol(a)]
  
  a$date <- as.character(a$date, format = "%Y")
  a <- dcast(a, category ~ date, sum)
  a[,2:ncol(a)] <- format(round(a[,2:ncol(a)],2), nsmall =2)
  names(a)[1] <- "Ratio/Year"
  return(a)
}, striped = T, width = "100%", colnames = T, digits = 3, na = "")


output$tkrDetails_valuationRatios <- renderHighchart(
  {
    shiny::validate(need(length(RVs$tkrHistoryData)>0, "NO DATA"),
                    need(!is.null(RVs$fs_ratio_slctd_tkr), "Select Ticker"),
                    need(length(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$ratios)>0, "NO DATA")
                                )
    req(RVs$fs_ratio_slctd_tkr)

    a <-
      filter(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$ratios, group %in% 86:89)

    a <- a[, 3:ncol(a)]

    a$date <- as.character(a$date, format = "%Y")

    cat <- unique(a$category)

    a <- rbind(data.frame(
      group = c(86:89)
      ,
      category = cat
      ,
      date = c(rep(1901))
      ,
      value = c(rep(NA))
    )
    ,
    a)

    hchart_valRatios <-
      highchart() %>%
      hc_xAxis(
        categories = unique(a$date),
        floor = 1,
        labels = list(style = list(fontSize = "8px")),
        gridLineDashStyle = "Dash", gridLineColor = "rgb(211, 211, 211)"
      ) %>%
      hc_yAxis_multiples(
        list(
          labels = list(style = list(fontSize = "8px")
                        , format = '{value:.,f}')
          ,
          title = list(text = ""), gridLineDashStyle = "Dash", gridLineColor = "rgb(211, 211, 211)"
        ),
        list(
          labels = list(style = list(fontSize = "8px")
                        , format = '{value:.,f}')
          ,
          title = list(text = NULL),
          opposite = T
          , gridLineDashStyle = "Dash", gridLineColor = "rgb(211, 211, 211)"
        )
      ) %>%
      hc_add_series(
        a[a$category == "Price to Earnings" , ],
        type = "column" ,
        hcaes(x = date, y = value, group = category),
        tooltip = list(valueDecimals = 2)
      ) %>%
      hc_add_series(
        a[a$category != "Price to Earnings", ],
        type = "spline",
        hcaes(x = date, y = value, group = category),
        yAxis = 1,
        tooltip = list(valueDecimals = 2)
      ) %>%
      hc_plotOptions(
        column = list(
          borderWidth = 0,
          dataLabels = list(
            enabled = T,
            format = '{point.y:.2f}'
            ,
            style = list(fontSize = "8px", textOutline = F)
            ,
            inside = F,
            y = 3,
            color = "#666666"
          )
        )
        ,
        series = list(pointWidth = 30)

      ) %>%
      hc_legend(itemStyle = list(fontSize = "10px"),
                title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
                style = list(fontStyle = 'italic'),
                labelFormatter = JS("function (){
                                        if (this.name == \"Price to Earnings\") {
                                                return this.name + \" (lower is better)\";
                                        } else {
                                          return this.name
                                        };
                                      }
                                    
                                    ")
                ) %>%
      hc_tooltip(shared = T) %>%
      hc_add_theme(thm) %>%
      hc_title(text = "A totally useless chart") %>%
      hc_subtitle(text = "Since I had the data...")
    hchart_valRatios$x$conf_opts$lang$thousandsSep <- ","
    hchart_valRatios
  }
)

output$DivHistory <- renderPrint({req(RVs$fs_ratio_slctd_tkr,cancelOutput = T) 
                                  shiny::validate(need(!is.na(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$dividends), "NO DATA"))
                                  #req(length(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$dividends)>0)
                                  a <-RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$dividends
                                  a[,1] <- format(round(a,3), nsmall = 3)
                                  a <- data.frame(format(index(a), "%d-%b-%Y"),coredata(a), stringsAsFactors = F)
                                  names(a) <- c("","")
                                  a <- print.data.frame(a,row.names = F)
                                  })
output$SpltHistory <- renderPrint({
                                  req(RVs$fs_ratio_slctd_tkr, cancelOutput = T)
                                  shiny::validate(need(!is.na(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$splits), "NO DATA"))
                                  #req(length(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$splits)>0)
                                  a <- round(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$splits,3)
                                  a[,1] <- format(round(a,3), nsmall = 3)
                                  a <- data.frame(format(index(a), "%d-%b-%Y"),coredata(a), stringsAsFactors = F)
                                  names(a) <- c("","")
                                  a <- print.data.frame(a,row.names = F)
                                  }
                                  )