## Server for portfolio tab section one

# Setup selection of chart type


observeEvent(
  input$slct_chartpkg_port,
  {
    switch(
      input$slct_chartpkg_port,
      "1" = {
        hideElement("div_comapreTkrs_hc") 
        showElement("div_comapreTkrs_pa") 
      },
      "2" = {
        hideElement("div_comapreTkrs_pa") 
        showElement("div_comapreTkrs_hc") 
      }
    )
  }
)


# change color of update button to alert user to click it
port_updtbtnCol_obs <-
    observeEvent(
      {
        input$slct_CompareTkrSlct
        input$slct_benchmark_port
        input$slct_retfreq
      },
      {
        if(
          (length(input$slct_CompareTkrSlct)>0) && (
          !identical(RVs$CompareChartTkrsSlctd,input$slct_CompareTkrSlct) |
          !identical(RVs$slct_retfreq,input$slct_retfreq) |
          !identical(RVs$slct_benchmark_port, input$slct_benchmark_port)
          )
        ) {
        runjs(HTML('$("#btn_updateCompareChart").css("background-color", "tomato")'))
        }
        
      }, ignoreInit = T #, suspended = T
    )


# Store Name and data of the benchmark selected

RVs$slct_benchmark_port <- "^GSPC" # intialize reactive value
RVs$port_hc_daterange <- c(MaxDate_Global-years(1), MaxDate_Global) # intialize reactive value




# This observer triggers the changes in date range of charts and translate the selected ranges
# to values accomedating the return frequency, if monthly is selected the date range is 
# month end to month end, weekly then weekend to weekend, note that in creating the main
# data table, in case of monthly/weekly return frequency dates are recalculated to reflect 
# month end and weekend insetead of last day with price... this makes it easier to compare

observeEvent(
  {
    input$hc_Selection_port
    input$comapreTkrs_pa_daterange
    input$slct_chartpkg_port
    #input$slct_retfreq
     RVs$slct_retfreq
    
  }
  ,
  {
    # Detect which date range to use (from Highcharts or from PA chart)
    dateRangeUsed <-
      if (input$slct_chartpkg_port == 1) {
        c(input$comapreTkrs_pa_daterange[1], input$comapreTkrs_pa_daterange[2])
      } else {
        # deal with intial null value of selection range in Highcharts chart
        if(is.null(input$hc_Selection_port)) {RVs$port_hc_daterange} else {input$hc_Selection_port}
      }
    
    AdjDateRange <-
      switch(
        input$slct_retfreq,
        "1" = dateRangeUsed,
        "2" = c(as.Date(ceiling_date(ymd(dateRangeUsed[1]), unit = "week")-days(1)),
                as.Date(ceiling_date(ymd(dateRangeUsed[2]), unit = "week")-days(1))
        ),
        "3" = c(as.Date(ceiling_date(ymd(dateRangeUsed[1]), unit = "month")-days(1)),
                as.Date(ceiling_date(ymd(dateRangeUsed[2]), unit = "month")-days(1))
        )
      )
    RVs$port_Charts_Lims <- AdjDateRange
  }
)


# Update selector used to add tickers to first chart

observeEvent(
  RVs$t,
  {
    slctd <- 
      if(length(input$slct_CompareTkrSlct)>0) {
        intersect(input$slct_CompareTkrSlct, RVs$t)
      } else {
        na.omit(sort(RVs$t[1:3]))
      }
    
    updateSelectizeInput(session = session, 
                      inputId = "slct_CompareTkrSlct", 
                      choices = sort(RVs$t), 
                      selected = sort(slctd))
    updateSelectizeInput(session = session, 
                      inputId = "port_sectionTwo_Optim_slctTkrs", 
                      choices = sort(RVs$t), 
                      selected = sort(slctd)
    )
    RVs$CompareChartTkrsSlctd <- slctd
    RVs$slct_retfreq <- input$slct_retfreq
   
  }
)

# Update chart and table based on selected tickers, return frequency and benchmark
observeEvent(
  input$btn_updateCompareChart,
  {
    # these variables will trigger recalc of dataset and in turn charts and table
    RVs$CompareChartTkrsSlctd <- input$slct_CompareTkrSlct
    RVs$slct_retfreq <- input$slct_retfreq
    RVs$slct_benchmark_port <- input$slct_benchmark_port
    runjs(HTML('$("#btn_updateCompareChart").css("background-color", "")'))
  }
)


# Creating benchmark dataset suitable for each chart type, end result is a list with elements "hc" suitable for
# highcharts and pa suitable for performance analytics charts [could be more efficient].

observeEvent(
  {
    RVs$slct_benchmark_port
  },
  {
    bm_name <- switch(
      RVs$slct_benchmark_port,
      "^GSPC" = "S&P500",
      "^IXIC" = "NASDAQ",
      "^DJI" = "DJI",
      "SPY" = "SPY"
    )
    
    RVs$bm_name_port <- bm_name

})


observeEvent(
  {
    RVs$rets_bm_allperiods
    RVs$rets_assts_allperiods
    RVs$slct_retfreq
  },
  {
    a <- merge.xts(
      RVs$rets_assts_allperiods$Rets[[as.numeric(RVs$slct_retfreq)]],
      RVs$rets_bm_allperiods$Rets[[as.numeric(RVs$slct_retfreq)]]
      )
    names(a) <- c(
      names(RVs$rets_assts_allperiods$Rets[[as.numeric(RVs$slct_retfreq)]]), 
      names(RVs$rets_bm_allperiods$Rets[[as.numeric(RVs$slct_retfreq)]]
            )
      )
    RVs$Port_RaRb_lst <- a
    
  }
)



selection_port <- JS(HTML("function() {
                      var xmin = Highcharts.dateFormat('%Y-%m-%d', this.xAxis[0].min);
                          var xmax = Highcharts.dateFormat('%Y-%m-%d', this.xAxis[0].max);
                          Shiny.onInputChange('hc_Selection_port', [xmin, xmax]);}"))


output$compareTkrsChart_hc <- renderHighchart({

  shiny::validate(need(length(RVs$rets_assts_allperiods) > 0, "NO DATA"))
  shiny::validate(need(length(RVs$rets_bm_allperiods) > 0, "NO DATA"))
  shiny::validate(need(length(RVs$CompareChartTkrsSlctd) > 0, "NO Tickers Selected"))
  #shiny::validate(need(!anyNA(RVs$port_Charts_Lims), "Enter Valid Dates"))
  runjs(HTML('$("#compareTkrsChart_hc").css("background-color", "#F0F0F0;")'))
  runjs(HTML('$("#compareTkrsChart_hc").addClass("recalculating")'))

  z <- merge.xts(
    RVs$rets_assts_allperiods$Closings[[as.numeric(RVs$slct_retfreq)]],
    RVs$rets_bm_allperiods$Closings[[as.numeric(RVs$slct_retfreq)]]
  )
  names(z) <- c(
    names(RVs$rets_assts_allperiods$Closings[[as.numeric(RVs$slct_retfreq)]]), 
    names(RVs$rets_bm_allperiods$Closings[[as.numeric(RVs$slct_retfreq)]]
    )
  )
  
  a <- melt(
    data.frame(
      Date = index(z), 
      coredata(z[, RVs$CompareChartTkrsSlctd]), 
      stringsAsFactors = F
      ),
    id.vars = "Date")%>%
    mutate(variable = as.character(variable))
  names(a) <- c("Date", "tkr","Close")
  
  
  
  b <- z[, RVs$bm_name_port]

  lab <- switch(
    isolate(RVs$slct_retfreq),
    "1" = "Daily",
    "2" = "Weekly",
    "3" = "Monthly"
  )
  
  highchart(type = "stock") %>%
    hc_xAxis(labels = list(style = list(fontSize = "8px"))) %>%
    hc_add_series(b[1], name = "(show/hide all)", 
                  color = "transparent", 
                  marker = (enabled = F), 
                  #type = "scatter",
                  #legendIndex = 0,
                  lineWidth = 0,
                  events = list(
                    legendItemClick = JS(HTML(
                      "
                      function () {
                      var chart = $('#compareTkrsChart_hc').highcharts()
                      var series = chart.series[1];
                      if (series.visible) {
                      $(chart.series).each(function(){
                      this.setVisible(false, false);
                      });
                      chart.redraw();
                      } else {
                      $(chart.series).each(function(){
                      this.setVisible(true, false);
                      });
                      chart.redraw();
                      }
                      }
                      ")))) %>%
    hc_add_series(
      b,
      lineWidth = 1,
      dashStyle = 'longdash',
      tooltip = list(valueDecimals = 2), 
      name = RVs$bm_name_port,
      color = "black"
    ) %>%
    hc_add_series(a,
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      tooltip = list(valueDecimals = 2),
      lineWidth = 1
    ) %>%
    hc_plotOptions(series = list(compare = 'percent')) %>%
    hc_tooltip(
      pointFormat = HTML(
        '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.change}%)<br/>'
      )
    ) %>%
    hc_yAxis(labels = list(
      formatter = JS("function () {
                     return (this.value > 0 ? ' + ' : '') + this.value + '%';
                      }"),
      labels = list(style = list(fontSize = "8px"))
      ),
      opposite = F
      ) %>%
    hc_legend(
      title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide)</span>'),
      style = list(fontStyle = 'italic'),
      enabled = T,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom",
      symbolPadding = 1,
      symbolWidth = 10,
      itemStyle = list(fontSize = "9px")
    )  %>%
    hc_rangeSelector(buttons = list(
      list(type = 'month', count = 1, text = '1M'),
      list(type = 'month', count = 3, text = '3M'),
      list(type = 'month', count = 6, text = '6M'),
      list(type = 'year', count = 1, text = '1Y'),
      list(type = 'year', count = 3, text = '3Y'),
      list(type = 'year', count = 5, text = '5Y'),
      list(type = 'all', text = 'All')
    ),
    selected = 4
    ) %>%
    hc_add_theme(thm) %>%
    hc_title(text = paste(lab,"Cumulative Returns")) %>%
    hc_chart(
      events = list(
        redraw = selection_port
      )
    )

})



output$comapreTkrs_pa <- renderPlot({
  shiny::validate(need(nrow(RVs$Port_RaRb_lst) > 0, "NO DATA"))
  shiny::validate(need(length(RVs$CompareChartTkrsSlctd) > 0, "NO Tickers Selected"))
  shiny::validate(need(!anyNA(RVs$port_Charts_Lims), "Enter Valid Dates"))
  shiny::validate(need(RVs$port_Charts_Lims[1]<RVs$port_Charts_Lims[2], "From date MUST be before TO date and sufficient to produce MORE thane one period according to selected periodicity"))
  req(!anyNA(RVs$port_Charts_Lims), cancelOutput = T)
  runjs(HTML('$("#div_comapreTkrs_pa").css("background-color", "#F0F0F0;")'))
  runjs(HTML("$('.input-daterange > .form-control').prop('readonly', true)"))
  runjs(HTML("$('.input-daterange > .form-control').css('background-color', 'white')"))
  showElement("comapreTkrs_pa_daterange")
  req(length(RVs$Port_RaRb_lst)>0, cancelOutput = T)
  daterng <- paste(RVs$port_Charts_Lims[1],RVs$port_Charts_Lims[2] ,sep = "/")
  
  a <- RVs$Port_RaRb_lst[, c(RVs$bm_name_port,RVs$CompareChartTkrsSlctd)]
  
  lab <- switch(
    isolate(RVs$slct_retfreq),
    "1" = "Daily",
    "2" = "Weekly",
    "3" = "Monthly"
  )
  par(bg = "#F0F0F0"
      )
  chart.CumReturns(a[daterng]
                    , colorset= c("black", chartColors)
                    , lwd=2
                    , legend.loc = NULL
                    , ylab = "" #paste(lab,"Cumulative Return")
                    , main = paste(lab,"Cumulative Returns")
                    , lty = c(2, rep(1, length(RVs$CompareChartTkrsSlctd)))
  )
  
  
  
})

output$compareTkrs_pa_lgnd <- renderPlot({
  #req(length(RVs$Port_RaRb_lst) > 1, cancelOutput = T)
  #req(length(RVs$CompareChartTkrsSlctd)>0, cancelOutput = T)
  shiny::validate(need(length(RVs$CompareChartTkrsSlctd) > 0, ""))
  par(mar= c(0,0,0,0),bg = NA)
  plot.new()
  legend("bottom", 
         legend = c(RVs$bm_name_port, RVs$CompareChartTkrsSlctd) ,
         cex=.8,pt.cex = .8,
         col = c("black", chartColors),
         horiz = F,
         bty = 'n',
         lty=c(2, rep(1, length(RVs$CompareChartTkrsSlctd))),
         lwd = 1,
         ncol =ceiling(length(c(RVs$bm_name_port, RVs$CompareChartTkrsSlctd))/2)
  )
})



output$PortCorrChart <- renderHighchart({
  shiny::validate(need(nrow(RVs$Port_RaRb_lst) > 0, "NO DATA"))
  shiny::validate(need(length(RVs$CompareChartTkrsSlctd) > 0, "NO DATA"))
  shiny::validate(need(!anyNA(RVs$port_Charts_Lims), "Enter Valid Dates"))
  shiny::validate(
    need(
      RVs$port_Charts_Lims[1] < RVs$port_Charts_Lims[2],
      "From date MUST be before TO date and sufficient to produce MORE thane one period according to selected periodicity"
    )
  )
  lims <- RVs$port_Charts_Lims
  limsa <- paste(lims, collapse = "/")
  bmname <- JS(HTML(RVs$bm_name_port))
  slctedtkrs_port <- RVs$CompareChartTkrsSlctd
  subsetTkrs <- c(slctedtkrs_port, bmname)
  cormat <-
    cor(na.fill(RVs$Port_RaRb_lst[, subsetTkrs][limsa], fill = 0))
  
  cormat.order <- rev(corrplot::corrMatOrder(cormat,
                                             order = "hclust",
                                             hclust.method = "centroid"))
  
  hchart(cormat[cormat.order, cormat.order]) %>%
    hc_title(text = "Correlation Matrix") %>%
    hc_subtitle(text = paste(
      format(as.Date(lims[1]), "%b %d, %Y"),
      format(as.Date(lims[2]), "%b %d, %Y"),
      sep = " to "
    )) %>%
    hc_xAxis(labels = list(formatter = JS(HTML(
      paste(
        "
        function () {
        if (\"",
        bmname,
        "\"=== this.value) {
        return '<span style=\"fill: #3ec224;\">' + this.value + '</span>';
        } else {
        return this.value;
        }
}
  ",
  sep = ""
        )
      ))))  %>%
    hc_yAxis(labels = list(formatter = JS(HTML(
      paste(
        "
        function () {
        if (\"",
        bmname,
        "\"=== this.value) {
        return '<span style=\"fill: #3ec224;\">' + this.value + '</span>';
        } else {
        return this.value;
        }
        }
        ",
        sep = ""
        )
      )))) %>%
    hc_add_theme(thm)
  })


output$PortBoxChart <- renderHighchart({
  shiny::validate(need(nrow(RVs$Port_RaRb_lst)>0, "NO DATA"))
  shiny::validate(need(length(RVs$CompareChartTkrsSlctd)>0, "NO DATA"))
  shiny::validate(need(!anyNA(RVs$port_Charts_Lims), "Enter Valid Dates"))
  shiny::validate(need(RVs$port_Charts_Lims[1]<RVs$port_Charts_Lims[2], "From date MUST be before TO date and sufficient to produce MORE thane one period according to selected periodicity"))
  req(RVs$fs_ratio_slctd_tkr)
  lims <- RVs$port_Charts_Lims
  limsa <- paste(lims, collapse = "/")
  bmname <- JS(HTML(RVs$bm_name_port))
  slctedtkrs_port <- RVs$CompareChartTkrsSlctd
  tkrRets <-
    melt(as.data.frame(RVs$Port_RaRb_lst[,c(slctedtkrs_port, bmname)][limsa]), id.vars = NULL) %>%
    mutate(variable = as.character(variable))
  box <- hcboxplot.mod(
    x = round(tkrRets$value, 4),
    var = tkrRets$variable,
    #rep(RVs$fs_ratio_slctd_tkr, length(tkrRets)),
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
      pointFormat = "
      Maximum: {point.high}<br/>
      Upper Quartile: {point.q3}<br/>
      Median: {point.median}<br/>
      Lower Quartile: {point.q1}<br/>
      Minimum: {point.low}<br/>"
    ) %>%
    hc_yAxis(
      labels = list(format = '{value:,.2f}'),
      gridLineDashStyle = "Dash",
      gridLineColor = "rgb(211, 211, 211)"
    ) %>%
    hc_xAxis(
      gridLineDashStyle = "Dash",
      gridLineColor = "rgb(211, 211, 211)",
      labels = list(formatter = JS(HTML(
        paste(
          "function () {
          if (\"",
          bmname,
          "\"=== this.value) {
          return '<span style=\"fill: #3ec224;\">' + this.value + '</span>';
} else {
          return this.value;
}
  }
  ", sep = "")
        )))
      
        ) %>%
    hc_title(text = "Returns Boxplot")%>%
    hc_subtitle(
      text = paste(
        format(as.Date(lims[1]), "%b %d, %Y"),
        format(as.Date(lims[2]), "%b %d, %Y"),
        sep = " to "
      )
    )
  try({
    box$x$hc_opts$series[[2]]$tooltip$pointFormat <-
      "<b>{point.name}'s Outliers:</b> {point.y:.4f}<br/>"
    box$x$hc_opts$series[[2]]$tooltip$headerFormat <- ""
    box$x$hc_opts$series[[2]]$color <- "red"
  }, silent = T)
  
  datasublist <- box$x$hc_opts$series[[1]]$data
  
  for (i in 1:length(datasublist)) {
    bm <- datasublist[[i]]$name
    if (bm == bmname) {
      break()
      ;}
  }
  
  
  sublistbm <- datasublist[[i]]
  
  datasublist[[i]] <- NULL
  
  datasublistx <- append(list(sublistbm), datasublist)
  
  
  box$x$hc_opts$series[[1]]$data <- datasublistx
  
  return(box)
  
  
  
  })



output$port_metrics_tbl <- renderDataTable({
  shiny::validate(need(length(RVs$CompareChartTkrsSlctd) > 0, "NO Tickers Selected"))
  req(length(input$comapreTkrs_pa_daterange) > 0 || length(input$hc_Selection_port) > 0)
  shiny::validate(need(!anyNA(RVs$port_Charts_Lims), "Enter Valid Dates"))
  shiny::validate(need(RVs$port_Charts_Lims[1]<RVs$port_Charts_Lims[2], "From date MUST be before TO date and sufficient to produce MORE thane one period according to selected periodicity"))
  req(!anyNA(RVs$port_Charts_Lims), cancelOutput = T)
  req(nrow(RVs$Port_RaRb_lst)>0, cancelOutput = T)
  req(is.character(RVs$bm_name_port), cancelOutput = T)
  req(length(RVs$port_Charts_Lims)>0, cancelOutput = T)
  lims <- paste(RVs$port_Charts_Lims, collapse = "/")
  a <- na.fill(RVs$Port_RaRb_lst[, c(RVs$CompareChartTkrsSlctd)][lims], 0)
  b <- na.fill(RVs$Port_RaRb_lst[, c( RVs$bm_name_port)][lims],0)
  df <- fnCreateMetricsTbl(Ra = a, Rb = b)

  req(nrow(df)>0, cancelOutput = T)

  x <- datatable(df,
                 rownames=FALSE , 
                 class = "compact stripe",
                 options = list(dom = 't',paging = FALSE, scrollY = "400px", scrollX=F), height = "auto"
                 , callback = JS(HTML('$("#port_metrics_tbl > div > div > div.dataTables_scrollBody").css("height","auto")')),
                 caption = tags$caption(
                   style = "font-size:9px;padding-bottom: 0px; text-align:left;color: #bcbcbc;",
                   "Beta, Treynor:", RVs$bm_name_port , "|",
                   "Sharpe method = StdDev", "|",
                   "VaR, ES method = modified (p=95%)", "|",
                   "Risk free % = 0"
                 )
                  )%>%
    formatRound(columns=c("Ticker", "Beta", "Treynor Ratio", "Sharpe Ratio", 
                           "VaR", "ES", "Mean", "StdDev"), digits=4)
    
  return(x)
})

output$port_RiskVReturns_secOne <- renderHighchart({
  shiny::validate(need(length(RVs$CompareChartTkrsSlctd) > 0, "NO Tickers Selected"))
  req(length(input$comapreTkrs_pa_daterange) > 0 || length(input$hc_Selection_port) > 0)
  shiny::validate(need(!anyNA(RVs$port_Charts_Lims), "Enter Valid Dates"))
  shiny::validate(need(RVs$port_Charts_Lims[1]<RVs$port_Charts_Lims[2], "From date MUST be before TO date and sufficient to produce MORE thane one period according to selected periodicity"))
  req(!anyNA(RVs$port_Charts_Lims), cancelOutput = T)
  req(length(RVs$Port_RaRb_lst)>0, cancelOutput = T)
  lims <- RVs$port_Charts_Lims
  limsx <- paste(lims, collapse = "/")
  a <- RVs$Port_RaRb_lst[, c(RVs$CompareChartTkrsSlctd)][limsx]
  b <- RVs$Port_RaRb_lst[, c( RVs$bm_name_port)][limsx]
  df <- fnCreateMetricsTbl(Ra = a, Rb = b)
  req(nrow(df)>0, cancelOutput = T)
  
  
  
  highchart() %>%
    hc_add_series(
      data = df,
      hcaes(x = StdDev*100, y = Mean*100, y2 = `Sharpe Ratio`, y3 = VaR*100, y4 = ES*100, name = Ticker ),
      type ="scatter",
      name = "",
      colorByPoint = T,
      dataLabels = list(enabled = T, format = "{point.name}"),
      marker = list(symbol = "triangle-down")
    ) %>%
    hc_yAxis(labels = list(style = list(fontSize = "8px"), format = '{value:.2f}%'),
             title = list(text = "Mean Returns", style = list(`font-size`="11px"))) %>%
    hc_xAxis(labels = list(style = list(fontSize = "8px"), format = '{value:.2f}%'),
             title = list(text = "StdDev", style = list(`font-size`="11px"))) %>%
    hc_title(
      text = "Risk vs Returns"
    )%>%
    hc_subtitle(
      text = paste(
        format(as.Date(lims[1]), "%b %d, %Y"),
        format(as.Date(lims[2]), "%b %d, %Y"),
        sep = " to "
      )
    )%>%
    hc_legend(enabled = F) %>%
    hc_tooltip(
      headerFormat = "<b>{point.key}<br/>",
      pointFormat= "
    StdDev Returns: {point.x:.3f}%<br/>
      Mean Returns: {point.y:.3f}%<br/>
      Sharpe Ratio (StdDev, Rf = 0): {point.y2:.3f}<br/>
      VaR (p=0.95): {point.y3:.3f}%<br/>
      ES (p=0.95): {point.y4:.3f}%<br/>"
    ) %>%
    hc_add_theme(thm)
})


  


