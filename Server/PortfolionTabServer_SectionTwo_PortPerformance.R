

##### Portfolio Performance metrics and charts (Same as Stock Details Price history performance)

#### Validate inputs

RVs$port_SectionTwo_perf_slct_ErrMsg1x <- ""
RVs$port_SectionTwo_perf_slct_ErrMsg2x <- ""
RVs$port_SectionTwo_perf_slct_ErrMsg3x <- ""

observeEvent({
  input$port_SectionTwo_perf_slct_Rf
},
if (input$port_SectionTwo_perf_slct_Rf < 0 ||
    input$port_SectionTwo_perf_slct_Rf > Max_Rf / 100 ||
    is.na(input$port_SectionTwo_perf_slct_Rf)) {
  RVs$port_SectionTwo_Rf <- NA
  runjs(HTML(
    '$("#port_SectionTwo_perf_slct_Rf").css("background-color", "tomato")'
  ))
  RVs$port_SectionTwo_perf_slct_ErrMsg1x <-
    paste(
      "<i class=\"fa fa-exclamation-triangle\"></i> For realistic result annual Risk Free rate must be a decimal between 0 and",
      Max_Rf / 100,
      "(Max Risk Free Rate)"
    )
} else {
  RVs$port_SectionTwo_perf_slct_ErrMsg1x <- ""
  runjs(HTML(
    '$("#port_SectionTwo_perf_slct_Rf").css("background-color", "")'
  ))
  RVs$port_SectionTwo_Rf <- input$port_SectionTwo_perf_slct_Rf
  
})

observeEvent({
  input$port_SectionTwo_perf_slct_ConfLvl
},
if (input$port_SectionTwo_perf_slct_ConfLvl < .8 ||
    input$port_SectionTwo_perf_slct_ConfLvl >= 1 ||
    is.na(input$port_SectionTwo_perf_slct_ConfLvl)) {
  RVs$port_SectionTwo_conflvl <- NA
  runjs(
    HTML(
      '$("#port_SectionTwo_perf_slct_ConfLvl").css("background-color", "tomato")'
    )
  )
  RVs$port_SectionTwo_perf_slct_ErrMsg2x <-
    "<i class=\"fa fa-exclamation-triangle\"></i> For realistic result confidence level must be a decimal >= 0.8 and <1"
} else {
  RVs$port_SectionTwo_perf_slct_ErrMsg2x <- ""
  runjs(HTML(
    '$("#port_SectionTwo_perf_slct_ConfLvl").css("background-color", "")'
  ))
  RVs$port_SectionTwo_conflvl <-
    input$port_SectionTwo_perf_slct_ConfLvl
  
})

output$port_SectionTwo_perf_slct_ErrMsg1 <- renderUI({
  tags$span(HTML(RVs$port_SectionTwo_perf_slct_ErrMsg1x),
            style = "color:red;")
})


output$port_SectionTwo_perf_slct_ErrMsg2 <- renderUI({
  tags$span(HTML(RVs$port_SectionTwo_perf_slct_ErrMsg2x),
            style = "color:red;")
})

output$port_SectionTwo_perf_slct_ErrMsg3 <- renderUI({
  tags$span(HTML(RVs$port_SectionTwo_perf_slct_ErrMsg3x),
            style = "color:red;")
})


## Correct date range

observeEvent({
  input$port_SectionTwo_perf_slct_dateRng
  input$port_SectionTwo_perf_slct_freq_inpt
}
,
{
  if (anyNA(input$port_SectionTwo_perf_slct_dateRng) ||
      !is.Date(try(as.Date(input$port_SectionTwo_perf_slct_dateRng), silent = T)
      )
      ||
      input$port_SectionTwo_perf_slct_dateRng[1]  >  input$port_SectionTwo_perf_slct_dateRng[2]) {
    RVs$port_SectionTwo_perf_dateRng <- NA
    runjs(
      HTML(
        "$('#port_SectionTwo_perf_slct_dateRng > div > input').css('background-color', 'rgb(250, 139, 119)')"
      )
    )
    RVs$port_SectionTwo_perf_slct_ErrMsg3x <-
      "<i class=\"fa fa-exclamation-triangle\"></i> From date must be before To date with sufficient difference to produce more than 1 period according to selected Return Frequency"
  } else {
    RVs$port_SectionTwo_perf_slct_ErrMsg3x <- ""
    runjs(
      HTML(
        "$('#port_SectionTwo_perf_slct_dateRng > div > input').css('background-color', 'white')"
      )
    )
    AdjDateRange <-
      switch(
        input$port_SectionTwo_perf_slct_freq_inpt,
        "1" = input$port_SectionTwo_perf_slct_dateRng,
        "2" = c(
          as.Date(ceiling_date(
            ymd(input$port_SectionTwo_perf_slct_dateRng[1]), unit = "week"
          ) - days(1)),
          as.Date(ceiling_date(
            ymd(input$port_SectionTwo_perf_slct_dateRng[2]), unit = "week"
          ) - days(1))
        ),
        "3" = c(
          as.Date(ceiling_date(
            ymd(input$port_SectionTwo_perf_slct_dateRng[1]), unit = "month"
          ) - days(1)),
          as.Date(ceiling_date(
            ymd(input$port_SectionTwo_perf_slct_dateRng[2]), unit = "month"
          ) - days(1))
        )
      )
    RVs$port_SectionTwo_perf_dateRng <-
      paste(format(AdjDateRange[1]), format(AdjDateRange[2]) , sep = "/")
  }
  
  
})

observeEvent({
  RVs$port_SectionTwo_perf_dateRng
  RVs$port_SectionTwo_Rf
  RVs$port_SectionTwo_conflvl
},
{
  if (anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  )) {
    hideElement("port_SectionTwo_pefromanceResultsContainer")
  } else {
    showElement("port_SectionTwo_pefromanceResultsContainer")
  }
})

### Prep selected portfolio and benchmark returns

observeEvent({
  RVs$port_SectionTwo_perf_dateRng
  input$port_SectionTwo_perf_slct_freq_inpt
  input$port_SectionTwo_perf_slct_bnchmk
  RVs$port_sectionTwo_slctdport
},
{
  req(length(RVs$rets_assts_allperiods) > 0)
  req(length(RVs$rets_bm_allperiods) > 0)
  
  b <- switch(
    input$port_SectionTwo_perf_slct_bnchmk,
    "^GSPC" = "S&P500",
    "^IXIC" = "NASDAQ",
    "^DJI" = "DJI",
    "SPY" = "SPY"
  )
  
  port <- RVs$port_sectionTwo_slctdport
  
  if (is.null(port$OptimRes)) {
    RVs$port_SectionTwo_perf_PortBenchmarkReturns <- NULL
  }  else {
    portAssts <-
      head(names(port$OptimRes$`Efficient Frontier Points`),-5)
    
    asstsRets <-
      RVs$rets_assts_allperiods$Rets[[as.numeric(input$port_SectionTwo_perf_slct_freq_inpt)]][, portAssts]
    
    asstsRets <- na.fill(asstsRets, 0)
    
    OptimRets <- tryCatch(
      Return.portfolio(
        R = asstsRets,
        weights = head(
          as.numeric(port$OptimRes$`Optimal Portfolio`$`Solver Result`),
          -5
        ),
        geometric = F
      ),
      error = function(e)
        NA
    )
    MinRiskRets <- tryCatch(
      Return.portfolio(
        R = asstsRets,
        weights = head(
          as.numeric(port$OptimRes$`Min. Risk Portfolio`$`Solver Result`),
          -5
        ),
        geometric = F
      ),
      error = function(e)
        NA
    )
    
    equiWtRets <- tryCatch(
      Return.portfolio(
        R = asstsRets,
        weights = head(
          as.numeric(port$OptimRes$`Equal Weights Portfolio`$`Solver Result`),
          -5
        ),
        geometric = F
      ),
      error = function(e)
        NA
    )
    
    if (anyNA(list(OptimRets, MinRiskRets, equiWtRets), recursive = F)) {
      RVs$port_SectionTwo_perf_PortBenchmarkReturns <- NULL
    } else {
      PortRets <- merge.xts(OptimRets, MinRiskRets, equiWtRets)
      
      a <- merge.xts(PortRets,
                     RVs$rets_bm_allperiods$Rets[[as.numeric(input$port_SectionTwo_perf_slct_freq_inpt)]][, b])
      names(a) <-
        c(
          "Optimal Port.",
          "Min. Risk Port.",
          "Equal Wts. Port.",
          names(RVs$rets_bm_allperiods$Rets[[as.numeric(input$port_SectionTwo_perf_slct_freq_inpt)]][, b])
        )
      
      a <- na.omit(a)
      RVs$port_SectionTwo_perf_PortBenchmarkReturns <- a
    }
    
  }
  
})






### Portfolio Performance summary chart, title and legend


output$port_sectionTwo_PerfSumttl <- renderText({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  paste(RVs$port_sectionTwo_slctdport$PortName,
        " Performance ",
        sep = "")
})

output$port_sectionTwo_PerfSumSubttl <- renderText({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  Rets <-
    RVs$port_SectionTwo_perf_PortBenchmarkReturns[RVs$port_SectionTwo_perf_dateRng]
  paste(format(as.Date(min(index(
    Rets
  ))), "%b %d, %Y"),
  format(as.Date(max(index(
    Rets
  ))), "%b %d, %Y"),
  sep = " to ")
})


output$port_SectionTwo_PerformanceChart <- renderPlot({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(RVs$port_sectionTwo_slctdport)
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  shiny::validate(need(
    !anyNA(input$port_SectionTwo_perf_slct_dateRng),
    "Enter Valid Dates"
  ))
  shiny::validate(
    need(
      input$port_SectionTwo_perf_slct_dateRng[1] < input$port_SectionTwo_perf_slct_dateRng[2],
      "From date MUST be before TO date and sufficient to produce MORE thane one period according to selected periodicity"
    )
  )
  
  Rets <-
    RVs$port_SectionTwo_perf_PortBenchmarkReturns[, c(as.numeric(input$port_sectionTwo_PerfSum_slct), 4)][RVs$port_SectionTwo_perf_dateRng]
  
  plot.new()
  par(
    bg = NA,
    mgp = c(2, .5, 0),
    mar = c(0, 0, 0, 0),
    mai = c(0, 0, 0, 0)
  )
  charts.PerformanceSummary(
    Rets
    ,
    colorset = chartColors
    ,
    lwd = 2
    ,
    ylog = TRUE
    ,
    cex.lab = 1.5
    ,
    cex.axis = 1.2
    ,
    legend.loc = NULL
    ,
    main = NA
  )
})

output$port_SectionTwo_PerformanceChartLegend <- renderPlot({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(RVs$port_sectionTwo_slctdport)
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  a <-
    names(RVs$port_SectionTwo_perf_PortBenchmarkReturns[, c(as.numeric(input$port_sectionTwo_PerfSum_slct), 4)])
  par(mar = c(0, 0, 0, 0), bg = NA)
  plot.new()
  legend(
    "bottom",
    legend = c(a),
    cex = .9,
    pt.cex = 1,
    col = chartColors,
    horiz = T,
    bty = 'n',
    lty = c(rep(1, length(a))),
    lwd = 2
  )
})


### Single Portfolio box plot ####

output$port_sectionTwo_BoxCharttl <- renderText({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  paste(RVs$port_sectionTwo_slctdport$PortName,
        " Returns Boxplot",
        sep = "")
})

output$port_sectionTwo_BoxChartSubttl <- renderText({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  Rets <-
    RVs$port_SectionTwo_perf_PortBenchmarkReturns[RVs$port_SectionTwo_perf_dateRng]
  paste(format(as.Date(min(index(
    Rets
  ))), "%b %d, %Y"),
  format(as.Date(max(index(
    Rets
  ))), "%b %d, %Y"),
  sep = " to ")
})


output$port_SectionTwo_ReturnBoxplot <-
  renderHighchart({
    req(!anyNA(
      c(
        RVs$port_SectionTwo_perf_dateRng,
        RVs$port_SectionTwo_Rf,
        RVs$port_SectionTwo_conflvl
      )
    ))
    shiny::validate(need(
      nrow(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0,
      "NO DATA"
    ))
    shiny::validate(need(
      !anyNA(input$port_SectionTwo_perf_slct_dateRng),
      "Enter Valid Dates"
    ))
    shiny::validate(
      need(
        input$port_SectionTwo_perf_slct_dateRng[1] < input$port_SectionTwo_perf_slct_dateRng[2],
        "From date MUST be before TO date and sufficient to produce MORE thane one period according to selected periodicity"
      )
    )
    req(RVs$port_sectionTwo_slctdport)
    portName <- RVs$port_sectionTwo_slctdport$PortName
    bmname <-
      JS(HTML(tail(
        names(RVs$port_SectionTwo_perf_PortBenchmarkReturns), 1
      )))
    Rets <-
      RVs$port_SectionTwo_perf_PortBenchmarkReturns[, c(as.numeric(input$port_sectionTwo_BoxChart_slct), 4)][RVs$port_SectionTwo_perf_dateRng]
    PortRets <- melt(as.data.frame(Rets), id.vars = NULL) %>%
      mutate(variable = as.character(variable))
    box <- hcboxplot.mod(
      x = round(PortRets$value, 4),
      var = PortRets$variable,
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
      hc_chart(borderRadius = 0,
               backgroundColor = "none") %>%
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
            ",
            sep = ""
          )
        )))
        
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


## Single Portfolio returns distributions
output$port_sectionTwo_RetDistttl <- renderText({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  paste(RVs$port_sectionTwo_slctdport$PortName,
        " Returns Distribution",
        sep = "")
})

output$port_sectionTwo_RetDistSubttl <- renderText({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  Rets <-
    RVs$port_SectionTwo_perf_PortBenchmarkReturns[RVs$port_SectionTwo_perf_dateRng]
  paste(format(as.Date(min(index(
    Rets
  ))), "%b %d, %Y"),
  format(as.Date(max(index(
    Rets
  ))), "%b %d, %Y"),
  sep = " to ")
})



output$port_SectionTwo_ReturnDistributionChart <-
  renderHighchart({
    req(!anyNA(
      c(
        RVs$port_SectionTwo_perf_dateRng,
        RVs$port_SectionTwo_Rf,
        RVs$port_SectionTwo_conflvl
      )
    ))
    shiny::validate(need(
      nrow(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0,
      "NO DATA"
    ))
    shiny::validate(need(
      !anyNA(input$port_SectionTwo_perf_slct_dateRng),
      "Enter Valid Dates"
    ))
    shiny::validate(
      need(
        input$port_SectionTwo_perf_slct_dateRng[1] < input$port_SectionTwo_perf_slct_dateRng[2],
        "From date MUST be before TO date and sufficient to produce MORE thane one period according to selected periodicity"
      )
    )
    
    Rets <-
      RVs$port_SectionTwo_perf_PortBenchmarkReturns[, c(as.numeric(input$port_sectionTwo_RetDist_slct), 4)][RVs$port_SectionTwo_perf_dateRng]
    
    xVaRs <-
      VaR(
        R = Rets,
        p = RVs$port_SectionTwo_conflvl,
        method = "modified"
      )
    
    ### getting breaks for comparability####
    
    bPort <- hist(as.vector(Rets[, 1]), plot = F)$breaks
    bIndx <- hist(as.vector(Rets[, 2]), plot = F)$breaks
    strt <- min(c(bPort, bIndx))
    end <- max(c(bPort, bIndx))
    
    breaks <-
      seq(from = strt,
          to = end,
          length.out = length(bPort))
    
    hist.data <-
      hist(as.vector(Rets[, 1]), breaks = breaks , plot = F)
    hist.data.bm <-
      hist(as.vector(Rets[, 2]),
           breaks = breaks
           , plot = F)
    
    port_returnDist_df <-
      data.frame(
        x = hist.data$breaks[-length(hist.data$breaks)],
        y = c(hist.data$counts),
        z = c(hist.data.bm$counts),
        name = paste(hist.data$breaks[1:(length(hist.data$breaks) -
                                           1)],
                     hist.data$breaks[2:length(hist.data$breaks)],
                     sep = " to "),
        stringsAsFactors = F
      )
    
    port_returnDist_df_interval <-
      (max(hist.data$breaks) - min(hist.data$breaks)) / length(hist.data$counts)
    
    highchart() %>%
      hc_add_theme(thm) %>%
      hc_chart(borderRadius = 0,
               backgroundColor = "none") %>%
      hc_add_series(
        data = port_returnDist_df,
        hcaes(x = x, y = y, name = name),
        type = "column",
        pointPadding = 0,
        groupPadding = 0,
        pointRange = port_returnDist_df_interval,
        pointPlacement = 'between',
        name = paste(names(Rets)[1], " Returns")
      ) %>%
      hc_plotOptions(column = list(borderWidth = 1)) %>%
      hc_add_series(
        data = port_returnDist_df,
        hcaes(x = x, y = z, name = name),
        marker = list(enabled = F),
        type = "spline",
        color = "rgb(144, 237, 125)",
        name = paste(names(Rets)[2], " Returns"),
        tooltip = list(valueDecimals = 0)
      ) %>%
      hc_xAxis(
        labels = list(format = '{value:,.2f}'),
        tickInterval = port_returnDist_df_interval,
        gridLineDashStyle = "Dash",
        gridLineColor = "rgb(211, 211, 211)",
        plotLines = list(
          list(
            label = list(
              text = paste(names(Rets)[1],
                           " Mod. VaR ",
                           RVs$conflvl * 100,
                           "%", sep = ""),
              style = list(`font-size` = "10px")
            ),
            color = "red",
            width = 1,
            value = as.numeric(xVaRs[1, 1]),
            zIndex = 5,
            dashStyle = "ShortDot"
          ),
          list(
            label = list(
              text = paste(names(Rets)[2],
                           " Mod. VaR ",
                           RVs$conflvl * 100,
                           "%", sep = ""),
              style = list(`font-size` = "10px")
            ),
            color = "black",
            width = 1,
            value = as.numeric(xVaRs[1, 2]),
            zIndex = 5,
            dashStyle = "ShortDot"
          )
        )
        
        
      )  %>%
      hc_yAxis(gridLineDashStyle = "Dash",
               gridLineColor = "rgb(211, 211, 211)")  %>%
      hc_tooltip(shared = T)
    
    
  })


#### Metrics table measures ###
observeEvent(input$port_SectionTwo_perf_slct_freq_inpt
             ,
             {
               a <- switch(
                 input$port_SectionTwo_perf_slct_freq_inpt,
                 "1" = 252,
                 "2" = 52,
                 "3" = 12
               )
               RVs$port_SectionTwo_Rf_prd_factor <- a
             })


observeEvent({
  RVs$port_SectionTwo_perf_PortBenchmarkReturns
  RVs$port_SectionTwo_perf_dateRng
  RVs$port_SectionTwo_Rf
  RVs$port_SectionTwo_conflvl
},
{
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  
  Rets <-
    RVs$port_SectionTwo_perf_PortBenchmarkReturns[RVs$port_SectionTwo_perf_dateRng]
  index(Rets) <- as.POSIXct(index(Rets))
  PortPriceHistoryCAPMtbl <-
    table.CAPM(
      Ra = Rets[, 1:3],
      Rb = Rets[, 4],
      Rf = RVs$port_SectionTwo_Rf / RVs$port_SectionTwo_Rf_prd_factor,
      digits = 4
    )
  
  
  PortPriceHistoryCAPMtbl$bm <- NA
  names(PortPriceHistoryCAPMtbl) <- names(Rets)
  
  Port_calculatedavgeDD <-
    tryCatch(
      round(AverageDrawdown(R = Rets)
            , 3),
      error = function(e)
        NA
    )
  
  Port_calculatedDDlength <-
    tryCatch(
      round(AverageLength(R = Rets)
            , 3),
      error = function(e)
        NA
    )
  
  Port_calculatedDDrecov <-
    tryCatch(
      round(AverageRecovery(R = Rets)
            , 3),
      error = function(e)
        NA
    )
  
  #### mean and sd with and without outliers ###
  
  z <- mapply(function(x) {
    a <- c(coredata(Rets[, x]))
    a.mean <- mean(a)
    a.sd <- sd(a)
    a.outliers <- outliersPositions(a)
    if (length(a.outliers) != 0) {
      a.mean.clean <- mean(a[-a.outliers])
      a.sd.clean <- sd(a[-a.outliers])
    } else {
      a.mean.clean <- a.mean
      a.sd.clean <- a.sd
    }
    res <- data.frame(
      row.names = c(
        "Returns Mean Including Outliers",
        "Returns StDev. Including Outliers",
        "Returns Mean Excluding Outliers",
        "Returns StDev. Excluding Outliers"
      ),
      value = c(a.mean,
                a.sd,
                a.mean.clean,
                a.sd.clean),
      stringsAsFactors = F
    )
    names(res) <- c(names(Rets[, x]))
    return(res)
  },
  x = 1:ncol(Rets),
  SIMPLIFY = F)
  
  z <- do.call(cbind.data.frame, z)
  sharpetitle <-
    matrix(
      nrow = 1,
      ncol = 4,
      dimnames = list("Sharpe Ratio:", names(Rets))
    )
  sharpetbl <- round(
    SharpeRatio(
      R = Rets,
      Rf = RVs$port_SectionTwo_Rf / RVs$port_SectionTwo_Rf_prd_factor,
      p = RVs$port_SectionTwo_conflvl
    )
    ,
    3
  )
  
  
  VaRsTitle <-
    matrix(
      nrow = 1,
      ncol = 4,
      dimnames = list("Value At Risk (VaR):", names(Rets))
    )
  
  VaRs <-
    mapply(
      function (x, y) {
        a <- VaR(
          R = Rets,
          p = RVs$port_SectionTwo_conflvl,
          method = x
        )
        row.names(a) <-
          paste(y,
                " (",
                "p=",
                RVs$port_SectionTwo_conflvl * 100,
                "%",
                ")",
                sep = "")
        return(a)
        
      },
      x = c("modified", "gaussian", "historical"),
      y = c("Modified Method", "Gaussian Method", "Historical Method"),
      SIMPLIFY = F,
      USE.NAMES = F
    )
  VaRs <- do.call(rbind.data.frame, VaRs)
  
  CVaRsTitle <-
    matrix(
      nrow = 1,
      ncol = 4,
      dimnames = list("Estimated Shortfall (ES):", names(Rets))
    )
  
  CVaRs <-
    mapply(
      function (x, y) {
        a <- CVaR(
          R = Rets,
          p = RVs$port_SectionTwo_conflvl,
          method = x
        )
        row.names(a) <-
          paste(y,
                " (",
                "p=",
                RVs$port_SectionTwo_conflvl * 100,
                "%",
                ")",
                sep = "")
        return(a)
        
      },
      x = c("modified", "gaussian", "historical"),
      y = c(
        "ES Modified Method",
        "ES Gaussian Method",
        "ES Historical Method"
      ),
      SIMPLIFY = F,
      USE.NAMES = F
    )
  CVaRs <- do.call(rbind.data.frame, CVaRs)
  
  PerfMetricstabl <- list(
    Port_calculatedavgeDD,
    Port_calculatedDDlength,
    Port_calculatedDDrecov,
    PortPriceHistoryCAPMtbl,
    z,
    sharpetitle,
    sharpetbl,
    VaRsTitle,
    VaRs,
    CVaRsTitle,
    CVaRs
  )
  
  PerfMetricstabl <-
    do.call(rbind.data.frame, PerfMetricstabl)
  
  PerfMetricstabl$Name <- row.names(PerfMetricstabl)
  
  row.names(PerfMetricstabl) <- NULL
  
  PerfMetricstabl <- PerfMetricstabl[, c(5, 4, 1:3)]
  
  names(PerfMetricstabl)[3:5] <-
    c("Optimal Port.", "Min. Risk", "Equal Wts.")
  
  RVs$PerfMetricstabl <- filter(
    PerfMetricstabl,
    !Name %in% c(
      "Tracking Error",
      "Correlation p-value",
      "Beta+",
      "Beta-",
      "Annualized Alpha"
    )
  )
})

output$port_SectionTwo_perfmetricstbl <- renderTable({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  shiny::validate(need(nrow(RVs$PerfMetricstabl) > 0, "NO DATA"),
                  need(
                    length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0,
                    "NO DATA"
                  ))
  
  a <- switch(
    input$port_SectionTwo_perf_slct_freq_inpt,
    "1" = "(in days)",
    "2" = "(in weeks)",
    "3" = "(in months)"
  )
  
  df <- RVs$PerfMetricstabl
  df[1, "Name"] <- "Avg. Drawdown"
  df[2, "Name"] <- paste("Drawdown Duaration", a, sep = " ")
  df[3, "Name"] <- paste("Drawdown recovery", a, sep = " ")
  names(df)[1] <- ""
  return(df)
  
}, striped = T, width = "100%", colnames = T, digits = 3, na = "")


## Single Portfolio VAR/ES Sensitivity
output$port_sectionTwo_VaRsensttl <- renderText({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  paste(RVs$port_sectionTwo_slctdport$PortName,
        " VaR/ES Sensitivity",
        sep = "")
})

output$port_sectionTwo_VaRsensSubttl <- renderText({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  req(length(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0)
  Rets <-
    RVs$port_SectionTwo_perf_PortBenchmarkReturns[RVs$port_SectionTwo_perf_dateRng]
  paste(format(as.Date(min(index(
    Rets
  ))), "%b %d, %Y"),
  format(as.Date(max(index(
    Rets
  ))), "%b %d, %Y"),
  sep = " to ")
})



output$port_SectionTwo_VaRsens <- renderHighchart({
  req(!anyNA(
    c(
      RVs$port_SectionTwo_perf_dateRng,
      RVs$port_SectionTwo_Rf,
      RVs$port_SectionTwo_conflvl
    )
  ))
  shiny::validate(need(
    nrow(RVs$port_SectionTwo_perf_PortBenchmarkReturns) > 0,
    "NO DATA"
  ))

  Pseq <- seq(0.99, 0.89, by = -0.005)
  Rets <-
    RVs$port_SectionTwo_perf_PortBenchmarkReturns[, c(as.numeric(input$port_sectionTwo_VaRsens_slct))][RVs$port_SectionTwo_perf_dateRng]
  
  VaRsens <-
    mapply(
      function (x) {
        xx <-
          data.frame(
            method = c(),
            name = c(),
            p = c(),
            VaR = c(),
            stringsAsFactors = F
          )
        for (i in Pseq) {
          a <- VaR(R = Rets,
                   p = i,
                   method = x)
          a <- data.frame(
            method = paste("VaR", x),
            name = paste("p=", format(i * 100, nsmall = 2), "%",
                         sep = ""),
            p = i,
            round(coredata(a), 4),
            stringsAsFactors = F
          )
          xx <- rbind.data.frame(xx, a)
        }
        return(xx)
      },
      x = c("modified", "gaussian", "historical"),
      SIMPLIFY = F
    )
  
  
  VaRsens.df = do.call(rbind.data.frame, VaRsens)
  
  rownames(VaRsens.df) <- NULL
  
  
  CVaRsens <-
    mapply(
      function (x) {
        xx <-
          data.frame(
            method = c(),
            name = c(),
            p = c(),
            CVaR = c(),
            stringsAsFactors = F
          )
        for (i in Pseq) {
          a <- suppressMessages(CVaR(
            R = Rets,
            p = i,
            method = x
          ))
          a <- data.frame(
            method = paste("ES", x),
            name = paste("p=", format(i * 100, nsmall = 2), "%",
                         sep = ""),
            p = i,
            round(coredata(a), 4),
            stringsAsFactors = F
          )
          xx <- rbind.data.frame(xx, a)
        }
        return(xx)
      },
      x = c("modified", "gaussian", "historical"),
      SIMPLIFY = F
    )
  
  
  CVaRsens.df = do.call(rbind.data.frame, CVaRsens)
  
  rownames(CVaRsens.df) <- NULL
  
  CVaRsens.df.percent <- CVaRsens.df
  CVaRsens.df.percent$p <- round(CVaRsens.df.percent$p * 100, 2)
  CVaRsens.df.percent[, 4] <- round(CVaRsens.df.percent[, 4] * 100, 5)
  names(CVaRsens.df.percent)[4] <- "CVaR"
  
  VaRsens.df.percent <- VaRsens.df
  VaRsens.df.percent$p <- round(VaRsens.df.percent$p * 100, 2)
  VaRsens.df.percent[, 4] <- round(VaRsens.df.percent[, 4] * 100, 5)
  names(VaRsens.df.percent)[4] <- "VaR"
  
  
  
  highchart() %>%
    hc_add_series(
      data = VaRsens.df.percent,
      hcaes(x = p, y = VaR, group = method),
      type = "line",
      marker = list(enabled = F),
      lineWidth = 1,
      dashStyle = c("ShortDash", "ShortDot", "Solid")
    ) %>%
    hc_add_series(
      data = CVaRsens.df.percent,
      hcaes(x = p, y = CVaR, group = method),
      type = "line",
      marker = list(enabled = F),
      lineWidth = 1,
      dashStyle = c("ShortDash", "ShortDot", "Solid")
    ) %>%
    hc_xAxis(
      labels = list(format = "{value:.1f}%", fontSize = "8px"),
      title = list(text = "Confidence Level", style = list(fontSize =
                                                             "9px")),
      gridLineDashStyle = "Dash",
      gridLineColor = "rgb(211, 211, 211)"
    ) %>%
    hc_yAxis(
      labels = list(format = "{value}%", fontSize = "8px"),
      title = list(text = "VaR/ES", style = list(fontSize = 9)),
      gridLineDashStyle = "Dash",
      gridLineColor = "rgb(211, 211, 211)",
      plotLines = list(
        list(
          label = list(
            text = "Unreliable Results",
            style = list(`font-size` = "10px", color = "red")
          ),
          color = "red",
          width = 1,
          value = -100,
          zIndex = 5,
          dashStyle = "Solid"
        )
      )
    ) %>%
    hc_tooltip(shared = T, valueSuffix = "%") %>%
    hc_add_theme(thm) %>%
    hc_chart(borderRadius = 0,
             backgroundColor = "none") %>%
    hc_legend(itemStyle = list(fontSize = "8px"))
})
