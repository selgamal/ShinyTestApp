### Server for portfolio tab section three

RVs$port_SectionThree_daterange_ErrMsgx <- ""

RVs$port_sectionThree_slct_ComparePortSlctx <- NA
RVs$port_sectionThree_daterangex <- NA
RVs$port_sectionThree_slct_retfreqx <- NA
RVs$port_sectionThree_slct_benchmark_portx <- NA

observeEvent(RVs$Port_sectionTwo_savedPorts,
             {
               a <- names(RVs$Port_sectionTwo_savedPorts)
               a <- a[a != "Unsaved"]
               updateSelectizeInput(
                 session = session,
                 inputId = "port_sectionThree_slct_ComparePortSlct",
                 choices = a,
                 selected = head(a, 3)
               )
               RVs$port_sectionThree_optimWts <- mapply(
                 FUN = function(p) {
                   x <- RVs$Port_sectionTwo_savedPorts
                   z <-
                     x[[p]]$OptimRes$`Optimal Portfolio`$`Solver Result`
                   z <-
                     suppressMessages(reshape2::melt(z[, 1:(ncol(z) - 5)]
                                                     , variable.name = "symbol"
                                                     , value.name = "wt"))
                   return(z)
                 },
                 p = a,
                 SIMPLIFY = F
               )
             })

observeEvent({
  input$port_sectionThree_slct_ComparePortSlct
  input$port_sectionThree_daterange
  input$port_sectionThree_slct_retfreq
  input$port_sectionThree_slct_benchmark_port
},
{
  runjs(
    HTML(
      '$("#port_sectionThree_btn_updatePortCompareChart").css("background-color", "tomato")'
    )
  )
})



observeEvent(input$port_sectionThree_btn_updatePortCompareChart,
             {
               #clean up variables
               RVs$port_sectionThree_slct_ComparePortSlctx <- NA
               RVs$port_sectionThree_daterangex <- NA
               RVs$port_sectionThree_slct_retfreqx <- NA
               RVs$port_sectionThree_slct_benchmark_portx <- NA
               
               if (is.null(input$port_sectionThree_slct_ComparePortSlct) |
                   length(input$port_sectionThree_slct_ComparePortSlct) < 2) {
                 runjs(
                   HTML(
                     "$('#port_sectionThree_compchartslctrs > div:nth-child(1) > div > div > div > div.selectize-input').css('background-color', 'rgb(250, 139, 119)')"
                   )
                 )
                 RVs$port_SectionThree_daterange_ErrMsgx <-
                   "<i class=\"fa fa-exclamation-triangle\"></i> Select 2 or more saved portfolios to compare"
               } else {
                 runjs(
                   HTML(
                     "$('#port_sectionThree_compchartslctrs > div:nth-child(1) > div > div > div > div.selectize-input').css('background-color', '')"
                   )
                 )
                 RVs$port_SectionThree_daterange_ErrMsgx <- ""
                 
                 if (anyNA(input$port_sectionThree_daterange) ||
                     !is.Date(try(as.Date(input$port_sectionThree_daterange), silent = T))
                     ||
                     input$port_sectionThree_daterange[1]  >  input$port_sectionThree_daterange[2]) {
                   runjs(
                     HTML(
                       "$('#port_sectionThree_daterange > div > input').css('background-color', 'rgb(250, 139, 119)')"
                     )
                   )
                   RVs$port_SectionThree_daterange_ErrMsgx <-
                     "<i class=\"fa fa-exclamation-triangle\"></i> From date must be before To date with sufficient difference to produce more than 1 period according to selected Return Frequency"
                 } else {
                   RVs$port_SectionThree_daterange_ErrMsgx <- ""
                   runjs(
                     HTML(
                       "$('#port_sectionThree_daterange > div > input').css('background-color', 'white')"
                     )
                   )
                   AdjDateRange <-
                     switch(
                       input$port_sectionThree_slct_retfreq,
                       "1" = input$port_sectionThree_daterange,
                       "2" = c(
                         as.Date(ceiling_date(
                           ymd(input$port_sectionThree_daterange[1]), unit = "week"
                         ) - days(1)),
                         as.Date(ceiling_date(
                           ymd(input$port_sectionThree_daterange[2]), unit = "week"
                         ) - days(1))
                       ),
                       "3" = c(
                         as.Date(ceiling_date(
                           ymd(input$port_sectionThree_daterange[1]), unit = "month"
                         ) - days(1)),
                         as.Date(ceiling_date(
                           ymd(input$port_sectionThree_daterange[2]), unit = "month"
                         ) - days(1))
                       )
                     )
                   
                   RVs$port_sectionThree_slct_ComparePortSlctx <-
                     input$port_sectionThree_slct_ComparePortSlct
                   RVs$port_sectionThree_daterangex <-
                     paste(format(AdjDateRange[1]), format(AdjDateRange[2]) , sep = "/")
                   RVs$port_sectionThree_slct_retfreqx <-
                     input$port_sectionThree_slct_retfreq
                   RVs$port_sectionThree_slct_benchmark_portx <-
                     switch(
                       input$port_sectionThree_slct_benchmark_port,
                       "^GSPC" = "S&P500",
                       "^IXIC" = "NASDAQ",
                       "^DJI" = "DJI",
                       "SPY" = "SPY"
                     )
                   runjs(
                     HTML(
                       '$("#port_sectionThree_btn_updatePortCompareChart").css("background-color", "")'
                     )
                   )
                 }
                 
               }
             })


output$port_SectionThree_daterange_ErrMsg <- renderUI({
  tags$span(HTML(RVs$port_SectionThree_daterange_ErrMsgx),
            style = "color:red;")
})


observeEvent({
  RVs$port_sectionThree_slct_ComparePortSlctx
  RVs$port_sectionThree_daterangex
  RVs$port_sectionThree_slct_retfreqx
  RVs$port_sectionThree_slct_benchmark_portx
},
{
  req(length(names(RVs$port_sectionThree_optimWts)) > 0)
  if (anyNA(
    c(
      RVs$port_sectionThree_slct_ComparePortSlctx,
      RVs$port_sectionThree_daterangex,
      RVs$port_sectionThree_slct_retfreqx,
      RVs$port_sectionThree_slct_benchmark_portx
    )
  )) {
    hideElement("port_sectionThree_Contents")
    RVs$port_sectionThree_Rets <- NA
    RVs$port_sectionThree_Rets.monthly <- NA
  } else {
    showElement("port_sectionThree_Contents")
    p <- RVs$port_sectionThree_slct_ComparePortSlctx
    d <- RVs$port_sectionThree_daterangex
    a <- RVs$port_sectionThree_optimWts
    b <- as.numeric(RVs$port_sectionThree_slct_retfreqx)
    i <- RVs$port_sectionThree_slct_benchmark_portx
    r <- mapply(
      FUN = function(x, y) {
        s <- a[[x]]$symbol
        w <- a[[x]]$wt
        Ret <- RVs$rets_assts_allperiods$Rets[[y]][, s][d]
        Ret <- na.fill(Ret, 0)
        Ret <-
          Return.portfolio(
            R = Ret,
            weights = w,
            wealth.index = F,
            geometric = F
          )
        return(Ret)
      },
      x = p,
      y = b,
      SIMPLIFY = F
    )
    
    r.merge <- do.call(merge.xts, r)
    r.merge$bm <-
      na.fill(RVs$rets_bm_allperiods$Rets[[b]][, i][d], 0)
    names(r.merge) <- c(p, i)
    
    r.monthly <- mapply(
      FUN = function(x, y) {
        s <- a[[x]]$symbol
        w <- a[[x]]$wt
        Ret <- RVs$rets_assts_allperiods$Rets[[y]][, s][d]
        Ret <- na.fill(Ret, 0)
        Ret <-
          Return.portfolio(
            R = Ret,
            weights = w,
            wealth.index = F,
            geometric = F
          )
        return(Ret)
      },
      x = p,
      y = 3,
      SIMPLIFY = F
    )
    r.monthly.merge <- do.call(merge.xts, r.monthly)
    r.monthly.merge$bm <-
      na.fill(RVs$rets_bm_allperiods$Rets[[3]][, i][d], 0)
    names(r.monthly.merge) <- c(p, i)
    
    RVs$port_sectionThree_Rets <- r.merge
    RVs$port_sectionThree_Rets.monthly <- r.monthly.merge
    
  }
})


observeEvent(
  RVs$port_sectionThree_Rets
  ,
  {
    req(nrow(RVs$port_sectionThree_Rets)>0)
    z <- RVs$port_sectionThree_Rets
    updateSelectInput(
      session = session,
      inputId = "port_sectionThree_RowTwo_colTwo_relativePerf_slctr",
      choices = as.character(names(z)),
      selected = as.character(tail(names(z),1))
    )
    
  }
)


#### Performance chart ####

output$port_sectionThree_RowOne_colOne_subtitle <- renderText({
  req(!anyNA(
    c(
      RVs$port_sectionThree_daterangex,
      RVs$port_sectionThree_slct_ComparePortSlctx
    )
  ))
  req(nrow(RVs$port_sectionThree_Rets) > 0)
  Rets <- RVs$port_sectionThree_Rets
  lab <- switch(
    RVs$port_sectionThree_slct_retfreqx,
    "1" = "Daily from ",
    "2" = "Weekly from ",
    "3" = "Monthly from "
  )
  paste(lab,
        " ",
        format(as.Date(min(index(
          Rets
        ))), "%b %d, %Y"),
        " to ",
        format(as.Date(max(index(
          Rets
        ))), "%b %d, %Y"),
        sep = "")
})


output$port_SectionThree_PerfSum <- renderHighchart({
  shiny::validate(need(nrow(RVs$port_sectionThree_Rets) > 0, "NO DATA"))
  
  z <-  RVs$port_sectionThree_Rets
  bm_name <- names(z)[ncol(z)]
  z.wealth <- mapply(function(x) {
    a <- Return.portfolio(z[, x], wealth.index = T)
    a <- data.frame(
      Date = index(a),
      tkr = x,
      close = round(coredata(a[, 1]) * 100, 3),
      stringsAsFactors = F
    )
    names(a)[3] <- "Close"
    return(a)
  },
  x = names(z),
  SIMPLIFY = F)
  
  z.wealth.df <-
    do.call(rbind.data.frame,
            c(
              z.wealth,
              make.row.names = F,
              stringsAsFactors = F
            ))
  row.names(z.wealth.df) <- NULL
  
  a <- z.wealth.df
  names(a) <- c("Date", "tkr", "Close")
  
  RVs$TestingA <- a[a$tkr != bm_name, ]
  
  b <- Return.portfolio(z[, ncol(z)], wealth.index = T)
  names(b) <- names(z)[ncol(z)]
  b[, 1] <- round(b * 100, 3)
  
  
  dds <- PerformanceAnalytics:::Drawdowns(z)
  
  dds.df <- data.frame(
    Date = index(dds),
    coredata(dds),
    stringsAsFactors = F,
    check.names = F
  ) %>%
    melt(id.vars = "Date") %>%
    mutate(variable = as.character(variable))
  names(dds.df) <- c("Date", "tkr", "Close")
  
  HC <-
    highchart(type = "stock") %>%
    hc_add_theme(thm) %>%
    hc_chart(borderRadius = 0,
             backgroundColor = "none") %>%
    hc_xAxis(labels = list(style = list(fontSize = "8px"))) %>%
    hc_yAxis_multiples(
      create_yaxis(
        2,
        heights = c(3, 1),
        sep = .05,
        turnopposite = F,
        opposite = F,
        labels = list(
          list(
            formatter = JS(
              "function () {
              return (this.value > 0 ? ' + ' : '') + Highcharts.numberFormat(this.value,0) + '%';
}"
              ),
            style = list(fontSize = "8px")
            ),
          list(
            formatter = JS(
              "function () {
              return (this.value > 0 ? ' + ' : '') + Highcharts.numberFormat(this.value,0) + '%';
}"
),
style = list(fontSize = "8px")
            )
          ),
title = list(list(text = "Cumulative Returns")
             ,
             list(text = "Drawdown"))
          )
        ) %>%
    hc_add_series(
      b[1],
      name = "(show/hide all)",
      color = "transparent",
      marker = (enabled = F),
      yAxis = 0,
      lineWidth = 0,
      events = list(legendItemClick = JS(
        HTML(
          "
          function () {
          var chart = $('#port_SectionThree_PerfSum').highcharts()
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
          "
        )
        ))
        ) %>%
    hc_add_series(
      a[a$tkr == bm_name,],
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{series.color}">{series.name}</span>: <b>${point.y}</b> ({point.change}% Cumulative)<br/>'
        )
      ),
      lineWidth = 1,
      yAxis = 0,
      compare = 'percent',
      dashStyle = 'longdash',
      color = 'black',
      id = bm_name
    ) %>%
    hc_add_series(
      a[a$tkr != bm_name,],
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{series.color}">{series.name}</span>: <b>${point.y}</b> ({point.change}% Cumulative)<br/>'
        )
      ),
      lineWidth = 1,
      yAxis = 0,
      compare = 'percent',
      color = rep(chartColors, length.out = (ncol(z) - 1)),
      id = unique(a[a$tkr != bm_name, 2])
    ) %>%
    hc_add_series(
      linkedTo = bm_name,
      dds.df[dds.df$tkr == bm_name,],
      type = "spline",
      hcaes(x = Date, y = Close * 100, group = tkr),
      lineWidth = 1,
      yAxis = 1,
      dashStyle = 'longdash',
      color = 'black',
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y}%</b> (Drawdown)<br/>'
        )
      )
    ) %>%
    hc_add_series(
      linkedTo = unique(a[a$tkr != bm_name, 2]),
      dds.df[dds.df$tkr != bm_name,],
      type = "spline",
      hcaes(x = Date, y = Close * 100, group = tkr),
      lineWidth = 1,
      yAxis = 1,
      color = rep(chartColors, length.out = (ncol(z) - 1)),
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y}%</b> (Drawdown)<br/>'
        )
      )
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
    hc_rangeSelector(enabled = F) %>%
    hc_navigator(enabled = F) %>%
    hc_scrollbar(enabled = F)
  HC
  })

#### Relative performance ####

output$port_sectionThree_RowTwo_colTwo_relativePerf_subtitle <-
  renderText({
    req(!anyNA(
      c(
        RVs$port_sectionThree_daterangex,
        RVs$port_sectionThree_slct_ComparePortSlctx
      )
    ))
    req(nrow(RVs$port_sectionThree_Rets) > 0)
    Rets <- RVs$port_sectionThree_Rets
    lab <- switch(
      RVs$port_sectionThree_slct_retfreqx,
      "1" = "Daily from ",
      "2" = "Weekly from ",
      "3" = "Monthly from "
    )
    paste(lab,
          " ",
          format(as.Date(min(index(
            Rets
          ))), "%b %d, %Y"),
          " to ",
          format(as.Date(max(index(
            Rets
          ))), "%b %d, %Y"),
          sep = "")
  })

output$port_SectionThree_relativePerf <- renderHighchart({
  shiny::validate(need(nrow(RVs$port_sectionThree_Rets) > 0, "NO DATA"))
  req(input$port_sectionThree_RowTwo_colTwo_relativePerf_slctr)
  z <-  RVs$port_sectionThree_Rets
  Rb <- input$port_sectionThree_RowTwo_colTwo_relativePerf_slctr
  Ra <- setdiff(names(z), Rb)
  
  relPerf <- Return.relative(Ra = z[, Ra], Rb = z[, Rb])
  
  dummy <- as.xts(relPerf[, 1])
  
  rel <-
    data.frame(
      Date = index(relPerf),
      coredata(relPerf),
      stringsAsFactors = F,
      check.names = F
    )
  
  names(rel) <- gsub("/", " to ", names(rel))
  
  rel.long <- melt(rel, id.vars = "Date") %>%
    mutate(variable = as.character(variable))
  names(rel.long) <- c("Date", "tkr", "Close")
  
  hc <-
    highchart(type = "stock") %>%
    hc_add_theme(thm) %>%
    hc_chart(borderRadius = 0,
             backgroundColor = "none") %>%
    hc_xAxis(labels = list(style = list(fontSize = "8px"))) %>%
    hc_yAxis(labels = list(style = list(fontSize = "8px"), format = '{value:,.2f}'),
             opposite = F) %>%
    hc_add_series(
      dummy[1],
      name = "(show/hide all)",
      color = "transparent",
      marker = (enabled = F),
      yAxis = 0,
      tooltip = list(enabled = F),
      lineWidth = 0,
      events = list(legendItemClick = JS(
        HTML(
          "
          function () {
          var chart = $('#port_SectionThree_relativePerf').highcharts()
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
          "
        )
        ))
        ) %>%
    hc_add_series(
      rel.long,
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      tooltip = list(valueDecimals = 2),
      lineWidth = 1
    )  %>%
    hc_rangeSelector(enabled = F) %>%
    hc_navigator(enabled = F) %>%
    hc_scrollbar(enabled = F) %>%
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
    )
  hc
  
  })



##### Box Plot ####

output$port_sectionThree_RowThree_colTwo_boxplot_subtitle <-
  renderText({
    req(!anyNA(
      c(
        RVs$port_sectionThree_daterangex,
        RVs$port_sectionThree_slct_ComparePortSlctx
      )
    ))
    req(nrow(RVs$port_sectionThree_Rets) > 0)
    Rets <- RVs$port_sectionThree_Rets
    lab <- switch(
      RVs$port_sectionThree_slct_retfreqx,
      "1" = "Daily from ",
      "2" = "Weekly from ",
      "3" = "Monthly from "
    )
    paste(lab,
          " ",
          format(as.Date(min(index(
            Rets
          ))), "%b %d, %Y"),
          " to ",
          format(as.Date(max(index(
            Rets
          ))), "%b %d, %Y"),
          sep = "")
  })

output$port_SectionThree_boxPlot <- renderHighchart({
  shiny::validate(need(nrow(RVs$port_sectionThree_Rets) > 0, "NO DATA"))
  z <-  RVs$port_sectionThree_Rets
  bmname <- names(z)[ncol(z)]
  tkrRets <-
    melt(as.data.frame(z), id.vars = NULL) %>%
    mutate(variable = as.character(variable))
  box <- hcboxplot.mod(
    x = round(tkrRets$value, 4),
    var = tkrRets$variable,
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
  ", sep = "")
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
      
    }
  }
  
  
  sublistbm <- datasublist[[i]]
  
  datasublist[[i]] <- NULL
  
  datasublistx <- append(list(sublistbm), datasublist)
  
  
  box$x$hc_opts$series[[1]]$data <- datasublistx
  
  return(box)
  })


##### Risk V Rets Chart ####

output$port_sectionThree_RowThree_colOne_riskVRet_subtitle <-
  renderText({
    req(!anyNA(
      c(
        RVs$port_sectionThree_daterangex,
        RVs$port_sectionThree_slct_ComparePortSlctx
      )
    ))
    req(nrow(RVs$port_sectionThree_Rets) > 0)
    Rets <- RVs$port_sectionThree_Rets
    lab <- switch(
      RVs$port_sectionThree_slct_retfreqx,
      "1" = "Daily from ",
      "2" = "Weekly from ",
      "3" = "Monthly from "
    )
    paste(lab,
          " ",
          format(as.Date(min(index(
            Rets
          ))), "%b %d, %Y"),
          " to ",
          format(as.Date(max(index(
            Rets
          ))), "%b %d, %Y"),
          sep = "")
  })


output$port_SectionThree_riskVRet <- renderHighchart({
  shiny::validate(need(nrow(RVs$port_sectionThree_Rets) > 0, "NO DATA"))
  z <-  RVs$port_sectionThree_Rets
  Rb <- names(z)[ncol(z)]
  Ra <- setdiff(names(z), Rb)
  
  a <- z #[, Ra]
  b <- z[, Rb]
  df <- fnCreateMetricsTbl(Ra = a, Rb = b)
  req(nrow(df) > 0, cancelOutput = T)
  
  
  
  highchart() %>%
    hc_add_series(
      data = df,
      hcaes(
        x = StdDev * 100,
        y = Mean * 100,
        y2 = `Sharpe Ratio`,
        y3 = VaR * 100,
        y4 = ES * 100,
        name = Ticker
      ),
      type = "scatter",
      name = "",
      colorByPoint = T,
      dataLabels = list(enabled = T, format = "{point.name}"),
      marker = list(symbol = "triangle-down")
    ) %>%
    hc_yAxis(
      labels = list(style = list(fontSize = "8px"), format = '{value:.2f}%'),
      title = list(text = "Mean Returns", style = list(`font-size` =
                                                         "11px"))
    ) %>%
    hc_xAxis(
      labels = list(style = list(fontSize = "8px"), format = '{value:.2f}%'),
      title = list(text = "StdDev", style = list(`font-size` = "11px"))
    ) %>%
    hc_legend(enabled = F) %>%
    hc_tooltip(
      headerFormat = "<b>{point.key}<br/>",
      pointFormat = "
      StdDev Returns: {point.x:.3f}%<br/>
      Mean Returns: {point.y:.3f}%<br/>
      Sharpe Ratio (StdDev, Rf = 0): {point.y2:.3f}<br/>
      VaR (p=0.95): {point.y3:.3f}%<br/>
      ES (p=0.95): {point.y4:.3f}%<br/>"
    ) %>%
    hc_add_theme(thm) %>%
    hc_chart(borderRadius = 0,
             backgroundColor = "none")
})

###### Rolling performance #####

### to prevent update when monthly rets are not affected by user interaction


output$port_sectionThree_RowOne_colTwo_rollingPerf_subtitle <-
  renderText({
    req(nrow(RVs$port_sectionThree_Rets.monthly) > 0)
    Rets <- RVs$port_sectionThree_Rets.monthly
    paste(
      format(as.Date(min(index(
        Rets
      ))), "%b %d, %Y"),
      " to ",
      format(as.Date(max(index(
        Rets
      ))), "%b %d, %Y"),
      " (Only Monthly)",
      sep = ""
    )
  })

output$port_SectionThree_rollingPerf <- renderHighchart({
  shiny::validate(need(nrow(RVs$port_sectionThree_Rets.monthly) > 0, "NO DATA"))
  z <- RVs$port_sectionThree_Rets.monthly
  bmname <- names(z)[ncol(z)]
  
  Rolling.Rets <-
    rollapply(
      data = z,
      FUN = "Return.annualized",
      scale = 12,
      width = 12
    )
  Rolling.Rets <- na.omit(Rolling.Rets)
  Rets <- data.frame(
    Date = index(Rolling.Rets),
    coredata(Rolling.Rets),
    stringsAsFactors = F,
    check.names = F
  ) %>%
    melt(id.vars = "Date") %>%
    mutate(variable = as.character(variable))
  names(Rets) <- c("Date", "tkr", "Close")
  
  dummy <- as.xts(Rolling.Rets[, 1])
  
  
  Rolling.SD <-
    rollapply(
      data = z,
      FUN = "StdDev.annualized",
      scale = 12,
      width = 12
    )
  Rolling.SD <- na.omit(Rolling.SD)
  SD <- data.frame(
    Date = index(Rolling.SD),
    coredata(Rolling.SD),
    stringsAsFactors = F,
    check.names = F
  ) %>%
    melt(id.vars = "Date") %>%
    mutate(variable = as.character(variable))
  names(SD) <- c("Date", "tkr", "Close")
  
  
  Roling.SR <-
    rollapply(
      data = z,
      FUN = "SharpeRatio.annualized",
      scale = 12,
      width = 12
    )
  Roling.SR <- na.omit(Roling.SR)
  SR <- data.frame(
    Date = index(Roling.SR),
    coredata(Roling.SR),
    stringsAsFactors = F,
    check.names = F
  ) %>%
    melt(id.vars = "Date") %>%
    mutate(variable = as.character(variable))
  names(SR) <- c("Date", "tkr", "Close")
  
  
  
  hc3 <-
    highchart(type = "stock") %>%
    hc_add_theme(thm) %>%
    hc_chart(borderRadius = 0,
             backgroundColor = "none") %>%
    hc_xAxis(labels = list(style = list(fontSize = "8px"))) %>%
    hc_yAxis_multiples(
      create_yaxis(
        3,
        heights = c(1, 1, 1),
        sep = .05,
        turnopposite = F,
        opposite = F,
        labels = list(
          list(
            formatter = JS(
              "function () {
              return (this.value > 0 ? ' + ' : '') + Highcharts.numberFormat(this.value,2);
}"
),
labels = list(style = list(fontSize = "8px"))
            ),
list(
  formatter = JS(
    "function () {
    return (this.value > 0 ? ' + ' : '') + Highcharts.numberFormat(this.value,2);
}"
),
labels = list(style = list(fontSize = "8px"))
  ),
list(
  formatter = JS(
    "function () {
    return (this.value > 0 ? ' + ' : '') + Highcharts.numberFormat(this.value,2);
}"
),
labels = list(style = list(fontSize = "8px"))
  )
),
title = list(
  list(text = "Returns")
  ,
  list(text = "StdDev."),
  list(text = "Sharpe Ratio")
)
          )
    ) %>%
    hc_add_series(
      dummy[1],
      name = "(show/hide all)",
      color = "transparent",
      marker = (enabled = F),
      yAxis = 0,
      lineWidth = 0,
      events = list(legendItemClick = JS(
        HTML(
          "
          function () {
          var chart = $('#port_SectionThree_rollingPerf').highcharts()
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
          "
        )
        ))
        ) %>%
    hc_add_series(
      Rets[Rets$tkr == bmname, ],
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{point.color}">\u25CF</span> {series.name} <span style="color:red">Returns</span>: <b>{point.y}</b><br/>'
        )
      ),
      lineWidth = 1,
      yAxis = 0,
      dashStyle = 'longdash',
      color = 'black',
      id = bmname
    ) %>%
    hc_add_series(
      Rets[Rets$tkr != bmname, ],
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{point.color}">\u25CF</span> {series.name} <span style="color:red">Returns</span>: <b>{point.y}</b><br/>'
        )
      ),
      lineWidth = 1,
      yAxis = 0,
      color = rep(chartColors, length.out = (ncol(z) - 1)),
      id = unique(Rets[Rets$tkr != bmname, 2])
    ) %>%
    hc_add_series(
      linkedTo = bmname,
      SD[SD$tkr == bmname, ],
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      lineWidth = 1,
      yAxis = 1,
      dashStyle = 'longdash',
      color = 'black',
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{point.color}">\u25CF</span> {series.name} <span style="color:darkred">StdDev.</span>: <b>{point.y}</b><br/>'
        )
        
      )
    ) %>%
    hc_add_series(
      linkedTo = unique(SD[SD$tkr != bmname, 2]),
      SD[SD$tkr != bmname, ],
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      lineWidth = 1,
      yAxis = 1,
      color = rep(chartColors, length.out = (ncol(z) - 1)),
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{point.color}">\u25CF</span> {series.name} <span style="color:darkred">StdDev.</span>: <b>{point.y}</b><br/>'
        )
        
      )
    ) %>%
    hc_add_series(
      linkedTo = bmname,
      SR[SR$tkr == bmname, ],
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      lineWidth = 1,
      yAxis = 2,
      dashStyle = 'longdash',
      color = 'black',
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{point.color}">\u25CF</span> {series.name} <span style="color:green">Sharpe Ratio</span>: <b>{point.y}</b><br/>'
        )
        
      )
    ) %>%
    hc_add_series(
      linkedTo = unique(SR[SR$tkr != bmname, 2]),
      SR[SR$tkr != bmname, ],
      type = "spline",
      hcaes(x = Date, y = Close, group = tkr),
      lineWidth = 1,
      yAxis = 2,
      color = rep(chartColors, length.out = (ncol(z) - 1)),
      tooltip = list(
        valueDecimals = 2,
        pointFormat = HTML(
          '<span style="color:{point.color}">\u25CF</span> {series.name} <span style="color:green">Sharpe Ratio</span>: <b>{point.y}</b><br/>'
        )
        
      )
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
    hc_rangeSelector(enabled = F) %>%
    hc_navigator(enabled = F) %>%
    hc_scrollbar(enabled = F)
  hc3
})

### Section Three Table #####

output$port_sectionThree_metrics_tbl <- renderDataTable({
  shiny::validate(need(nrow(RVs$port_sectionThree_Rets) > 0, "NO DATA"))
  z <-  RVs$port_sectionThree_Rets
  Rb <- names(z)[ncol(z)]
  Ra <- setdiff(names(z), Rb)
  
  a <- z[, Ra]
  b <- z[, Rb]
  df <- fnCreateMetricsTbl(Ra = a, Rb = b)
  req(nrow(df) > 0, cancelOutput = T)
  names(df)[1] <- "Port."
  
  x <- datatable(
    df,
    rownames = FALSE ,
    class = "compact stripe",
    options = list(
      dom = 't',
      paging = FALSE,
      scrollY = "400px",
      scrollX = F
    ),
    height = "auto"
    ,
    callback = JS(
      HTML(
        '$("#port_sectionThree_metrics_tbl > div > div > div.dataTables_scrollBody").css("height","auto")'
      )
    ),
    caption = tags$caption(
      style = "font-size:9px;padding-bottom: 0px; text-align:left;color: #bcbcbc;",
      "Beta, Treynor:",
      Rb ,
      "|",
      "Sharpe method = StdDev",
      "|",
      "VaR, ES method = modified (p=95%)",
      "|",
      "Risk free % = 0"
    )
  ) %>%
    formatRound(
      columns = c(
        "Beta",
        "Treynor Ratio",
        "Sharpe Ratio",
        "VaR",
        "ES",
        "Mean",
        "StdDev"
      ),
      digits = 4
    )
  
  return(x)
})


