
# Financial information tables and chart div ####

# Prep data to be used in FS tables and charts ####

observeEvent(RVs$fs_ratio_slctd_tkr,
             {
               req(length(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$currency) > 0, cancelOutput = T)
               tkrCurr.a <- gsub("Currency ",
                                 "",
                                 RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$currency,
                                 ignore.case = T)
               
               tkrCurr.a <-
                 paste(" (", tkrCurr.a, " Millions", ")", sep = "")
               
               output$bs_currency <- renderText(tkrCurr.a)
               output$is_currency <- renderText(tkrCurr.a)
               output$cf_currency <- renderText(tkrCurr.a)
               
               
             })


observeEvent({
  RVs$fs_ratio_slctd_tkr #input$js_tkrlnk[1]
  input$Period
  
},
{
  #shiny::validate(need(input$js_tkrlnk[1], "Please select a ticker"))
  if (is.na(RVs$fs_ratio_slctd_tkr)) {
    RVs$fs_dataplt <- NULL
    RVs$fs_data <- NULL
    RVs$ratios_data <- NULL
  } else if (is.null(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$fs)) {
    RVs$fs_dataplt <- NULL
    RVs$fs_data <- NULL
    RVs$ratios_data <-
      RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$ratios
  } else {
    req(!is.na(RVs$fs_ratio_slctd_tkr), cancelOutput = T)
    req(!is.null(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$fs), cancelOutput = T)
    
    z <-
      filter(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$fs, Period == input$Period)
    
    
    b <- dcast(z, type + group + category ~ date, sum)
    
    noncurrentassets <-
      data.frame(
        type = "BS",
        group = 16,
        category = "Total Non-Current Assets",
        b[b$group == 17 &
            b$type == "BS", c(4:ncol(b))] - b[b$group == 10 &
                                                b$type == "BS", c(4:ncol(b))],
        check.names = F,
        stringsAsFactors = F
      )
    
    noncurrentliab <-
      data.frame(
        type = "BS",
        group = 30,
        category = "Total Non-Current Liabilities",
        b[b$group == 31 &
            b$type == "BS", c(4:ncol(b))] - b[b$group == 23 &
                                                b$type == "BS", c(4:ncol(b))],
        check.names = F,
        stringsAsFactors = F
      )
    
    b[b$type == "BS" & b$group == 27,]$group <- 43
    b[b$type == "BS" &
        b$group == 43,]$category <-
      "*Total Debt (Short & Long term)"
    b[b$type == "BS" & b$group == 31,]$category <-
      "Total Liabilities*"
    b[b$type == "BS" &
        b$group == 40,]$category <- "Total Liabilities & Equity"
    
    OperatingExpenses <-
      data.frame(
        type = "IS",
        group = 12,
        category = "Total Operating Expenses",
        b[b$group == 13 &
            b$type == "IS", c(4:ncol(b))] -
          b[b$group == 5 &
              b$type == "IS", c(4:ncol(b))]
        ,
        check.names = F,
        stringsAsFactors = F
      )
    
    Otherexpenses <-
      data.frame(
        type = "IS",
        group = 16,
        category = "Net Other Income/Expenses",
        b[b$group == 17 &
            b$type == "IS", c(4:ncol(b))] -
          b[b$group == 13 &
              b$type == "IS", c(4:ncol(b))]
        ,
        check.names = F,
        stringsAsFactors = F
      )
    
    
    incomeTax <-
      data.frame(
        type = "IS",
        group = 17.5,
        category = "Income Tax",
        b[b$group == 18 &
            b$type == "IS", c(4:ncol(b))] -
          b[b$group == 17 &
              b$type == "IS", c(4:ncol(b))]
        ,
        check.names = F,
        stringsAsFactors = F
      )
    
    
    
    nonrecurring <-
      data.frame(
        type = "IS",
        group = 24,
        category = "Net Non-recurring & Discont. Ops.",
        b[b$group == 25 &
            b$type == "IS", c(4:ncol(b))] -
          b[b$group == 21 &
              b$type == "IS", c(4:ncol(b))]
        ,
        check.names = F,
        stringsAsFactors = F
      )
    
    c <- rbind.data.frame(
      noncurrentassets,
      noncurrentliab,
      OperatingExpenses,
      Otherexpenses,
      incomeTax,
      nonrecurring,
      #totalDebt,
      b,
      make.row.names = F
    )
    
    c[c$category == "Net Income Before Extra. Items", "category"] = "Net Income From Continuing Ops"
    c[c$category == "Basic EPS Including Extraordinary Items", "category"] = "Basic EPS"
    #c[c$category =="Net Change in Cash","category" ] = "Net Change in Cash including forex. effect"
    
    c[c$type == "IS" &
        c$group == 4, c(4:ncol(c))] = -c[c$type == "IS" &
                                           c$group == 4, c(4:ncol(c))]
    
    d <- c[with(c, order(type, group)),]
    #e <- scales::comma(d)
    RVs$fs_dataplt <- d
    for (i in 4:ncol(d)) {
      d[, i] <- as.character(accounting(d[, i]))
      
    }
    names(d)[-c(1, 2)] <-
      c("Caption", as.character(as.Date(names(d[-c(1:3)])), format = "%b %d, %Y"))
    d = d[, c(1, 2, 3, ncol(d):4)]
    #return(d)
    RVs$fs_data <- d
    
    RVs$ratios_data <-
      RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$ratios
  }
})



output$BS <- renderTable({
  shiny::validate(need(length(RVs$fs_data) > 0, "NO DATA"))
  RVs$fs_data[RVs$fs_data$type == "BS", -c(1, 2)]
}, width = "100%", striped = T)

output$BSplot1 <- renderHighchart({
  shiny::validate(need(length(RVs$fs_dataplt) > 0, "NO DATA"))
  
  a <- filter(
    RVs$fs_dataplt,
    category %in% c(
      "Total Assets",
      "Total Liabilities*",
      "Total Equity",
      "*Total Debt (Short & Long term)"
    )
  )[, c(3:ncol(RVs$fs_data))] %>%
    melt(id = "category")
  a$variable <-
    as.character(as.Date(a$variable), format = "%b %d, %Y")
  b <-
    join_all(dfs = list(
      filter(
        a,
        category %in% c(
          "Total Liabilities*",
          "Total Equity",
          "*Total Debt (Short & Long term)"
        )
      )
      ,
      filter(a, category == "Total Assets")[, c(2, 3)]
      ,
      filter(a, category == "*Total Debt (Short & Long term)")[, c(2, 3)]
    )
    , by = "variable")
  
  b$percent <- paste0(round(b[, 3] / b[, 4] * 100, 2), "%")
  b$DebtToEquity <- round(b[, 5] / b[, 3] * 100, 2)
  b$DebtToEquityPerc <- paste0(b$DebtToEquity, "%")
  b[b$category == "Total Liabilities*", 1] = "Liabilities"
  b[b$category == "Total Equity", 1] = "Equity"
  b[b$category == "*Total Debt (Short & Long term)", 1] = "Total Debt"
  names(b) <-
    c(
      "Item",
      "YearEnded",
      "Balance",
      "TotalAssets",
      "TotalDebt",
      "Percentage",
      "DebtToEquity",
      "DebtToEquityPerc"
    )
  
  hchart <-
    highchart() %>%
    hc_xAxis(categories = unique(b$YearEnded),
             labels = list(style = list(fontSize = "8px"))) %>%
    hc_yAxis_multiples(list(
      labels = list(style = list(fontSize = "8px")
                    , format = '{value:,.0f} M')
      ,
      title = list(text = NULL)
    )
    ,
    list(
      labels = list(style = list(fontSize = "8px")
                    , format = '{value}%')
      ,
      title = list(text = NULL),
      opposite = T
    )) %>%
    hc_add_series(
      b[b$Item != "Total Debt", ],
      type = "column",
      hcaes(
        x = YearEnded,
        y = Balance,
        y2 = Percentage ,
        group = Item
      ),
      stack = 'Assets',
      tooltip = list(valueSuffix = " M")
    ) %>%
    hc_add_series(
      b[b$Item == "Total Debt", ],
      type = "column",
      hcaes(
        x = YearEnded,
        y = Balance,
        y2 = Percentage ,
        group = Item
      ),
      stack = 'Debt',
      tooltip = list(valueSuffix = " M")
    ) %>%
    hc_add_series(
      b[b$Item == "Equity", ],
      type = "spline",
      name = "Debt To Equity"
      ,
      hcaes(x = YearEnded, y = DebtToEquity)
      ,
      yAxis = 1
      ,
      tooltip = list(valueSuffix = "%")
    ) %>%
    hc_plotOptions(
      column = list(
        borderWidth = 0,
        dataLabels = list(
          enabled = T,
          format = '{point.y2}'
          ,
          style = list(fontSize = "8px", textOutline = F)
          ,
          color = "#666666"
        ),
        stacking = "normal"
      )
      ,
      series = list(pointWidth = 35)
    ) %>%
    hc_legend(itemStyle = list(fontSize = "10px"),
              title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
              style = list(fontStyle = 'italic')) %>%
    hc_tooltip(shared = T) %>%
    hc_add_theme(thm) 
  hchart$x$conf_opts$lang$thousandsSep <- ","
  
  hchart
  
})

output$BSplot2 <- renderHighchart({
  shiny::validate(need(length(RVs$ratios_data) >
                  0, "NO DATA"))
  Ratios_tkrs_df <- RVs$ratios_data
  a <- filter(Ratios_tkrs_df
              ,
              sub.section == "Liquidty/Financial Health" &
                !is.na(value))[, c(2:ncol(Ratios_tkrs_df))]
  a$date <- as.character(a$date, format = "%Y") #%b %d,
  
  a$value <- formattable(a$value, digits = 2, format = "f")
  

  a <- rbind(data.frame(
    sub.section = c(rep("Liquidty/Financial Health"))
    ,
    group = c(74, 75, 76)
    ,
    category = c("Current Ratio", "Quick Ratio", "Financial Leverage")
    ,
    date = c(1901, 1901, 1901)
    ,
    value = c(NA, NA, NA)
  )
  ,
  a)
  
  hcharty <-
    highchart() %>%
    hc_xAxis(
      categories = unique(a$date),
      floor = 1,
      labels = list(style = list(fontSize = "8px"))
    ) %>%
    hc_yAxis(
      labels = list(style = list(fontSize = "8px")
                    , format = '{value:.2f}')
      ,
      title = list(text = "")
    ) %>%
    hc_add_series(a[a$category == "Financial Leverage", ],  type = "column", name =
                    "Financial Leverage" , hcaes(x = date, y = value)) %>%
    hc_add_series(a[a$category == "Current Ratio", ],  type = "spline", name =
                    "Current Ratio" , hcaes(x = date, y = value)) %>%
    hc_add_series(a[a$category == "Quick Ratio", ],  type = "spline", name =
                    "Quick Ratio" , hcaes(x = date, y = value)) %>%
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
      series = list(pointWidth = 40)
      
    ) %>%
    hc_legend(itemStyle = list(fontSize = "10px"),
              title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
              style = list(fontStyle = 'italic')) %>%
    hc_tooltip(shared = T, pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>') %>%
    hc_add_theme(thm)
  hcharty$x$conf_opts$lang$thousandsSep <- ","
  
  hcharty
})



#updateSelectInput(session, inputId = "ratio", choices = unique(rv$Ratiosdf$category), selected = head(rv$Ratiosdf$category,3))

output$BSplot3 <- renderHighchart({
  shiny::validate(need(length(RVs$ratios_data) >
                  0, "NO DATA"))
  Ratios_tkrs_df <- RVs$ratios_data
  a <- filter(Ratios_tkrs_df,
              sub.section == "Efficiency" &
                !is.na(value))[, c(2:ncol(Ratios_tkrs_df))]
  a$date <- as.character(a$date, format = "%Y")
  
  a$suffix <- ifelse(
    a$category %in% c(
      "Days Sales Outstanding",
      "Days Inventory",
      "Payables Period",
      "Cash Conversion Cycle"
    )
    ,
    "days"
    ,
    "times"
  )
  shiny::validate(need(length(input$ratio) > 0, 'Please Select a Ratio'))
  
  a <- rbind(data.frame(
    sub.section = c(rep("Liquidty/Financial Health"))
    ,
    group = c(78:85)
    ,
    category = c(
      "Days Sales Outstanding",
      "Days Inventory" ,
      "Payables Period",
      "Cash Conversion Cycle",
      "Receivables Turnover",
      "Inventory Turnover",
      "Fixed Assets Turnover",
      "Asset Turnover"
    )
    ,
    date = c(rep(1901, 8))
    ,
    value = c(rep(NA, 8))
    ,
    suffix = c(rep("days", 4), rep("times", 4))
  )
  
  ,
  a)
  
  hchartz <-
    highchart() %>%
    hc_xAxis(
      categories = unique(a$date),
      floor = 1,
      labels = list(style = list(fontSize = "8px"))
    ) %>%
    hc_yAxis(
      labels = list(style = list(fontSize = "8px")
                    , format = '{value:.2f}')
      ,
      title = list(text = "")
    ) %>%
    hc_add_series(
      a[a$category %in% input$ratio, c(3:6)],
      type = "spline",
      hcaes(
        x = date,
        y = value,
        y2 = suffix ,
        group = category
      )
      ,
      tooltip = list(valueSuffix = "{point.y2}")
    ) %>% #
    hc_plotOptions(column = list(
      dataLabels = list(
        enabled = T,
        format = '{point.y:.2f}'
        ,
        style = list(fontSize = "8px")
        ,
        inside = F,
        y = 5
      )
    )
    ,
    series = list(pointWidth = 40)) %>%
    hc_legend(itemStyle = list(fontSize = "10px"),
              title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
              style = list(fontStyle = 'italic')) %>% #layout="vertical", align = "right", verticalAlign="top")%>%
    hc_tooltip(shared = T, pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f} {point.y2}</b><br/>') %>%
    hc_add_theme(thm)
  hchartz$x$conf_opts$lang$thousandsSep <- ","
  
  hchartz
  
  
})

output$IS <- renderTable({
  shiny::validate(need(length(RVs$fs_data) > 0, "NO DATA"))
  RVs$fs_data[RVs$fs_data$type == "IS", -c(1, 2)]
  
}, width = "100%", striped = T)


output$ISplot1 <- renderHighchart({
  shiny::validate(need(length(RVs$fs_dataplt) > 0, "NO DATA"))
  
  a <- filter(RVs$fs_dataplt,
              category %in% c("Total Revenue", "Net Income"))[, c(3:ncol(RVs$fs_dataplt))] %>%
    melt(id = "category")
  a$variable <-
    as.character(as.Date(a$variable), format = "%b %d, %Y")
  
  b <- join_all(dfs = list(a
                           , filter(a, category == "Total Revenue")[, c(2, 3)])
                , by = "variable")
  
  b$percent <- round(b[, 3] / b[, 4] * 100, 2)
  
  names(b) <-
    c("Caption" , "Date" , "Balance"  ,  "TotalRev", "Percent")
  
  hchart_IS_a <-
    highchart() %>%
    hc_xAxis(categories = unique(b$Date),
             labels = list(style = list(fontSize = "8px"))) %>%
    hc_yAxis_multiples(list(
      labels = list(style = list(fontSize = "8px")
                    , format = '{value:.,f} M')
      ,
      title = list(text = "")
    )
    ,
    list(
      labels = list(style = list(fontSize = "8px")
                    , format = '{value:.2f}%')
      ,
      title = list(text = NULL),
      opposite = T
    )) %>%
    hc_add_series(
      b,
      type = "column" ,
      hcaes(x = Date, y = Balance, group = Caption),
      index = c(1, 0),
      color = c('#7cb5ec', '#90ed7d'),
      tooltip = list(valueSuffix = " M")
    ) %>%
    hc_add_series(
      b[b$Caption == "Net Income",],
      type = "spline",
      name = "Net Income %" ,
      hcaes(x = Date, y = Percent),
      color = '#f45b5b' ,
      yAxis = 1,
      tooltip = list(valueSuffix = "%")
    ) %>%
    hc_plotOptions(
      column = list(
        borderWidth = 0,
        dataLabels = list(
          enabled = T,
          format = '{point.y:.,f} M'
          ,
          style = list(fontSize = "8px", textOutline = F)
          ,
          inside = F,
          y = 3,
          color = "#666666"
        )
      )
      ,
      series = list(pointWidth = 40)
      
    ) %>%
    hc_legend(
      itemStyle = list(fontSize = "10px"),
      title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
      style = list(fontStyle = 'italic')
    ) %>%
    # hc_title(text = "Total Revenue & Net Income",
    #          style = list(fontFamily = '"Helvetica Neue",Helvetica,Arial,sans-serif'), align = "left") %>%
    hc_tooltip(shared = T) %>%
    #hc_chart(backgroundColor = "balck")
    hc_add_theme(thm)
  
  hchart_IS_a$x$conf_opts$lang$thousandsSep <- ","
  
  hchart_IS_a
  
})

output$ISplot3 <- renderHighchart({
  shiny::validate(need(length(RVs$ratios_data) >
                  0, "NO DATA"))
  Ratios_tkrs_df <- RVs$ratios_data
  a <- filter(
    Ratios_tkrs_df,
    section == "Profitability" &
      group %in% c(17, 18, 19, 20, 22, 24) & !is.na(value)
  )[, c(2:ncol(Ratios_tkrs_df))]
  a$date <- as.character(a$date, format = "%Y")
  
  a <- rbind(data.frame(
    sub.section = c(rep("Profitability"))
    ,
    group = c(17, 18, 19, 20, 22, 24)
    ,
    category = c(
      "COGS",
      "Gross Margin",
      "SG&A",
      "R&D",
      "Operating Margin",
      "EBT Margin"
    )
    ,
    date = c(rep(1901))
    ,
    value = c(rep(NA))
  )
  ,
  a)
  
  hchartISc <-
    highchart() %>%
    hc_xAxis(
      categories = unique(a$date),
      floor = 1,
      labels = list(style = list(fontSize = "8px"))
    ) %>%
    hc_yAxis(
      labels = list(style = list(fontSize = "8px")
                    , format = '{value:.2f}%')
      ,
      title = list(text = ""),
      ceiling = 100
    ) %>%
    hc_add_series(
      a[a$category %in% c("Gross Margin", "COGS"), ],
      type = "column",
      hcaes(x = date, y = value, group = category)
      ,
      stack = "GM",
      stacking = "normal",
      tooltip = list(valueSuffix = "%"),
      legendIndex = c(0, 1)
    ) %>%
    hc_add_series(
      a[a$category %in% c("SG&A", "R&D"), ],
      type = "column",
      index = c(1, 0),
      hcaes(x = date, y = value, group = category)
      ,
      stack = "Costs",
      tooltip = list(valueSuffix = "%"),
      legendIndex = c(3, 2)
    ) %>%
    hc_add_series(
      a[a$category %in% c("EBT Margin", "Operating Margin"), ],
      type = "spline",
      hcaes(x = date, y = value, group = category)
      ,
      tooltip = list(valueSuffix = "%"),
      legendIndex = c(4, 5)
    ) %>%
    hc_plotOptions(column = list(borderWidth = 0,
                                 series = list(pointWidth = 12))) %>%
                     hc_legend(itemStyle = list(fontSize = "10px"),
                               title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
                               style = list(fontStyle = 'italic')) %>%
                     hc_tooltip(shared = T) %>%
                     hc_add_theme(thm)
                   hchartISc$x$conf_opts$lang$thousandsSep <- ","
                   hchartISc
})
  
  
  output$ISplot2 <- renderHighchart({
    shiny::validate(need(length(RVs$ratios_data) >
                    0, "NO DATA"))
    Ratios_tkrs_df <- RVs$ratios_data
    a <- filter(
      Ratios_tkrs_df
      ,
      section == "Profitability"
      & sub.section == "Profitability"
      & group %in% c(26, 28, 30, 31)
    )[, c(2:ncol(Ratios_tkrs_df))]
    
    a$date <- as.character(a$date, format = "%Y")
    
    
    a <- rbind(data.frame(
      sub.section = c(rep("Profitability"))
      ,
      group = c(26, 28, 30, 31)
      ,
      category = c(
        "Net Margin %",
        "Return on Assets %",
        "Return on Equity %",
        "Return on Invested Capital %"
      )
      ,
      date = c(rep(1901))
      ,
      value = c(rep(NA))
    )
    ,
    a)
    
    
    
    hchartISb <-
      highchart() %>%
      hc_xAxis(
        categories = unique(a$date),
        floor = 1,
        labels = list(style = list(fontSize = "8px"))
      ) %>%
      hc_yAxis(
        labels = list(style = list(fontSize = "8px")
                      , format = '{value:.2f}%')
        ,
        title = list(text = "")#, ceiling = 50
      ) %>%
      hc_add_series(
        a[a$category %in% c("Return on Assets %",
                            "Return on Equity %",
                            "Return on Invested Capital %"), ],
        type = "column",
        hcaes(x = date, y = value, group = category),
        tooltip = list(valueSuffix = "%")
      ) %>% #, legendIndex = c(0,1)
      hc_add_series(
        a[a$category == "Net Margin %", ],
        type = "spline",
        hcaes(x = date, y = value, group = category)
        ,
        tooltip = list(valueSuffix = "%")
      ) %>% #, legendIndex = c(4,5)
      hc_plotOptions(column = list(borderWidth = 0,
                                   series = list(pointWidth = 12))) %>%
                       hc_legend(itemStyle = list(fontSize = "10px"),
                                 title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
                                 style = list(fontStyle = 'italic')) %>%
                       hc_tooltip(shared = T) %>%
                       hc_add_theme(thm)
                     hchartISb$x$conf_opts$lang$thousandsSep <- ","
                     
                     hchartISb
  })
  
  output$CF <- renderTable({
    shiny::validate(need(length(RVs$fs_data) > 0, "NO DATA"))
    RVs$fs_data[RVs$fs_data$type == "CF", -c(1, 2)]
  }, width = "100%", striped = T)
  
    output$CFplot2 <- renderHighchart({
      shiny::validate(need(length(RVs$ratios_data) >
                      0, "NO DATA"))
      Ratios_tkrs_df <- RVs$ratios_data
      a <- filter(Ratios_tkrs_df,
                  group %in% c(13, 14, 52, 53)
                  #& !is.na(value)
                  )[, c(2:ncol(Ratios_tkrs_df))]
                  
                  a$date <- as.character(a$date, format = "%Y")
                  
                  a$category <-
                    gsub("Free Cash Flow Per Share \\* USD",
                         "Free Cash Flow Per Share",
                         a$category)
                  a$category <-
                    gsub("Free Cash Flow USD Mil", "Free Cash Flow", a$category)
                  
                  
                  a <- rbind(data.frame(
                    sub.section = c(rep("Financials", 2), rep("Cash Flow Ratios", 2))
                    ,
                    group = c(13, 14, 52, 53)
                    ,
                    category = c(
                      "Free Cash Flow"
                      ,
                      "Free Cash Flow Per Share"
                      ,
                      "Free Cash Flow/Sales %"
                      ,
                      "Free Cash Flow/Net Income"
                    )
                    ,
                    date = c(rep(1901))
                    ,
                    value = c(rep(NA))
                  )
                  ,
                  a)
                  
                  hchart_IS_a <-
                    highchart() %>%
                    hc_xAxis(
                      categories = unique(a$date),
                      floor = 1,
                      labels = list(style = list(fontSize = "8px"))
                    ) %>%
                    hc_yAxis_multiples(
                      list(
                        labels = list(style = list(fontSize = "8px")
                                      , format = '{value:.,f} M')
                        ,
                        title = list(text = "")
                      )
                      ,
                      list(
                        labels = list(style = list(fontSize = "8px")
                                      , format = '{value:.2f}%')
                        ,
                        title = list(text = NULL),
                        opposite = T
                      ),
                      list(
                        labels = list(style = list(fontSize = "8px")
                                      , format = '${value:.2f}')
                        ,
                        title = list(text = NULL),
                        opposite = T
                      )
                    ) %>%
                    hc_add_series(
                      a[a$category == "Free Cash Flow" , ],
                      type = "column" ,
                      hcaes(x = date, y = value, group = category),
                      tooltip = list(valueSuffix = " M")
                    ) %>%
                    hc_add_series(
                      a[a$category %in% c("Free Cash Flow/Net Income", "Free Cash Flow/Sales %"), ],
                      type = "spline",
                      hcaes(x = date, y = value, group = category),
                      yAxis = 1,
                      tooltip = list(valueSuffix = "%")
                    ) %>%
                    hc_add_series(
                      a[a$category == "Free Cash Flow Per Share" , ],
                      type = "spline",
                      hcaes(x = date, y = value, group = category),
                      yAxis = 2,
                      tooltip = list(valuePrefix = "$")
                    ) %>%
                    hc_plotOptions(
                      column = list(
                        borderWidth = 0,
                        dataLabels = list(
                          enabled = T,
                          format = '{point.y:.,f} M'
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
                              style = list(fontStyle = 'italic')) %>%
                    hc_tooltip(shared = T) %>%
                    hc_add_theme(thm)
                  hchart_IS_a$x$conf_opts$lang$thousandsSep <- ","
                  
                  hchart_IS_a
                  
    })
    
    
    output$CFplot1 <- renderHighchart({
      shiny::validate(need(length(RVs$fs_dataplt) > 0, "NO DATA"))
      CFplotloaded <- JS(
        HTML(
          '
          function() {
          $(window).resize();
          var TDs = $(\"#tkr_fs_ratios table > tbody > tr > td:not(:first-child)\");
          for (var i=0; i<TDs.length; i++) {
          var temp = TDs[i];
          if (temp.innerHTML.indexOf(\"(\")==1) $(temp).css(\"color\", \"red\");
          }
          }
          '
          )
          )
      a <- filter(RVs$fs_dataplt,
                  type == "CF")[, c(3:ncol(RVs$fs_dataplt))] %>%
        melt(id = "category")
      
      
      a$variable <-
        as.character(as.Date(a$variable), format = "%b %d, %Y")
      
      hchartCF1 <-
        highchart() %>%
        hc_xAxis(categories = unique(a$variable),
                 labels = list(style = list(fontSize = "8px"))) %>%
        hc_yAxis(
          labels = list(style = list(fontSize = "8px")
                        , format = '{value:.,0f} M')
          ,
          title = list(text = "")
        ) %>%
        hc_add_series(
          a[a$category %in% c(
            "Cash from Operating Activities",
            "Cash from Investing Activities",
            "Cash from Financing Activities"
          ), ]
          ,
          type = "column",
          hcaes(x = variable, y = value, group = category)
          ,
          index = c(2, 1, 0)
        ) %>%
        hc_add_series(a[a$category == "Net Change in Cash", ]
                      ,  type = "spline", hcaes(x = variable, y = value, group = category)) %>%
        hc_plotOptions(
          column = list(
            borderWidth = 0,
            dataLabels = list(
              enabled = T,
              format = '{point.y:.,0f} M'
              ,
              style = list(fontSize = "8px", textOutline = F)
              ,
              inside = F
            ) #, y = 5 )
          )
          ,
          series = list(pointWidth = 25)
          
        ) %>%
        hc_legend(
          itemStyle = list(fontSize = "10px"),
          title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
          style = list(fontStyle = 'italic')
        ) %>% #layout="vertical", align = "right", verticalAlign="top")%>%
        hc_tooltip(shared = T, valueSuffix = " M") %>%
        hc_add_theme(thm) %>%
        hc_chart(events = list(load = CFplotloaded))
      hchartCF1$x$conf_opts$lang$thousandsSep <- ","
      
      hchartCF1
      
          })
    
      
      

      
            
