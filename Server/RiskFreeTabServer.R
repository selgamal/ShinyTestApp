# Risk Free Tab server side

# To reduce time on scraping Yield curve table from treasury.us website, current year rates 
# are added to an existing RDS ("RDSs/YCHistory.rds") with the daily rates table from begining of 
# 2007 until end of of 2016, that enhances perfomance significantly as opposed to downloading 
# the whole table every session. Also if "UsePreLoadedTkrData" is set to TRUE, a pre-existing
# dataset will be loaded instead of updating to current year.

##Update UI inputs for dataset params
updateDateInput(session = session,
                inputId = "slct_YCdates1", 
                min = min(YC_melt$Date),
                max = max(YC_melt$Date),
                value = max(YC_melt$Date)
)
updateDateInput(session = session,
                inputId = "slct_YCdates2", 
                min = min(YC_melt$Date),
                max = max(YC_melt$Date)
)
updateDateInput(session = session,
                inputId = "slct_YCdates3", 
                min = min(YC_melt$Date),
                max = max(YC_melt$Date)
)

updateCheckboxGroupInput(session = session,
                         inputId = "slct_YCterms",
                         choices = unique(YC_monthly.df$variable),
                         selected = unique(YC_monthly.df$variable)[1:2],
                         inline = T
)

output$hc_YieldCurve <- renderHighchart(
  {
    req(nrow(YC_melt)>0)
    req(length(YC_xts)>0)
    req(length(c(input$slct_YCdates1, input$slct_YCdates2, input$slct_YCdates3))>0)
    runjs(HTML("$('.shiny-date-input > .form-control').prop('readonly', true)"))
    y<- YC_xts
    
    d <- c(input$slct_YCdates1, input$slct_YCdates2, input$slct_YCdates3) #input$slct_YCdates #tail(unique(YC_melt$Date),3)
    
    loadyc <- JS(HTML("function() {
                 var ycloaded = Math.random();
                      Shiny.onInputChange('YCmotionchartloaded', ycloaded);}"))
    
    highchart() %>%
      hc_chart(type = "spline",
               events = list(
                 load = loadyc
               )
               ) %>%
      hc_motion(enabled = TRUE, labels = as.character(index(y)), series = 0 ,autoplay = F, updateInterval = 1)  %>%
      hc_add_series(
        name = "Yield Curve Over time (Playable)",
        lineWidth = 1,
        color = "red",
        data = list(list(sequence = y$`1M`),
                    list(sequence = y$`3M`),
                    list(sequence = y$`6M`),
                    list(sequence = y$`1Y`),
                    list(sequence = y$`2Y`),
                    list(sequence = y$`3Y`),
                    list(sequence = y$`5Y`),
                    list(sequence = y$`7Y`),
                    list(sequence = y$`10Y`),
                    list(sequence = y$`20Y`),
                    list(sequence = y$`30Y`)
        )) %>%
      hc_add_series(
        filter(YC_melt, Date %in% d),
        hcaes(x = variable, 
              y = value, 
              group = DateMDY ),
        type = "spline",
        dataLabels = list(
          enabled = T,
          style = list(fontSize = "7px", textOutline = 0),
          formatter = JS('function () {
                         if (this.x == "30Y") return this.series.name
                         }'),
          color = "#666666",
          backgroundColor= "lightblue"
          )
      ) %>%
      hc_tooltip(
           valueSuffix = "%", 
             pointFormat= '<span style="color:{point.color}">\u25CF</span>{series.name}: <b>{point.y:.2f}%</b><br/>'
      ) %>%
      hc_xAxis(categories =names(y), 
               labels = list(fontSize = "9px"),
               title = list(text = "Maturities", style = list(fontSize="9px"))) %>%
      hc_yAxis(labels = list(style = list(fontSize = "8px"), format = '{value}%'))  %>%
      hc_legend(enabled = T)%>%
      hc_add_theme(thm) %>%
      # hc_credits(enabled = TRUE,
      #            text = "Based On Data From US Treasury Site",
      #            href = "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield") %>%
      hc_legend(
        title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
        style = list(fontStyle = 'italic'),
        enabled = T,
        symbolPadding =1,
        symbolWidth=10,
        itemStyle = list(fontSize = "9px")
      )
    
  }
)

observeEvent(
  input$YCmotionchartloaded,
  runjs("$('#hc_yc > #play-controls').each(function() {
    $(this).remove();
        });
       $('#play-controls').each(function() {
                  $(this).parent().after(this);
        });
        $(window).trigger('resize');")
)

output$hc_YieldCurveTerms <- renderHighchart(
  {
    req(nrow(YC_monthly.df)>0)
    req(length(input$slct_YCterms)>0)
    highchart(type = "stock")%>%
      #hc_chart(events = list(load = redrawx)) %>%
      hc_xAxis(minRange = 30 * 24 * 3600 * 1000, categories = unique(YC_monthly.df$Date) ) %>%
      hc_yAxis(labels = list(style = list(fontSize = "8px"), format = '{value}%')) %>%
      hc_add_series(
        data = filter(YC_monthly.df, variable %in% input$slct_YCterms), 
        hcaes(x = Date,
              y = value,
              group = variable),
        type = 'spline',
        lineWidth = 1.5,
        tooltip = list(valueSuffix = "%",
                       xDateFormat = '%b \'%y',
                       pointFormat= '<span style="color:{point.color}">\u25CF</span>{series.name}: <b>{point.y:.2f}%</b><br/>')
      ) %>%
      hc_rangeSelector(buttons = list(
        list(type = 'month', count = 6, text = '6M'),
        list(type = 'year', count = 1, text = '1Y'),
        list(type = 'year', count = 3, text = '3Y'),
        list(type = 'year', count = 5, text = '5Y'),
        list(type = 'year', count = 7, text = '7Y'),
        list(type = 'all', text = 'All')
      ),
      selected = 3
      ) %>%
      hc_legend(enabled = T) %>%
      hc_add_theme(thm) %>%
      hc_navigator(height= 25) %>%
      hc_scrollbar(height= 3)%>%
      hc_legend(
        title = list(text = '<span style="font-size: 9px; color: #666; font-weight: normal">(Click to show/hide series)</span>'),
        style = list(fontStyle = 'italic'),
        enabled = T,
        symbolPadding =1,
        symbolWidth=10,
        itemStyle = list(fontSize = "9px")
      )
  })


output$MonthlyYC <- renderDataTable(
  {
    YCdf.mon.perc <- YCdf.mon
    YCdf.mon.perc[, 2:12] <- YCdf.mon.perc[, 2:12]/100
    datatable(
      YCdf.mon.perc,
      rownames = F,
      class = "compact stripe",
      options = list(dom = 't',
                     columnDefs = list(
                       list(visible = FALSE,
                            targets = c(12)),
                       list(iDataSort = 12, targets = 0)
                     )
                     , scrollY = "500px"
                     , scrollX=F
                     , paging = FALSE
                     , order = list(list(0, "desc"))
      ),height = "100%", 
      caption = tags$caption(
        style = "font-size:18px;padding-top: 0px;color: black;",
        "Monthly Yield Curve Rates"
      )
      
    ) %>% 
      formatPercentage(
        c("1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "20Y", 
          "30Y"), 2)
  }
)

