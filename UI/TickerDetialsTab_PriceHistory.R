# The UI for div showing Price history analysis ####


tags$div(
  id = "tkr_price_history",
  checkboxInput(
    "UseAdjusted",
    "Use Prices Adjusted for Dividends",
    width = "100%"
    ,
    value = T
  ),
  verbatimTextOutput("adj"),
  verbatimTextOutput("hcsym"),
  fluidRow(
    column(
      width = 12,
      style = "font-size:80%;",
      tags$h4("Price History", actionLink(inputId = "btn_IndicatorsOpts", "Change Indicators Inputs", icon = icon("info-circle"), style = "color:cornflowerblue;font-size: 60%;")),
      radioButtons(inputId = "slct_chartpkg", 
                   label = "Select Chart Package",
                   choices = c("Quantmod ChartSeries (Best Performance)" = 1,
                               "HighCharts (Best Looks and options - takes longer to load)"=2),
                   selected = 1,
                   inline = T,
                   width = "100%"
                   ),
      bsModal(
        id = "Chart1Opts",
        title = "Indicators Inputs",
        trigger = "btn_IndicatorsOpts",
        size = "small",
                   div(id = "BBInputs",
                       verticalLayout(
                         tags$h5("Bollinger Bands Inputs", style = "font-weight: 600;"),
                         flowLayout(
                           id = "bbinput1",
                           tags$h6("Number of Periods For Moving Average", style = "margin-top: 5px;"),
                           numericInput(
                             inputId = "BBnPeriods",
                             label = NULL,
                             value = 20,
                             min = 5 ,
                             max = 999,
                             step = 1,
                             width = "62px"
                           )
                         ),
                         flowLayout(
                           id = "bbinput2",
                           tags$h6("Number of Standard Deviations", style = "margin-top: 5px;"),
                           numericInput(
                             inputId = "BBsd",
                             label = NULL,
                             value = 2,
                             min = 1,
                             step = 1,
                             max = 999,
                             width = "62px"
                           )
                         )
                       ))
                 ,
                   div(id = "SMAInputs",
                       verticalLayout(
                         tags$h5("SMA Inputs", style = "font-weight: 600;"),
                         flowLayout(
                           id = "SMAinput1",
                           tags$h6("Number of Periods For Fast SMA", style = "margin-top: 5px;"),
                           numericInput(
                             inputId = "smafast",
                             label = NULL,
                             value = 50,
                             min = 5,
                             max = 999,
                             step = 1,
                             width = "62px"
                           )
                         ),
                         flowLayout(
                           id = "SMAinput2",
                           tags$h6("Number of Periods For Slow SMA", style = "margin-top: 5px;"),
                           numericInput(
                             inputId = "smaslow",
                             label = NULL,
                             value = 200,
                             min = 20,
                             max = 999,
                             step = 1,
                             width = "62px"
                           )
                         )
                       )),
                      div(id = "MACDnputs",
                          verticalLayout(
                            tags$h5("MACD Inputs", style = "font-weight: 600;"),
                            flowLayout(
                              id = "macdinput1",
                              tags$h6("Number of periods for fast MA", style = "margin-top: 5px;"),
                              numericInput(
                                inputId = "MACDFastPeroids",
                                label = NULL,
                                value = 12,
                                min = 5 ,
                                max = 999,
                                step = 1,
                                width = "62px"
                              )
                            ),
                            flowLayout(
                              id = "MACDinput2",
                              tags$h6("Number of periods for slow MA", style = "margin-top: 5px;"),
                              numericInput(
                                inputId = "MACDSlowPeroids",
                                label = NULL,
                                value = 26,
                                min = 1,
                                step = 1,
                                max = 999,
                                width = "62px"
                              )
                            ),
                            flowLayout(
                              id = "MACDinput3",
                              tags$h6("Number of periods for signal MA", style = "margin-top: 5px;"),
                              numericInput(
                                inputId = "MACDSignalPeriods",
                                label = NULL,
                                value = 9,
                                min = 1,
                                step = 1,
                                max = 999,
                                width = "62px"
                              )
                            )
                          ))
                      ,
                      div(id = "RSIInputs",
                          verticalLayout(
                            tags$h5("RSI Inputs", style = "font-weight: 600;"),
                            flowLayout(
                              id = "RSIinput1",
                              tags$h6("Number of periods for MA", style = "margin-top: 5px;"),
                              numericInput(
                                inputId = "RSIperiods",
                                label = NULL,
                                value = 14,
                                min = 5,
                                max = 100,
                                step = 1,
                                width = "62px"
                              )
                            ),
                            actionButton(
                              inputId = "btn_UpdateChart1", 
                              label = "Update Chart", 
                              icon = icon("refresh"),
                              style = "width: 100%;"
                            )
                            
                          ))
      ),
      div(id="priceHistoryCharts",
      div(id = "candlestickmain_qm_container", style = "height:585px; background-color:#F0F0F0;",
          dateRangeInput(inputId = "candlestick_qm_daterange",
                         label = NULL,
                         start = MaxDate_Global - years(1),
                         end = MaxDate_Global,
                         max = MaxDate_Global,
                         min = MinDate_Global,
                         format = "MM dd, yyyy"),
          plotOutput(outputId = "candlestickmain_qm", height = "550px")
      ),
      div(id = "candlestickmain_hc_container", style = "background-color: #F0F0F0;",
        highchartOutput(outputId = "candlestickmain_hc", height = "550px")
      )
      ), fluidRow( id = "CandlStickTabletitleRow",
        column(width = 12, wellPanel(id = "CandlStickTabletitlePanel", 
                                     style="box-shadow: none;overflow: hidden;padding: 5px;margin-top: 3px;border-radius: 0;margin-bottom: 0;border-width: 0; border-color: transparent;background-color: #f9f9f9;",
                                     tags$h5("Other Information", style = "margin:3px;float:left"),
                                     actionButton("HideShowCandlStickTable", 
                                                  label = "+", 
                                                  style = "float:right;padding-top: 0px;height: 22px;")
                                     )
               )
      )
      , fluidRow(id = "CandlStickTableRow", class = "slider" , style = "max-height:0px", #"display:none;",
        column(
          width = 6, tableOutput("CandlstickTable")
        ),
        column(
          width = 6,
          div(id="dvdsplts",
              flowLayout(
                verticalLayout(
                  tags$h4("Dividends History", tags$span("(split adjusted)",
                                                         style = "font-size: 10px;font-style: italic;"),
                          style = "margin-left: 15px;font-size: 12px;margin-top: 2px;margin-bottom: 2px;"),
                  verbatimTextOutput("DivHistory")
                ),
                verticalLayout(
                  tags$h4("Splits History",  style = "margin-left: 15px;font-size: 12px;margin-top: 2px;margin-bottom: 2px;"),
                  verbatimTextOutput("SpltHistory")
                )
              )
          )
        )
      )
      ,
      fluidRow( id = "performanceTitleRow",
                column(width = 12, wellPanel(id = "performanceTitlePanel", 
                                             style="box-shadow: none;overflow: hidden;padding: 5px;margin-top: 3px;border-radius: 0;margin-bottom: 0;border-width: 0; border-color: transparent;background-color: #f9f9f9;",
                                             tags$h5("Performance", style = "margin:3px;float:left"),
                                             actionButton("HideShowPerformanceDiv", 
                                                          label = "+", 
                                                          style = "float:right;padding-top: 0px;height: 22px;")
                )
                )
      ),
      div(id= "PriceHistoryPerformanceDiv", class = "slider", style ="max-height:0px;", #"display:none;",
      div(id = "pefromanceSelectionsContainer",style = "margin-top: 5px;background-color: rgba(249, 249, 249, 0.45);padding: 8px 0px 8px 8px;",
          div(id = "pricehistory_perf_selections_Notes", style = "margin-right: 15px;margin-bottom: 8px;",
          tags$span(tags$strong("NOTE 1:", style = "color:red;")," All measures are", tags$strong("NOT Annualized", style = "color:red;"), 
                    "except where specifically indicated"),
          tags$br(),
          tags$span(tags$strong("NOTE 2:", style = "color:red;" ), "For periods where no Returns/Closing price available",tags$strong("Zero Return is assigned", style = "color:red;"))
          ),
      fluidRow(id = "pefromanceSelections1",
        column(
          width = 12,
          flowLayout(
            tags$span(id = "priceHistory_perf_slct_dateRng_title", "Date Range" ),
            dateRangeInput(inputId = "priceHistory_perf_slct_dateRng",
                           label = NULL,
                           start = MaxDate_Global - years(3),
                           end = MaxDate_Global,
                           max = MaxDate_Global,
                           min = MinDate_Global,
                           format = "M dd, yyyy",
                           width = "180px",
                           separator = "-"
            ),
            tags$span(id = "priceHistory_perf_slct_freq_title", "Return Frequency"),
            selectInput(inputId = "priceHistory_perf_slct_freq_inpt",
                        label = NULL,
                        choices = c(
                          Daily = 1,
                          Weekly = 2,
                          Monthly = 3
                        ),
                        selected = 3,
                        multiple = F,
                        width = "65px"
            ),
            tags$span(id = "priceHistory_perf_slct_bnchmk_title", "Benchmark"),
            selectInput(inputId = "priceHistory_perf_slct_bnchmk",
                        label = NULL,
                        choices = c(`S&P500` = "^GSPC",
                                    NASDAQ = "^IXIC",
                                    `Dow Jones IA` = "^DJI",
                                    SPDR = "SPY"),
                        selected = "S&P500",
                        width = "90px"),
                #column(width = 12,
                #flowLayout(
                span("Annual Risk Free rate (0.03 for 3%)", id = "priceHistory_perf_slct_Rf_title" ),
                numericInput(
                  inputId = "priceHistory_perf_slct_Rf",
                  label = NULL,
                  value = 0,
                  step = 0.001,
                  width = "48px",
                  min = 0
                ),
                span("Confidence level", id = "priceHistory_perf_slct_ConfLvl_title" ),
                numericInput(
                  inputId = "priceHistory_perf_slct_ConfLvl",
                  label = NULL,
                  value = 0.95,
                  min = 0.8,
                  max = 1,
                  step = 0.01,
                  width = "44px"
                )
                
              )
            )
          ),
      uiOutput("priceHistory_perf_slct_ErrMsg1"),
      uiOutput("priceHistory_perf_slct_ErrMsg2"),
      uiOutput("priceHistory_perf_slct_ErrMsg3")
      ),
      div(id = "pefromanceResultsContainer",
          fluidRow(
            column(width = 6, style ="padding-right: 0;margin-top: 3px;",
                   tableOutput(outputId = "perfmetricstbl"),
                   div(id = "sharperatio", style = "display:none;",
                       tags$h6("Sharpe Ratio:", style = "font-weight:600;margin-top: 0px;margin-bottom: 3px;"),
                       tableOutput("calculatedsharpetbl")#,
                       #verbatimTextOutput("calculatedsharpe")
                   ),
                   div(id = "TkrDetails_VaR_container",
                       tags$h6("Value At Risk (VaR):", style = "font-weight:600;margin-top: 0px;margin-bottom: 3px;"),
                       tableOutput("tkrDetails_VaR")
                   )
                   ,
                   div(id = "TkrDetails_ES_container",
                       tags$h6("Estimated Shortfall (ES):", style = "font-weight:600;margin-top: 0px;margin-bottom: 3px;"),
                       tableOutput("tkrDetails_ES"),
                       highchartOutput("tkrDetails_VaRsens")
                   )
            )
            ,
            column(
              width = 6, style = "font-size:120%;",
              div(id = "preformanceChart_container", 
                  style = "border-radius:6px;background-color:#F0F0F0;",
                  plotOutput(outputId = "PerformanceChart", height = "410px"),
                  plotOutput(outputId = "PerformanceChartLegend", height = "30px")
              )
              ,
              
              highchartOutput(outputId = "tkrReturnBoxplot", height = "235px"),
              highchartOutput(outputId = "tkrReturnDistributionChart")
              
            )
          )
        )
      ),
      fluidRow( id = "valRatiosTitleRow",
                column(width = 12, wellPanel(id = "valRatiosTitlePanel", 
                                             style="box-shadow: none;overflow: hidden;padding: 5px;margin-top: 3px;border-radius: 0;margin-bottom: 0;border-width: 0; border-color: transparent;background-color: #f9f9f9;",
                                             tags$h5("Valuation Ratios", style = "margin:3px;float:left"),
                                             actionButton("HideShowValRatiosDiv", 
                                                          label = "+", 
                                                          style = "float:right;padding-top: 0px;height: 22px;")
                )
                )
      ), 
      div(id= "valRatiosContainerDiv", class = "slider", style ="max-height:0px",
          fluidRow(style = "margin-top:3px;",
                   column(
                     width = 12,
                     style = "font-size:80%;margin-bottom:10px;",
                     tableOutput("tkrDetails_valutionRatios_tbl"),
                     highchartOutput("tkrDetails_valuationRatios")
                   )
          )
          )

    )
  )
)