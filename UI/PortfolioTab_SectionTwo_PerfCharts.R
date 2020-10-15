## UI for port portfolio performance -- excatly same as in price history tab

fluidRow(
  id = "port_SectionTwo_ChartsRowTwo",
  style = "display:none;",
  div(
    id = "port_SectionTwo_pefromanceSelectionsContainer",
    style = "margin-top: 5px;background-color: rgba(249, 249, 249, 0.45);padding: 8px 0px 8px 8px;margin: 3px 15px 0 15px;",
    fluidRow(
      id = "port_SectionTwo_pefromanceSelections1",
      div(
        id = "port_SectionTwo_WhichRets",
        style = "margin-left: 15px;margin-right: 15px;margin-bottom: 8px;",
        tags$span(
          tags$strong("NOTE 1:", style = "color:red;"),
          " Recommended to use same date range and returns frequency used for optimizing the portfolio, different date ranges and return frequencies will yield inconsistent results"
        ),
        tags$br(),
        tags$span(
          tags$strong("NOTE 2:", style = "color:red;"),
          " All measures are",
          tags$strong("NOT Annualized", style = "color:red;"),
          "except where specifically indicated"
        ),
        tags$br(),
        tags$span(
          tags$strong("NOTE 3:", style = "color:red;"),
          "For periods where no Returns/Closing price available",
          tags$strong("Zero Return is assigned", style = "color:red;")
        )
      ),
      column(
        width = 12,
        flowLayout(
          tags$span(id = "port_SectionTwo_perf_slct_dateRng_title", "Date Range"),
          dateRangeInput(
            inputId = "port_SectionTwo_perf_slct_dateRng",
            label = NULL,
            start = MaxDate_Global - years(3),
            end = MaxDate_Global,
            max = MaxDate_Global,
            min = MinDate_Global,
            format = "M dd, yyyy",
            width = "180px",
            separator = "-"
          ),
          tags$span(id = "port_SectionTwo_perf_slct_freq_title", "Return Frequency"),
          selectInput(
            inputId = "port_SectionTwo_perf_slct_freq_inpt",
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
          tags$span(id = "port_SectionTwo_perf_slct_bnchmk_title", "Benchmark"),
          selectInput(
            inputId = "port_SectionTwo_perf_slct_bnchmk",
            label = NULL,
            choices = c(
              `S&P500` = "^GSPC",
              NASDAQ = "^IXIC",
              `Dow Jones IA` = "^DJI",
              SPDR = "SPY"
            ),
            selected = "S&P500",
            width = "90px"
          ),
          span("Annual Risk Free rate (0.03 for 3%)", id = "port_SectionTwo_perf_slct_Rf_title"),
          numericInput(
            inputId = "port_SectionTwo_perf_slct_Rf",
            label = NULL,
            value = 0,
            step = 0.001,
            width = "48px",
            min = 0
          ),
          span("Confidence level", id = "port_SectionTwo_perf_slct_ConfLvl_title"),
          numericInput(
            inputId = "port_SectionTwo_perf_slct_ConfLvl",
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
    uiOutput("port_SectionTwo_perf_slct_ErrMsg1"),
    uiOutput("port_SectionTwo_perf_slct_ErrMsg2"),
    uiOutput("port_SectionTwo_perf_slct_ErrMsg3")
  ),
  div(id = "port_SectionTwo_pefromanceResultsContainer", style = "margin: 3px 15px 0 15px;",
      fluidRow(
        column(
          width = 6,
          style = "padding-right: 2px;",
          tableOutput(outputId = "port_SectionTwo_perfmetricstbl"),
          div(
            id = "port_SectionTwo_VaRsens_container",
            style = "background-color: #F0F0F0;border-radius: 6px;margin-top: 3px;",
            tags$h4(textOutput("port_sectionTwo_VaRsensttl"),
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"),
            tags$h6(textOutput("port_sectionTwo_VaRsensSubttl"),
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"),
            radioButtons(
              inputId = "port_sectionTwo_VaRsens_slct",
              label = NULL,
              choices = c(
                "Optimal Portfolio" = 1,
                "Minimum Risk Portfolio" = 2,
                "Equal Weights Portfolio" = 3
              ),
              selected = 1,
              inline = T
            ),
            highchartOutput("port_SectionTwo_VaRsens")
          )
        )
        ,
        column(
          width = 6,
          style = "font-size:120%;padding-left: 2px;",
          div(
            id = "port_SectionTwo_preformanceChart_container",
            tags$h4(textOutput("port_sectionTwo_PerfSumttl"),
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"),
            tags$h6(textOutput("port_sectionTwo_PerfSumSubttl"),
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"),
            checkboxGroupInput(
              inputId = "port_sectionTwo_PerfSum_slct",
              label = NULL,
              choices = c(
                "Optimal Portfolio" = 1,
                "Minimum Risk Portfolio" = 2,
                "Equal Weights Portfolio" = 3
              ),
              selected = 1,
              inline = T
            ),
            style = "border-radius:6px;border: 1px solid #F0F0F0;",
            plotOutput(outputId = "port_SectionTwo_PerformanceChart", height = "410px"),
            plotOutput(outputId = "port_SectionTwo_PerformanceChartLegend", height = "30px")
          )
          ,
          div(
            id = "port_SectionTwo_BoxChart_container",
            style = "background-color: #F0F0F0;border-radius: 6px;margin-top: 3px;",
            tags$h4(textOutput("port_sectionTwo_BoxCharttl"),
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"),
            tags$h6(textOutput("port_sectionTwo_BoxChartSubttl"),
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"),
            checkboxGroupInput(
              inputId = "port_sectionTwo_BoxChart_slct",
              label = NULL,
              choices = c(
                "Optimal Portfolio" = 1,
                "Minimum Risk Portfolio" = 2,
                "Equal Weights Portfolio" = 3
              ),
              selected = 1,
              inline = T
            ),
            highchartOutput(outputId = "port_SectionTwo_ReturnBoxplot", height = "235px")
          ),
          div(
            id = "port_SectionTwo_RetDist_container",
            style = "background-color: #F0F0F0;border-radius: 6px;margin-top: 3px;",
            tags$h4(textOutput("port_sectionTwo_RetDistttl"),
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"),
            tags$h6(textOutput("port_sectionTwo_RetDistSubttl"),
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"),
            radioButtons(
              inputId = "port_sectionTwo_RetDist_slct",
              label = NULL,
              choices = c(
                "Optimal Portfolio" = 1,
                "Minimum Risk Portfolio" = 2,
                "Equal Weights Portfolio" = 3
              ),
              selected = 1,
              inline = T
            ),
            highchartOutput(outputId = "port_SectionTwo_ReturnDistributionChart")
          )
        )
      ))
  
)
