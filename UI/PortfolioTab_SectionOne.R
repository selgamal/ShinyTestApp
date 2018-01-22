div(
  id = "PortsectionOne_div",
  class = "slider",
  style = "max-height:0px;",
  wellPanel(
    id = "PortsectionOne_container",
    style = "background-color: white;
    border: 1px solid lightgray;
    padding: 8px;
    font-size:80%;",
    flowLayout(
      id = "compchartslctr",
      selectizeInput(
        inputId = "slct_CompareTkrSlct",
        label = "Select Tickers To Compare",
        choices = NULL,
        selected = NULL,
        multiple = T,
        options = list(
          placeholder = paste('Up to', SelectizeMaxSelection, 'Ticker Symbols'),
          maxItems = SelectizeMaxSelection
        ),
        #10
        width = "450px"
      ),
      verticalLayout(
        flowLayout(
          selectInput(
            inputId = "slct_retfreq",
            label = "Return Freq.",
            choices = c(
              Daily = 1,
              Weekly = 2,
              Monthly = 3
            ),
            selected = 3,
            multiple = F,
            width = "82px"
          ),
          selectInput(
            inputId = "slct_benchmark_port",
            label = "Benchmark",
            choices = c(
              `S&P500` = "^GSPC",
              NASDAQ = "^IXIC",
              `Dow Jones IA` = "^DJI",
              SPDR = "SPY"
            ),
            selected = "S&P500",
            width = "120px"
          )
        ),
        actionButton(
          inputId = "btn_updateCompareChart",
          label = "Update Chart",
          icon = icon("refresh"),
          width = "218px"
        )
        
      )
    ),
    div(
      id = "port_sectionOne_Contents",
      fluidRow(id = "port_sectionOne_rowOne",
               column(
                 width = 12,
                 radioButtons(
                   inputId = "slct_chartpkg_port",
                   label = "Select Chart Package",
                   choices = c(
                     "PA package Chart (Best Performance)" = 1,
                     "HighCharts (Best Looks and options - takes longer to load)" =
                       2
                   ),
                   selected = 1,
                   inline = T,
                   width = "100%"
                 ),
                 div(id = "div_comapreTkrs_hc",
                     highchartOutput("compareTkrsChart_hc", height = "430px")),
                 div(
                   id = "div_comapreTkrs_pa",
                   dateRangeInput(
                     inputId = "comapreTkrs_pa_daterange",
                     label = NULL,
                     start = MaxDate_Global - years(3),
                     end = MaxDate_Global,
                     max = as.Date(ceiling_date(ymd(MaxDate_Global), unit = "month") -
                                     days(1)),
                     min = MinDate_Global,
                     format = "MM dd, yyyy"
                   ),
                   plotOutput("comapreTkrs_pa"),
                   plotOutput("compareTkrs_pa_lgnd", height = "30px")
                 )
               )),
      fluidRow(
        id = "port_sectionOne_rowTwo",
        column(width = 6,
               highchartOutput("PortCorrChart")),
        column(width = 6,
               highchartOutput("PortBoxChart"))
      ),
      fluidRow(
        id = "port_sectionOne_rowThree",
        style = "margin-top: 3px;",
        column(
          width = 6,
          style = "padding-right: 2px;",
          div(
            id = "port_SectionOne_tbl",
            style = "font-size:85%;background-color: rgba(240, 240, 240, 0.3);height: 400px;border-radius: 0 0 6px 6px;",
            dataTableOutput("port_metrics_tbl")
          ),
          div(id = "filler")
        ),
        column(
          width = 6,
          style = "padding-left: 2px;",
          highchartOutput("port_RiskVReturns_secOne")
        )
      )
    )
    
  )
  )
