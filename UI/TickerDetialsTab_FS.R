# The UI for div showing financial statements and ratios ####

tags$div(id = "tkr_fs_ratios", style = "display:none;",
  fluidRow(column(
    width = 12,
    tags$div(
      id = "slctrs",
      tags$h5("Select Financial Statements Period",
              style = "float: left;margin-top: 10px;margin-right: 6px;" ),
      selectInput(
        inputId = "Period",
        label = NULL,
        choices = c("Annual", "Quarter"),
        selected = "Annual",
        width = "100px"
      )
    )
  )),
  
  fluidRow(
    style = "font-size:80%;",
    column(
      width = 6, style = "padding-right: 0px;",
      flowLayout(class = "fstitles",
        tags$h4("Balance Sheet Captions"),
        tags$h6(textOutput("bs_currency"), style = "margin-top: 15px;text-indent: 3px;font-style: italic;")
      ),
      tableOutput(outputId = "BS")
    ),
    column(
      width = 6,
      tags$h4("Liabilities, Equity and Debt % of Total Assets"),
      highchartOutput(
        outputId = "BSplot1",
        width = "100%",
        height = "256px"
      )
    )
    
  ),
  fluidRow(
    style = "font-size:80%;",
    column(
      width = 6 , style = "padding-right: 0px;",
      tags$h4("Liquidity and Leverage"),
      highchartOutput(
        outputId = "BSplot2",
        width = "100%",
        height = "436px"
      )
    ),
    column(
      width = 6,
      tags$h4("Efficiency Ratios"),
      checkboxGroupInput(
        inputId = "ratio",
        label = NULL
        ,
        choices = c(
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
        inline = T
        ,
        width = "100%"
        ,
        selected = c("Days Sales Outstanding", "Days Inventory" , "Payables Period")
        
      ),
      highchartOutput(
        outputId = "BSplot3",
        width = "100%",
        height = "400px"
      )
    )
  ),
  #tags$hr(),
  fluidRow(
    style = "font-size:80%;", 
    column(
      width = 6, style = "padding-right: 0px;",
      flowLayout(class = "fstitles",
      tags$h4("Income Statement Captions "),
              tags$h6(textOutput("is_currency"), style = "margin-top: 15px;text-indent: 3px;font-style: italic;") 
             ),
      tableOutput(outputId = "IS")
    ),
    column(
      width = 6,
      tags$h4("Total Revenue & Net Income"),
      highchartOutput(
        outputId = "ISplot1",
        width = "100%",
        height = "100%"
      )
    )
  ),
  fluidRow(
    style = "font-size:80%;",
    column(
      width = 6 , style = "padding-right: 0px;",
      tags$h4("Profitability"),
      highchartOutput(
        outputId = "ISplot2",
        width = "100%",
        height = "400px"
      )
    ),
    column(
      width = 6,
      tags$h4("Percentage of Total Revenue"),
      highchartOutput(
        outputId = "ISplot3",
        width = "100%",
        height = "400px"
      )
    )
  ),
  fluidRow(
    style = "font-size:80%;",
    column( 
      width = 6, style = "padding-right: 0px;",
      flowLayout(class = "fstitles",
      tags$h4("Cash Flows Captions "),
              tags$h6(textOutput("cf_currency"), style = "margin-top: 15px;text-indent: 3px;font-style: italic;")
      ),
      tableOutput(outputId = "CF"),
      tags$h4("Free Cash Flows"),
      highchartOutput(
        outputId = "CFplot2",
        width = "100%",
        height = "268px"
      )
    ),
    column(
      width = 6,
      tags$h4("Cash Flows"),
      highchartOutput(
        outputId = "CFplot1",
        width = "100%",
        height = "500px"
      )
    )
  )
)