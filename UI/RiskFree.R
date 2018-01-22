# Risk Free Rates tab ####

tabPanel(value = 4,
         "US Treasury Yield Curve",
         fluidRow(
           column(width = 12, 
                  id = "YC_TabTitle_container", 
                  style = "font-size:65%;padding-right: 2px;padding-left: 2px;",
                  tags$h3("US Treasury Yield Curve Rates",
                  tags$span("(Based on Data from",
                            tags$a("US Treasury Site", target="_blank",
                                   href = "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldAll" 
                                   ),")"
                            , style = "font-size: 12px;font-style: italic;")
                  )
           )
         ),
         fluidRow(
           column(width = 6, 
                  id = "YC_dtatbl_container", 
                  style = "font-size:65%;padding-right: 2px;padding-left: 2px;",
                  dataTableOutput("MonthlyYC")
                  ),
           column(
             width = 6,
             id="YC_Charts_Container", style = "padding-right: 2px;padding-left: 2px;",
             div(id = "YC_Chart_Container", style = "background-color: #F0F0F0;",
                 tags$h4("Yield Curve Rates Over Time", style = "padding-top: 5px;padding-left: 5px;margin-top: 0px;"),
                 tags$h6("Select dates to add to chart",style = "padding-left: 5px;"),
                 flowLayout(style = "padding-left: 5px;",
                  dateInput(inputId = "slct_YCdates1", 
                            label = "",
                            min = NULL, #min(YC_melt$Date),
                            max = NULL, #max(YC_melt$Date),
                            value = NULL, #max(YC_melt$Date),
                            format = "M dd, yyyy",
                            width = "100px"),
                  dateInput(inputId = "slct_YCdates2", 
                            label = "",
                            min = NULL, #min(YC_melt$Date),
                            max = NULL, #max(YC_melt$Date),
                            value = NULL,  #min(YC_melt$Date),
                            format = "M dd, yyyy",
                            width = "100px"),
                  dateInput(inputId = "slct_YCdates3", 
                            label = "",
                            min = NULL, #min(YC_melt$Date),
                            max = NULL, #max(YC_melt$Date),
                            value = NULL, #unique(YC_melt$Date)[ceiling(nrow(YCHistory)/2)],
                            format = "M dd, yyyy",
                            width = "100px")
                 )
                 ,div(
                   id = "hc_yc", style = "background-color: #F0F0F0;",
                   verbatimTextOutput("YC2"),
                   highchartOutput("hc_YieldCurve", height = "300px")
                 )
             ),
             div(id = "YC_Chart_Container", style = "background-color: #F0F0F0",
                 tags$h4("Rates History (Monthly)",
                         style = "padding-top: 5px;padding-left: 5px;margin-top: 3px;"),
                 checkboxGroupInput(inputId = "slct_YCterms",
                                    label = "Select Maturities to add to Chart",
                                    choices = NULL, #unique(YC_monthly.df$variable),
                                    selected = NULL, # unique(YC_monthly.df$variable)[1:2],
                                    inline = T
                 ),
                 highchartOutput("hc_YieldCurveTerms")
             )
             )
           ), div(id = "height_tab2", style = "height:30px;")
         )
