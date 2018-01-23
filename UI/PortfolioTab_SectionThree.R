#### UI for portfolio tab section three

div(
  id = "PortsectionThree_div",
  class = "slider",
  style = "max-height:0px;",
  wellPanel(
    id = "PortsectionTwo_container",
    style = "background-color: white;
    border: 1px solid lightgray;
    padding: 8px;
    font-size:80%;",
    flowLayout(
      id = "port_sectionThree_compchartslctrs",
      selectizeInput(
        inputId = "port_sectionThree_slct_ComparePortSlct",
        label = "Select Portfolios to compare (from saved in previous section)",
        choices = NULL, #c("ASDF","zxcv","werty","cvnbm","tyuih","ukdfg","ryhdxc","dgggds"),#
        selected = NULL,
        multiple = T,
        options = list(
          placeholder = "Select portfolios to compare",
          maxItems = SelectizeMaxSelection
        ),
        width = "450px"
      ),
      verticalLayout(
        flowLayout(
          dateRangeInput(
            inputId = "port_sectionThree_daterange",
            label = "Returns Date Range",
            start = MaxDate_Global - years(3),
            end = MaxDate_Global,
            max = as.Date(ceiling_date(ymd(MaxDate_Global), unit = "month") -
                            days(1)),
            min = MinDate_Global,
            format = "M dd, yyyy",
            width = "200px"
          ),
          selectInput(
            inputId = "port_sectionThree_slct_retfreq",
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
            inputId = "port_sectionThree_slct_benchmark_port",
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
          inputId = "port_sectionThree_btn_updatePortCompareChart",
          label = "Update Charts",
          icon = icon("refresh"),
          width = "218px"
        )
      )
    ),
    uiOutput(outputId = "port_SectionThree_daterange_ErrMsg"),
    div(
      id = "port_sectionThree_Contents",
      style = "display: none;",
      fluidRow(
        id = "port_sectionThree_RowOne",
        column(
          id = "port_sectionThree_RowOne_colOne",
          width = 6,
          style = "padding-right: 2px;",
          div(
            id = "port_SectionThree_ChartsRowOneColOne_PerfSum_title",
            style = "margin-top: 3px;background-color: #F0F0F0;border-radius: 6px;",
            tags$h4("Performance Summary for $100",
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto; font-size: 19px;"),
            div(
              id = "port_SectionThree_ChartsRowOneColOne_PerfSum_subtitle",
              style = "text-align: center;height: 16px;margin: 6px 0 6px 0;",
              tags$h6(
                textOutput(outputId = "port_sectionThree_RowOne_colOne_subtitle", inline = T),
                style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"
              )
            ),
            highchartOutput("port_SectionThree_PerfSum")
          )
        ),
        column(
          id = "port_sectionThree_RowOne_colTwo",
          width = 6,
          style = "padding-left: 2px;",
          div(
            id = "port_SectionThree_ChartsRowOneColTwo_rollingPerf_title",
            style = "margin-top: 3px;background-color: #F0F0F0;border-radius: 6px;",
            tags$h4("12 Months Rolling Performance", 
                    tags$span("(ANNUALIZED)", style = "color:red;font-size: 12px;font-weight: bold;"),
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto; font-size: 19px;"),
            div(
              id = "port_SectionThree_ChartsRowOneColTwo_rollingPerf_subtitle",
              style = "text-align: center;height: 16px;margin: 6px 0 6px 0;",
              tags$h6(
                textOutput(outputId = "port_sectionThree_RowOne_colTwo_rollingPerf_subtitle", inline = T),
                style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"
              )
            ),
            highchartOutput("port_SectionThree_rollingPerf")
          )
        )
      ),
      fluidRow(
        id = "port_sectionThree_RowTwo",
        column(
          id = "port_sectionThree_RowTwo_colOne",
          width = 6,
          style = "padding-right: 2px;",
          div(
            id = "port_SectionThree_tbl",
            style = "font-size:85%;background-color: rgba(240, 240, 240, 0.3);height: 485px;border-radius: 0 0 6px 6px;",
            dataTableOutput("port_sectionThree_metrics_tbl")
          ),
          div(id = "port_SectionThree_filler")
        ),
        column(
          id = "port_sectionThree_RowTwo_colTwo",
          width = 6,
          style = "padding-left: 2px;",
          div(
            id = "port_SectionThree_ChartsRowOneColTwo_relativePerf_title",
            style = "margin-top: 3px;background-color: #F0F0F0;border-radius: 6px;",
            tags$h4("Relative Performance",
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto; font-size: 19px;"),
            div(
              id = "port_SectionThree_ChartsRowTwoColTwo_relativePerf_subtitle",
              style = "text-align: center;margin: 6px 0 6px 0;",
              tags$h6(
                textOutput(outputId = "port_sectionThree_RowTwo_colTwo_relativePerf_subtitle", inline = T),
                style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"
              ),
              flowLayout(
                tags$label("Select Portfolio to Compare performance against:", style = "margin-bottom: 0;margin-top: 5px;"),
                selectInput(
                  inputId = "port_sectionThree_RowTwo_colTwo_relativePerf_slctr",
                  label = NULL,
                  choices = NULL,
                  multiple = F,
                  width = "90px"
                )
              )
            ),
            highchartOutput("port_SectionThree_relativePerf")
          )
        )
      ),
      fluidRow(
        id = "port_sectionThree_RowThree",
        column(
          id = "port_sectionThree_RowThree_colOne",
          width = 6,
          style = "padding-right: 2px;",
          div(
            id = "port_SectionThree_ChartsRowThreeColOne_riskVRet_title",
            style = "margin-top: 3px;background-color: #F0F0F0;border-radius: 6px;",
            tags$h4("Portfolio Risk Vs Returns",
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto; font-size: 19px;"),
            div(
              id = "port_SectionThree_ChartsRowThreeColOne_riskVRet_subtitle",
              style = "text-align: center;height: 16px;margin: 6px 0 6px 0;",
              tags$h6(
                textOutput(outputId = "port_sectionThree_RowThree_colOne_riskVRet_subtitle", inline = T),
                style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"
              )
            ),
            highchartOutput("port_SectionThree_riskVRet")
          )
        ),
        column(
          id = "port_sectionThree_RowThree_colTwo",
          width = 6,
          style = "padding-left: 2px;",
          div(
            id = "port_SectionThree_ChartsRowThreeColTwo_boxPlot_title",
            style = "margin-top: 3px;background-color: #F0F0F0;border-radius: 6px;",
            tags$h4("Portfolios Returns Box Plot",
                    style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto; font-size: 19px;"),
            div(
              id = "port_SectionThree_ChartsRowThreeColTwo_boxPlot_subtitle",
              style = "text-align: center;height: 16px;margin: 6px 0 6px 0;",
              tags$h6(
                textOutput(outputId = "port_sectionThree_RowThree_colTwo_boxplot_subtitle", inline = T),
                style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto;"
              )
            ),
            highchartOutput("port_SectionThree_boxPlot")
          )
        )
      )
    )
      
      
      
      
      
      
      
      
  )
)