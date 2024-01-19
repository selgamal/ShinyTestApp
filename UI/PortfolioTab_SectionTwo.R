div(
  id = "PortsectionTwo_div",
  class = "slider",
  style = "max-height:0px;",
  wellPanel(
    id = "PortsectionTwo_container",
    style = "background-color: white;
    border: 1px solid lightgray;
    padding: 8px;
    font-size:80%;
    margin-bottom: 3px;",
    flowLayout(
      id = "portspecstitle",
      style = "margin-bottom: 8px;",
      tags$h5("Setup Portfolio Specs:", style = "margin: 2px 0 3px 0;"),
      actionButton(
        inputId = "btn_PortSpecfill",
        label = tagList(icon("arrow-down"), "fill from above"),
        style = "background-color: #efefef;font-size: 100%;padding: 0 4px 0 4px;height: 20px;font-style: italic;border: none;"
      ),
      div(style = "margin-bottom: 2px;",
          span(
            style = "font-style:italic;",
            span("(Note:", style = "font-weight:bold;"),
            span(
              "Recommended to try first optimization with default specs and tune it)"
            )
          ))
    ),
    fluidRow(
      id = "port_sectionTwo_specs_rowOne",
      column(
        id = "port_sectionTwo_specs_rowOneColOne",
        width = 5,
        selectizeInput(
          inputId = "port_sectionTwo_Optim_slctTkrs",
          label = HTML(
            "<span>Included Tickers <span style=\"font-weight: normal;font-style: italic;\">(Select up to 10 from tickers selected above)</span></span>"
          ),
          choices = NULL,
          selected = NULL,
          multiple = T,
          options = list(
            placeholder = paste('Up to', SelectizeMaxSelection, 'Ticker Symbols'),
            maxItems = SelectizeMaxSelection
          )
        )
      ),
      column(
        id = "port_sectionTwo_specs_rowOneColOne",
        width = 7,
        flowLayout(
          id = "PortOptim_Slctrs",
          dateRangeInput(
            inputId = "port_sectionTwo_Optim_daterange",
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
            inputId = "port_sectionTwo_Optim_retfreq",
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
          verticalLayout(
            flowLayout(
              tags$label("Set Max/Min weights allowed for each asset"),
              checkboxInput(
                "port_sectionTwo_Optim_UseMinMax",
                label = NULL,
                value = F
              )
            ),
            flowLayout(
              tags$label("Max weight:"),
              numericInput(
                inputId = "port_sectionTwo_Optim_MaxWt",
                label = NULL,
                max = 1,
                value = NULL,
                min = 0,
                step = 0.01,
                width = "50px"
              ),
              tags$label("Min weight:"),
              numericInput(
                inputId = "port_sectionTwo_Optim_MinWt",
                label = NULL,
                max = 1,
                value = NULL,
                min = -1,
                step = 0.01,
                width = "50px"
              )
            ),
            tags$span(
              id = "portMaxMinContstraintText",
              style = "cursor: pointer;font-size: 10px;font-style: italic;",
              HTML("<i class=\"fa fa-info-circle\" id=\"port_wt_info\"></i>"),
              "Weights higher/lower bounds constraints"
            ),
            bsTooltip(
              "portMaxMinContstraintText",
              title = "If Min/Max bounds are set then following conditions must be met:<br> -Min and Max must be a value between 0 and 1;<br> -Min < Max; <br> -Max * # of assets must be > 1;<br> -Min * # of assets must be < 1.",
              placement = "bottom",
              trigger = "click",
              options = NULL
            )
          ),
          tags$span(
            id = "port_setionTwo_ZeroWtNote",
            style = "font-size: 10px;font-style: italic;",
            HTML("<i class=\"fa fa-info-circle\"></i>"),
            "To disable Min OR Max weight, leave blank, if 0 is entered it will be considered as a weight constraint."
          )
        )
      )
    ),
    fluidRow(
      id = "port_sectionTwo_specs_rowTwo",
      column(
        id = "port_sectionTwo_specs_rowTwoColone",
        width = 4,
        flowLayout(
          tags$label(id = "port_Optim_Rf_title", "Annual Risk Free rate (0.03 for 3%)"),
          numericInput(
            inputId = "port_sectionTwo_Optim_slct_Rf",
            label = NULL,
            value = 0,
            step = 0.001,
            min = 0,
            width = "54px"
          )
        ),
        uiOutput(outputId = "port_setionTwo_RfNote")
        ,
        actionButton(
          "port_sectionTwo_OptimBtn",
          label = "Get Optimal Portfolio",
          width = "100%"
        )
      ),
      column(
        id = "port_sectionTwo_specs_rowTwoColTwo",
        width = 8,
        verbatimTextOutput("port_sectionTwo_OptimResultMsg", placeholder = T)
    )
),
fluidRow(
  id = "port_sectionTwo_specs_rowTwo_cont",
  column(
    id = "port_sectionTwo_specs_rowTwo_Cont_ColOne",
    width = 12,
    tags$h6(
      "Save up to 10 portfolios to individually display below, or compare in the next section (",
      textOutput(outputId = "port_sectionTwo_NumSaved", inline = T),
      " of 10 slots used)" , tags$a(id = "port_sectionTwo_save_help",icon("question-circle"), href = "#", style = "color:black"),
      bsTooltip(
        "port_sectionTwo_save_help",
        title = "To store the currently \"Unsaved\" portfolio, select \"Save\" from the first dropdown list, and make sure that \"Unsaved\" is selected in the second dropdown list, then enter a name for the portfolio in the last input space and click save (each portfolio saved should have a unique name).<br><br>To delete a saved portfolio, select \"Delete\" from the first dropdown list, and from the second dropdown list, select the portfolio to delete and click Delete.",
        placement = "bottom",
        trigger = "click",
        options = NULL
      ),
      style = "margin-top:5px;font-weight: 700;margin-bottom:3px"
    ),
    wellPanel(
      id = "port_sectionTwo_specs_saveResults",
      style = "background-color: rgb(245, 245, 245);
      border: 0;
      margin-bottom: 5px;
      min-height: 60px;
      font-size: 13px;
      padding: 7px 7px 0px 7px;
      box-shadow: 5px 5px 5px #888888;
      ",
        flowLayout(
          tags$label("Select Action:", `for` = "port_sectionTwo_selectAction", style = "font-weight: 600;font-size: 12px;"),
          selectInput(
            inputId = "port_sectionTwo_selectAction",
            label = NULL,
            choices = list(`Save` = 1,
                           `Delete` = 2),
            selected = 1,
            multiple = F,
            width = "105px"
          ),
          selectizeInput(
            inputId = "port_sectionTwo_specs_selectPort",
            label = NULL,
            choices = NULL,
            selected = NULL,
            multiple = F,
            width = "180px",
            options = list(
              maxOptions = 10,
              create = F,
              placeholder = "Portfolio to apply action"
            )
          ),
          textInput(
            inputId = "port_sectionTwo_specs_enterName",
            label = NULL,
            value = NULL,
            placeholder = "Enter Name",
            width = "160px"
          ),
          actionButton(
            inputId = "port_sectionTwo_specs_doAction",
            label = "Save",
            icon = icon("floppy-o", verify_fa=FALSE)
          )
          
        ),
        uiOutput(outputId = "port_sectionTwo_specs_saveBoxHelp"),
        uiOutput(outputId = "port_sectionTwo_specs_saveBoxHelpErr", inline = T)
    )
  )
),
fluidRow(
  id = "port_sectionTwo_specs_rowThree",
  column(
    id = "port_sectionTwo_specs_rowThreeColOne",
    width = 4,
    selectInput(
      inputId = "port_sectionTwo_specs_slctSaved",
      label = "Select Saved Portfolio To Display",
      choices = NULL,
      selected = NULL,
      multiple = F
    ),
    tags$span(
      icon("info-circle"),
      style = "font-style: italic;",
      "Displays Currently Optimized portfolio by default"
    )
  ),
  column(
    id = "port_sectionTwo_specs_rowThreeColTwo",
    width = 8,
    verbatimTextOutput("port_sectionTwo_OptimStat", placeholder = T)
  )
),
fluidRow(
  id = "port_SectionTwo_ChartsRowOne",
  style = "display:none;",
  column(
    id = "port_SectionTwo_ChartsRowOneColOne_Eff",
    style = "padding-right: 2px;",
    width = 6,
    div(
      style = "margin-top: 3px;background-color: #F0F0F0;border-radius: 6px;",
      tags$h4("Efficient Frontier",
              style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto; font-size: 19px;"),
      div(
        id = "port_SectionTwo_ChartsRowOneColTwo_EfFront_subtitle",
        style = "text-align: center;height: 16px;margin: 6px 0 6px 0;",
        tags$span(
          "*Optimal Portfolio calculated based on Max. Sharpe Ratio - Std.Dev as Risk"
        )
      ),
      highchartOutput("port_SectionTwo_Eff")
    )
  ),
  column(
    id = "port_SectionTwo_ChartsRowOneColTwo_pie",
    style = "padding-left: 2px;",
    width = 6,
    div(
      style = "margin-top: 3px;background-color: #F0F0F0;border-radius: 6px;",
      tags$h4("Portfolio Allocation",
              style = "text-align: center;margin: 0;padding-top: 5px;font-family: Roboto; font-size: 19px;"),
      radioButtons(
        inputId = "port_sectionTwo_pie_slct",
        label = "",
        choices = c(
          "Optimal Portfolio" = 1,
          "Min. Risk Portfolio" = 2
        ),
        selected = 1,
        inline = T
      ),
      
      highchartOutput("port_SectionTwo_Pie")
    )
  )
),
source("UI/PortfolioTab_SectionTwo_PerfCharts.R", local = T)$value
)
)