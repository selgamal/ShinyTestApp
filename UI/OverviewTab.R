# Overview Tab #####

tabPanel(value = 1,
  "Overview",
  fluidRow(
    valueBoxOutput(outputId = "vbx_dji"),
    valueBoxOutput(outputId = "vbx_sp"),
    valueBoxOutput(outputId = "vbx_nasdaq")
  ),
  fluidRow(
    column(
      width = 8,
      wellPanel(
        id = "bblzContainer",
        style = "width:100%;float:left;border: 0;box-shadow: none;"
        ,
        h3("Last trade by change % and bid size")
        ,
        actionButton(
          inputId = "seeDetails",
          label = " More Info",
          icon = icon("info-circle"),
          style = "float:left"
        )
        ,
        div(
          style = "padding:41px 0px 0px 0px;",
          id = "bblzContainer2",
          bubblesOutput(
            outputId = "bublz",
            width = "100%",
            height = "400px"
          )
          ,
          plotOutput("barplot", width = "40%", height = "70px")
        )
      )
    )
    ,
    column(
      width = 4,
      id = "GnrLsrCol",
      div(
        id = "Gnrlsr",
        style = "float:left;width: 100%;",
        fluidRow(
          h4("Session Highest Gain", style = "border-radius: 3px;text-align: center;background-color: #f5f5f5;margin-left: 15px;margin-right: 15px;") ,
          valueBoxOutput(outputId = "vbx_tkrgnr")
        )
        ,
        fluidRow(
          h4("Session Highest Loss", style = "border-radius: 3px;text-align: center;background-color: #f5f5f5;margin-left: 15px;margin-right: 15px;") ,
          valueBoxOutput(outputId = "vbx_tkrlosr")
        ),
        fluidRow(
          h4("Current Prices", style = "border-radius: 3px;text-align: center;background-color: #f5f5f5;margin-left: 15px;margin-right: 15px;") ,
          wellPanel(uiOutput("tkr_stats"), id = "currentprices",style = "box-shadow: none;margin-left: 15px;margin-right: 15px;padding:3px;display:flex;background:white;border: 1px solid lightgray;")
        )
      )
    )
  ), div(id = "height_tab1", style = "height:21px;")
)