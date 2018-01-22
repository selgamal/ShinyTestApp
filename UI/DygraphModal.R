# BS Modal conating dygraphs on clicking any ticker ui ####
bsModal(
  div(
    id = "dycontainer",
    style = "position:relative;z-index:0;",
    HTML('<i id="dyload" class="fa fa-spinner fa-pulse fa-5x"></i>'),
    dygraphOutput("dyintra", height = "300px"),
    dygraphOutput("dytenyears", height = "300px")
  )
  ,
  id = "plotmodal"
  ,
  title = div(
    selectInput(
      inputId = "slctprd"
      ,
      label = NULL
      ,
      choices = c(
        "1 Day",
        "3 Months",
        "6 Months",
        "1 Year",
        "3 Years",
        "5 Years",
        "10 Years"
      )
      ,
      width = "120px"
    ),
    tags$h4(textOutput("charttitle"), style = "text-align:center;float:left;font-weight: 600;color: #83a4bf;margin-left: 5px;")
    ,
    div(id = "dylabelesDiv", style = "float: right; margin-right: 10px;font-size: 70%;margin-top: 10px;background-color:lightgray;")
    ,
    id = "dymodaltilecontents"
  )
  ,
  trigger = ""
  ,
  size = "s"
)