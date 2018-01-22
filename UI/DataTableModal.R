#### BS modal conatining Data Table on click of "Details" button in the overview tab ####
bsModal(
  div(
    DT::dataTableOutput(outputId = "rttable", width = "100%")
    ,
    style = "position:relative;font-size:70%; width:100%; padding-top:0px;z-index=1;"
  )
  ,
  id = "detailstable"
  ,
  title = tagList(
    tags$h3("Some Details", style = "float:left")
    ,
    actionButton(
      "refreshTable",
      label = "Refresh",
      icon = icon(name = "refresh"),
      style = "float:left;margin-left:10px;margin-top:15px;"
    )
  )
  ,
  trigger = "seeDetails"
  ,
  size = "l"
)