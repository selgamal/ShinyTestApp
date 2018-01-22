#### BS modal conatining Containing companies list on clicking list icon in sidebar ####
bsModal(
  div(
    DT::dataTableOutput(outputId = "CompaniesList", width = "100%", height = "350px")
    ,
    style = "position:relative;font-size:70%; width:100%; padding-top:0px;z-index=1;"
  )
  ,
  id = "Companiestable"
  ,
  title = tagList(
    div(
      tags$h3("Available Companies", style = "margin-bottom:0;"), 
      tags$span("Available Price History range is from", 
                format(as.Date(MinDate_Global),"%b %d, %Y"),
                " to ", 
                format(as.Date(MaxDate_Global),"%b %d, %Y")
                , style = "font-size: 12px;font-style: italic;")
    )
    ,
    tags$h4("Click companies then \"+\" to add to report"),
    verbatimTextOutput(outputId = "selectedCompanies", placeholder = T),
    actionButton(
      "addComp",
      label = "+",
      #icon = icon(name = "plus-square"),
      style = "float:left;margin-left:10px;font-size:18px; padding:0;width:8%;height: 65px;"
    )
  )
  ,
  trigger =""
  ,
  size = "l"
)