### Side Bar #####
wellPanel(
  id = "sidebarpan",
  width = 1
  
  ,
  div(id = "hidesidebar", icon("caret-square-o-left", verify_fa=FALSE)),
  selectizeInput(
    label = tagList(tags$label("Select Ticker Symbols", style = "margin-bottom:0;"), HTML("&nbsp;"),
                    if(UsePreLoadedTkrData) {
                      actionLink(inputId = "btn_CompanyList", label = NULL, icon = icon("list"), style = "color:cornflowerblue;")
                    } else { NULL}
                    )
    ,
    inputId = "input_tkr"
    ,
    choices = sptkrs
    ,
    options = list(
      maxOptions = 600
      ,
      create = !UsePreLoadedTkrData
      ,
      placeholder = paste('Up to', SelectizeMaxSelection, 'Ticker Symbols')
      ,
      maxItems = SelectizeMaxSelection #50
    )
    ,
    multiple = TRUE
  )
  ,
  actionButton(
    inputId = "btn_sbmt",
    label = "Get Data",
    icon = icon(name = "download")
  )
  ,
  wellPanel(
    id = "dataCredits",
    style = "position: absolute;bottom: 10px;width: 200px;height: 47px;background-color: rgba(255, 255, 255, 0.32);padding: 7px;font-style: italic;",
    if(UsePreLoadedTkrData) {
      tags$span("Based on preloaded dataset of stocks data collected from multiple sources.",
                style = "font-size: 10px; display: block;"
                )
      
    } else {
          tags$span("Based on Data from",
              tags$a("Yahoo Finance", target="_blank" ,href = "https://finance.yahoo.com" ),
              tags$span(", "),
              tags$a("Google Finance", target="_blank", href = "https://www.google.com/finance"),
              tags$span(", and"),
              tags$a("Morningstar", target="_blank", href ="http://www.morningstar.com/"),
              style = "font-size: smaller; display: block;"
              )
    }
  )
  ,
  style = "margin-left:5px;position:fixed;box-shadow: 10px 10px 15px #888888;height:550px;width: 238px"
)