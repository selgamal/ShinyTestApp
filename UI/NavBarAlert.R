
# Alert showing on the navbar for tickers with no data #### 
tagList(icon("warning")
        , div(
          id = "alertdropdown"
          , tags$span(id = "dropdownlist"
                      , tagList(
                        tags$span("No Data or Data not Enough For: ", style = "font-weight:bold;")
                        ,
                        tags$hr(style = "margin-top:0; margin-bottom:0;")
                        ,
                        textOutput("dropdowntxt")
                      ))
        ))