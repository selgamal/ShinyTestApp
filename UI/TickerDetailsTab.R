####__Ticker Details Tab #####


tabPanel(value = 2, style="height: 2500px;",
  "Price History & Financials "
  ,
  wellPanel(
    tags$h4("Select Ticker To Display Available Financials and Ratios"
            ,
            style = "margin-top: 0px;")
    ,
    div(style = "position:relative;z-index:0;",
      HTML('<i id="tkrsload" class="fa fa-spinner fa-pulse"></i>'),
      uiOutput("slct_Tkr_fs", class = "nav")
    )
    
    ,
    style = "background-color: white;border: 1px solid lightgray; padding: 5px;margin-bottom: 5px;min-height: 60px;"
  )
  ,
  verbatimTextOutput("tkrdetailstst")
  ,
  wellPanel(
     tags$ul(id = "div_price_fs",
    tags$li(actionLink(inputId = "price_fs",label =  "Price History",style = "float:left;border-radius: 8px;"), class = "active")
    ,
    tags$li(actionLink(inputId = "price_fs",label =  "Financial Information", style = "float:left;border-radius: 8px;"))
    , class = "nav")
    ,
    HTML('<i id="CoNameload" class="fa fa-spinner fa-pulse"></i>')
    ,
    tags$h4(textOutput("CompanyName"), style ="font-weight: 600;margin-bottom: 1px;")
    ,
    style = "background-color: white;border: 1px solid lightgray; padding: 5px;margin-bottom: 5px;position:relative;z-index:0;min-height: 80px;"
  )
  ,
  source("UI/TickerDetialsTab_FS.R", local = T)$value
  ,
  source("UI/TickerDetialsTab_PriceHistory.R", local = T)$value
  , div(id = "height_tab3", style = "height:21px;")

)