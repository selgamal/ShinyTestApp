# portfolio tab #####
tabPanel(
  value = 3,
  "Portfolio",
  tags$h4("Portfolio"),
  wellPanel(
    tags$p(
      "Tickers can be compared in this tab, portfolios can be created and optimized (simple optimization, nothing fancy), and created portfolios can be compared. For best use:",
      style = "margin: 0 0 2px; font-weight: 700;"
    ),
    tags$ol(
      tags$li(
        tags$span("First sction \"Compare Tickers\":",
                  style = "font-weight:bold"
                  ), "Select tickers to compare based on few metrics. Selected tickers can be all or subset of the tickers initially selected in the sude panel on the left."
      ),
      tags$li(
        tags$span("Second section \"Optimize a Portfolio\":",
                  style = "font-weight:bold"
        ),"Select tickers to create a portfolio, then Input desired option for portfolio optimization, review results and if desired, save the resulting portfolio (can save 10 portfolios only)."
      ),
      tags$li(
        tags$span("Third section \"Compare Saved Portfolios\":",
                  style = "font-weight:bold"
        ),"Portfolios saved in second section can be compared based on few metrics."
      ),
      style = "margin: 0 0 5px;-webkit-padding-start: 13px;"
    ),
    style = "background-color: white;
    border: 1px solid lightgray;
    margin-bottom: 5px;
    min-height: 60px;
    font-size: 13px;
    padding: 7px 7px 0px 7px;
    font-style: italic;"
  ),
  fluidRow(id = "port_sectionOneTitleRow",
           column(
             width = 12,
             wellPanel(
               id = "port_sectionOneTitlePanel",
               style = "box-shadow: none;overflow: hidden;padding: 5px;margin-top: 3px;border-radius: 0;margin-bottom: 3px;border-width: 0; border-color: transparent;background-color: #f9f9f9;",
               tags$h5("Compare Tickers", style = "margin:3px;float:left"),
               actionButton(
                 "HideShow_port_sectionOne",
                 label = "+",
                 style = "float:right;padding-top: 0px;height: 22px;"
               )
             )
           )),
  source("UI/PortfolioTab_SectionOne.R", local = T)$value
  ,
  fluidRow(id = "port_sectionTwoTitleRow",
           column(
             width = 12,
             wellPanel(
               id = "port_sectionTwoTitlePanel",
               style = "box-shadow: none;overflow: hidden;padding: 5px;margin-top: 3px;border-radius: 0;margin-bottom: 3px;border-width: 0; border-color: transparent;background-color: #f9f9f9;",
               tags$h5("Optimize a Portfolio", style = "margin:3px;float:left"),
               actionButton(
                 "HideShow_port_sectionTwo",
                 label = "+",
                 style = "float:right;padding-top: 0px;height: 22px;"
               )
             )
           )),
  source("UI/PortfolioTab_SectionTwo.R", local = T)$value,
  fluidRow(id = "port_sectionThreeTitleRow",
           column(
             width = 12,
             wellPanel(
               id = "port_sectionThreeTitlePanel",
               style = "box-shadow: none;overflow: hidden;padding: 5px;margin-top: 3px;border-radius: 0;margin-bottom: 3px;border-width: 0; border-color: transparent;background-color: #f9f9f9;",
               tags$h5("Compare Saved Portfolios", style = "margin:3px;float:left"),
               actionButton(
                 "HideShow_port_sectionThree",
                 label = "+",
                 style = "float:right;padding-top: 0px;height: 22px;"
               )
             )
           )),
  source("UI/PortfolioTab_SectionThree.R", local = T)$value
  , div(id = "height_tab4", style = "height:21px;")
  )