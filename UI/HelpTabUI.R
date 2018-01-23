tabPanel(title = NULL, 
         value = 5, 
         icon = icon("question-circle fa-2x"),
         div(id = "HelpTextContent",
           tags$h2("About this app and how to use it"),
           tags$h3("About"),
           tags$h4("Purpose"),
           tags$p(
             "This app/report is an ongoing project created for the purpose of experimenting with shinyApps as a flexible analysis and reporting tool, and it is published for the purpose of testing, getting feedback and suggestions from other users. Stocks prices history data is used because it happens to be my favorite dataset, and because it is a reasonable representation of the data I usually work with, so a warning is in order just in case:"),
           div(style= "border-radius: 4px;padding: 10px 5px 2px 10px;margin-bottom: 12px;background-color: rgb(245, 245, 245); box-shadow: 5px 5px 5px #888888;",
             tags$h5(icon("exclamation-triangle"), "Important Warning", icon("exclamation-triangle"),
                     style = "text-align: center;font-weight: 700;color: red;font-family: monospace;"
                     ),
             tags$p(
               style="font-weight: 700;font-family: monospace;",
               "This app is NOT intended to support any investment analysis or decisions and there is NO guarantee regarding the accuracy of data or calculations included in this app, the sole purpose of this app is to experiment with and test shinyApps."
               )
           ),
           tags$h4("Code"),
           tags$p("Full Code is available on", 
                  tags$a("GitHub,", target="_blank" ,href = "https://github.com/selgamal/ShinyTestApp.git" ), "All comments and suggestions are welcomed."),
           tags$h4("Browser - Google Chrome for best results"),
           tags$p(
             "This app was tested on Google Chrome and resolution of 1360 x 768 or higher ONLY, there is no guarantee it will work properly on other browsers, so for best experience use Google Chrome."),
           tags$h3("How to use"),
           tags$h4("General"),
           tags$p(
             "The following video shows an overview of the flow of this app, generally users select the ticker symbols they want to see information about in panel on the left side, and then the data for the selected tickers is retrieved and each tab shows some information and analysis of the selected tickers."),
           HTML('<iframe style = "margin: 0 auto;display: block;" width="560" height="315" src="https://www.youtube.com/embed/xuX83kEm24g" frameborder="0" allowfullscreen></iframe>'),
        tags$h4("Price History Data"),
        tags$p(
                "
               For the app published on shinyapps.io price history data is a preloaded dataset that includes price history of certain stocks, a complete list of available tickers is available on clicking the list icon in the left panel", 
               actionLink(inputId = "btn_CompanyList_help", label = NULL, icon = icon("list"), style = "color:cornflowerblue;")
               ,".", verbatimTextOutput("xxx")),
        tags$p(
               "       
               The app can be set to get price history data for any stock available on Yahoo for last 10 years (from current date) by setting the variable \"UsePreLoadedTkrData\" to FALSE, then the dropdown selection list will be initialized by the S&P500 companies tickers, but any ticker can be input and price history data will be returned if it is available on Yahoo, if data is not available, the user will be alerted for tickers with no or insufficient data.
               "),
        tags$h4("Tabs"),
        tags$h5("Overview Tab", style="font-weight:bold;margin-bottom: 0;"),
        tags$p("This tab shows near real-time price and it is refreshed every 10 seconds; it should be noted that the information shown is dependent on connectivity and availability of source. The market status (open or closed) is shown on the top right side of the navigation bar, obviously if the market is closed, the information is not expected to update."),
        tags$p("The three boxes on top track major market indices, the boxes on the right side track the highest gainer and loser observed during the session. The bubbles visual tracks last bid size (bubble size) and last change (bubble color) for the selected tickers. More information about the selected tickers is available on clicking the \"More Info\" button."),
        tags$p("On clicking any of the bubbles or boxes a charts pops up that tracks closing price for the current day, the user can select to see the history, if the preloaded data is used then it will show daily price history with range according to the preloaded data, if live data is used it will show 10 years daily price history from current date."),
        
        tags$h5("US Treasury Yield Curve", style="font-weight:bold;margin-bottom: 0;"),
        tags$p("This tab shows US Treasury Yield Curve history based on date from US treasury website. The main table just lists the month end rates. The first chart on the right side can compare up to 3 yield curves on the specified dates in the input boxes, in addition to a \"playable\" curve that animates the change in yield curve over the last 100 months, this is triggered by clicking the play button in the bottom of the chart. The second chart on the right side plots the rate history of maturities as per user selection."),
        
        tags$h5("Price History & Financials", style="font-weight:bold;margin-bottom: 0;"),
        tags$p("This tab zooms in on each stock; all selected stocks are displayed in the top panel and the user can navigate between them. For the selected stock the focus is on two main areas, Price History and Financial Information, the user can navigate between those areas by clicking the area to display in the second panel."),
        tags$p(span("Price History", style = "font-weight:600;"),
               "is displayed by default, and has the following sections:", 
               style ="margin-bottom: 0;",
               tags$ul(
                 tags$li("Candlestick chart: displays price history for the date range selected in top right side, in addition to some indicators, like SMA, RSI and MACD that can be set by clicking the \" Change Indicators Inputs\" link. The user can select between 2 types of charting packages, either Quantmod ChartSeries or HighCharts."),
                 tags$li("Other Information: On clicking the (+) icon to expand the seciont, it displays some basic price information based on the selected date range."),
                 tags$li("Performance: On clicking the (+) icon to expand the seciont, it displays some commonly used metrics to measure the performance of the stock. The user can change inputs for the calculations of these metrics in the first panel under the \"Performance\" title panel, and all metrics and charts will update automatically."),
                 tags$li("Valuation Ratios: On clicking the (+) icon to expand the seciont, it displays available historical valuation ratios")
               )
               ),
        tags$p(span("Financial Information", style = "font-weight:600;"),
               "displays captions of the main financial statements and related visuals. The user can select to display quarterly or annual financial information from the drop down on the top of the section."),
        
        tags$h5("Portfolio", style="font-weight:bold;margin-bottom: 0;"),
        tags$p("In this tab, the user can compare the performance of all or subset of the initially selected stocks, and then the user can put together up to 10 portfolios and compare them. This tab is divided into three sections as follows:"),
        tags$p(span("Compare Tickers:", style = "font-weight:600;"),
               "In this section, the user can select a subset or all stocks available stocks to compare performance, see correlation and risk versus returns for each stock. Stocks are selected from the dropdown list, and user selects benchmark and return calculation frequency to be used, and then click the button to update charts and metrics, noting in this section, user needs to click to update calculation when changing any of the inputs."),
        tags$p(span("Optimize a Portfolio:", style = "font-weight:600;"), style = " margin-bottom: 0;",
               "In this section, the user can select any number of available tickers and optimize and save up to 10 portfolios as follows:",
               tags$ul(
                 tags$li("Optimization", tags$br(),
                         tags$p("The optimization is based on \"portfolio.optim\" function from \"tseries\" package, which as per its description \" Computes an efficient portfolio from the given return series x in the mean-variance sense.\" and this is calculated over multiple returns levels and then the portfolio with the highest Return per Risk unit is selected as the optimal portfolio."),
                         tags$p("First the user selects optimization inputs (Tickers, Min/Max weights, Risk Free rate, Date range and frequency of returns used), then click \"Get Optimal Portfolio\", if the optimization is successful, a status message will appear with summary results, and the remaining of the visuals in this section is populated by the optimized portfolio data, and the user can review the visuals and calculated metrics and repeat this process, taking note that at this point the created portfolio is NOT stored yet, and it is labeled as \"Unsaved\".")
                         ),
                 tags$li("Saving a portfolio", tags$br(),
                         tags$p("To store the currently \"Unsaved\" portfolio, go to the save panel just below the \"Get Optimal Portfolio\" button, select \"Save\" from the first dropdown list, and make sure that \"Unsaved\" is selected in the second dropdown list, then enter a name for the portfolio in the last input space and click save (each portfolio saved should have a unique name). This process is repeated and up to 10 portfolios can be saved.")
                         ),
                 tags$li("Deleting Saved portfolio", tags$br(),
                         tags$p("To delete a saved portfolio, go to the save panel just below the \"Get Optimal Portfolio\" button, select \"Delete\" from the first dropdown list, and from the second dropdown list, select the portfolio to delete and click Delete.")
                 ),
                 tags$p("Each saved portfolio can be displayed by selecting it from the \"Select Saved Portfolio To Display\" dropdown list below the save panel, and calculations parameters can be changed as needed (date range, return frequency...etc.).")
               )
               ),
        tags$p(span("Compare Saved Portfolios:", style = "font-weight:600;"),
                    "In this section, portfolios saved in previous section can be compared, and calculations inputs can be changed as needed, noting that user need to click \"Update Charts\" button to effect changes after changing inputs."
                    )
        
        ),
        
        div(id = "height_tab5", style = "height:21px;")
         )