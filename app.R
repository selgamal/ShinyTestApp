

# Global items (load libraries, functions, global variables...) ####
source("Global/Global.R", local = T)$value

ui <- tagList(
  # Head Tag #####
  tags$head(
    htmlwidgets::getDependency('sparkline'),
    # to be able to use sparklines
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # my styles
    tags$script(src = "jscode.js"),
    # My js code
    tags$title("Shiny Test App")
  )
  ,
  useShinyjs()
  ,
  # BS modal containing Data table on clicking "More Info" button ####
  source("UI/DataTableModal.R", local = T)$value
  ,
  # BS Modal containing dygraphs on clicking any ticker ui ####
  source("UI/DygraphModal.R", local = T)$value
  ,
  # BS modal containing Companies List on clicking list Icon in side bar ####
  source("UI/CompaniesListModal.R", local = T)$value
  ,
  # Sidebar ####
  div(Id = "showsidebar",
      icon("bars")),
  source("UI/SideBar.R", local = T)$value
  ,
  #### NavBar #####
  navbarPageWithText(
    id = "main_tabs",
    div(
      "Shiny Test App",
      style = "line-height: 12px;",
      span("Beta 0.01", style = "font-size:10px; font-style:italic;color: cyan;"),
      tags$br(),
      span("Best viewed on Chrome 1360 x 768 Resolution or higher", style = "font-size:9px; font-style:italic;")
    ),
    position =  "fixed-top" ,
    collapsible = T,
    selected = 5,
    ###__Overview Tab ####
    source("UI/OverviewTab.R", local = T)$value
    ,
    ####__US Treasury yield curve rates #####
    source("UI/RiskFree.R", local = T)$value
    ,
    ####__Ticker Details Tab #####
    source("UI/TickerDetailsTab.R", local = T)$value
    ,
    ####__Portfolio Tab #####
    source("UI/PortfolioTab.R", local = T)$value
    ,
    source("UI/HelpTabUI.R", local = T)$value
    ,
    alert = source("UI/NavBarAlert.R", local = T)$value
    ,
    time =  div(tags$span(icon("circle-o-notch fa-spin"), "Loading....",
                          style = "position: absolute;z-index: -1;margin-left: 115px;font-family: monospace;")
                , uiOutput("time", style = "background-color: #f8f8f8;"), 
                style = "position: relative;z-index: 0;width: 255px;")
  ),
  # to be able to use items form shiny dashboard, the body display is set as none in styles.css
  dashboardPage(
    body = dashboardBody(),
    header = dashboardHeader(disable = T),
    sidebar = dashboardSidebar(disable = T)
  ),
  div(
    id = "footer",
    tags$div(
      style = "margin-bottom: 0px; width: 300em;",
      span(
        icon("exclamation-triangle"),
        "Important Warning",
        icon("exclamation-triangle"),
        style = "text-align: center;font-weight: 700;color: red;"
      ),
      span(
        style = "font-weight: 700;font-family: monospace;",
        "This app is NOT intended to support any investment analysis or decisions and there is NO guarantee regarding the accuracy of data or calculations included in this app, the sole purpose of this app is to experiment with and test shinyApps."
      )
    )
  )
)


server <- function(input, output, session) {
  ## Things to do in the begining of each session
  scipenop <- getOption("scipen")
  maxprint <- getOption("max.print")
  options(scipen = 999999)
  options(max.print = 999999)
  
  ### prep risk free/ yield curve dataset
  ### Update existing yield curve rates RDS with current year (bring it up to date for this session)
  ### OR
  ### do nothing and just Use existing Dataset preloaded on app start
  
  RiskFree_dta <- if (UsePreLoadedTkrData) {
    Preloaded_YieldCurveDta
  } else {
    fnRiskFreRateTables(x = readRDS("RDSs/YCHistory.rds"))
  }
  
  YC_melt <- RiskFree_dta$YC_melt
  YC_monthly.df <- RiskFree_dta$YC_monthly.df
  YC_xts <-
    xts::last(RiskFree_dta$YC_xts, "100 months") # seems that 100 series is the limit for hc_motion
  YCdf.mon <- RiskFree_dta$YCdf.mon
  maxYCT_date <- RiskFree_dta$maxYCT_date
  Max_Rf <- RiskFree_dta$Max_Rf
  
  rm(RiskFree_dta) # clean up
  
  ###Reactive Values ####
  RVs <-
    reactiveValues(
      # stores time and weather market is open or closed
      timeNstats = format(Sys.time(), "%a, %b %d, %Y %I:%M %p %Z", tz = "EST")
      ,
      # stores the selected tickers in the selectize input
      t = vector()
      ,
      # market indexes used, currently no UI input to change those, hard coded for now
      indx = c("^GSPC", "^IXIC", "SPY", "^DJI")
      ,
      # real time index data from getQuote
      Indxdta = data.frame()
      ,
      # real time tkr data from getQuote
      tkrdta = data.frame()
      ,
      # needed to detect top gainer
      maxlim = 0.1
      ,
      # needed to detect top loser
      minlim = -0.1
      ,
      # session gainer quote data
      sessiongainer = data.frame()
      ,
      # session loser quote data
      sessionloser = data.frame()
      ,
      # ticker symbols for gainer and loser
      sessionGnrLsrNames = c()
      ,
      # loser price history
      sessiongnrloserHistory = list()
      ,
      # gainer price history
      sessionGnrLoserIntra = list()
      ,
      # dataframe to populate bubbles visual
      bblz = data.frame()
      ,
      # used in constructing the color bar for the bubbles visual
      seq = unique(c(
        seq(-0.01, 0, length.out = 4), seq(0, 0.01, length.out = 4)
      )) #unique(c(seq(-0.01,0, abs(-0.01)/3), seq(0, 0.01, 0.01/3)))
      ,
      # data used in DataTable
      dtable = data.frame()
      ,
      # indexes history (10 years daily price, 52 weeks)
      indxHistoryData = list()
      ,
      # indexes intraday price for sparkline and charts
      indxIntraday = list()
      ,
      # All ticker History Data (10 years daily price, 52 weeks)
      tkrHistoryData = list()
      ,
      tkrIntraday = list()
      ,
      # to store currently shown/selected area in the dygraph (used to adjust max/min prices markers for the shown area)
      lims = as.character() # 10 years chart
      ,
      lims2 = as.character() # intraday
      ,
      # stores the symbol for the ticker shown in the value box visual
      slctdbxsym = as.character()
      ,
      # helper to trigger stop/resume observers when changed
      resumeObs1 = 0
      ,
      # helper to trigger stop/resume observers when changed
      resumeobs2 = 0
      ,
      # helper to trigger stop/resume observers when changed
      rmnotify = 0
      ,
      # stores the intial limit for dygraph chart
      dyintitlim = NULL
      ,
      # not used
      strtdygraph = 0
      ,
      # not used
      chartrange = NULL # c(as_datetime(Sys.time(), "EST")-years(10),as_datetime(Sys.time(), "EST"))
      ,
      # helper to trigger stop/resume observers when changed
      resumeintraobs = 0
      ,
      # helper to trigger stop/resume observers when changed
      resumGnlsrNamsObs = 0
      ,
      # helper to trigger stop/resume observers when changed
      resumeintraobs3 = 0
      ,
      # helper to trigger stop/resume observers when changed
      intradone1 = 0
      ,
      # Store ticker symbol selected in Financials and Ratios tab
      fs_ratio_slctd_tkr = NA
      ,
      fs_dataplt = NULL
      ,
      fs_data = NULL
      ,
      ratios_data = NULL
      ,
      dfComp = data.frame()
      ,
      candlestick_hc_daterange = paste(Sys.Date() - years(1), Sys.Date() , sep = "/")
      ,
      Candlestick_qm_range = NULL
      
    )
  
  #### General UI Elements ######
  ####_ Toggle alert info on clicking alert icon ####
  observeEvent(input$clkd, {
    toggleElement("alertdropdown", anim = T)
  })
  
  ###_ Calculate time and Market state #####
  
  source("Server/TimeMarketState.R", local = T)$value
  
  
  # Initial get indexes price history on change of selected indexes ####
  # (currently only once per session - no option to change indexes)
  observeEvent(RVs$indx, {
    if (UsePreLoadedTkrData) {
      RVs$indxHistoryData <- Preloaded_indxHistory
    } else {
      x <- mapply(
        FUN = function(x, y) {
          closeAllConnections()
          tkr <- tryCatch({
            tkr <-  suppressWarnings(getSymbols(x, from = y, auto.assign = F))
            tkr <- na.omit(tkr)
            names(tkr) <-
              c("Open", "High", "Low", "Close", "Volume", "AdjClose")
            tkr <- round(tkr, 2)
            Sys.sleep(.5)
            return(list(History = tkr))
          }, error = function(e)
            NULL)
        }
        ,
        x = RVs$indx
        ,
        y = Sys.Date() - years(10)
        ,
        SIMPLIFY = F
      )
      
      RVs$indxHistoryData <- x
    }
  })
  
  # Alert user to click submit when selectioned tickers change
  observeEvent(input$input_tkr
               ,
               if (!identical(RVs$t.all, toupper(input$input_tkr))) {
                 runjs(HTML(
                   "document.getElementById('btn_sbmt').style.backgroundColor = 'tomato'"
                 ))
               } else {
                 runjs(HTML(
                   "document.getElementById('btn_sbmt').style.backgroundColor = ''"
                 ))
               })
  
  
  # Download ticker data on clicking "Get Data" after selecting tickers
  # download takes a some time so intially all other observers are suspended to limit the mess as much as possible
  observeEvent(input$btn_sbmt, {
    # suspend regular quote updates
    refreshObs$suspend()
    
    # hide the alert in nav-bar for tickers with no data to prepare for the new download
    runjs(
      '
      $( document ).ready(function() {
      var a = document.getElementById("alert1");
      $(a).css("visibility", "hidden");
      });
      '
    )
    source("Server/GetTickerData.R", local = T)$value
    if (!identical(RVs$t.all, toupper(input$input_tkr))) {
      runjs(
        HTML(
          "document.getElementById('btn_sbmt').style.backgroundColor = 'tomato'"
        )
      )
    } else {
      runjs(HTML(
        "document.getElementById('btn_sbmt').style.backgroundColor = ''"
      ))
    }
    # to resume regular quote updates
    RVs$resumeObs1 <- RVs$resumeObs1 + 1
    
}, priority = 50)
  
  #### Organizing price history data in an easy to access form for use later
  #### see function "fnAllTkrsAllPeriods" Global/fnFunctions/fnAllTkrsAllPeriodsDump.R
  #### returns one xts for all tickers/indexes for each daily/weekly/monthly  returns and closings
  
  ##### first indexes
  observeEvent({
    RVs$indxHistoryData
  },
  {
    req(length(RVs$indxHistoryData) > 0, cancelOutput = T)
    if (UsePreLoadedTkrData) {
      RVs$rets_bm_allperiods <- Preloaded_indxRets
    } else {
      RVs$rets_bm_allperiods <-
        fnAllTkrsAllPeriods(RVs$indxHistoryData, type = "History")
    }
  })
  
  #### then tickers
  observeEvent({
    RVs$tkrHistoryData
    
  },
  {
    req(length(RVs$tkrHistoryData) > 0, cancelOutput = T)
    if (UsePreLoadedTkrData) {
      RVs$rets_assts_allperiods <- Preloaded_TkrsRets
    } else {
      RVs$rets_assts_allperiods <-
        fnAllTkrsAllPeriods(RVs$tkrHistoryData, type = "AdjHistory")
    }
  })
  
  
  
  ### Update Quote data ####
  
  # Observer to restart trigger for regular quotes update (when stopped in previous process)
  observeEvent({
    RVs$resumeObs1
  }, {
    delay(10000, refreshObs$resume())
  })
  
  # Helper triggers value changing every X secs to trigger updates
  trgr10 <- reactive({
    invalidateLater(10000)
    rnorm(1)
  })
  
  trgr60 <- reactive({
    invalidateLater(60000)
    rnorm(1)
  })
  
  
  # Process to update all quote data every X secs
  refreshObs <-
    observeEvent(trgr10(), {
      req(input$main_tabs %in% c(1, 5)) # refresh only if "overview" tab is selected
      source("Server/RefreshTickerData.R", local = T)$value
    }, priority = 40)
  
  output$tkr_stats <-
    renderUI(HTML(paste0(RVs$tkrdta$Marqhtml))) #renderUI(HTML(paste0(RVs$tkrdta[with(RVs$tkrdta, order(-RVs$tkrdta$changeNum1)),]$Marqhtml)))
  
  
  ### Reneder Value Boxes ####
  
  source("Server/ValueBoxes.R", local = T)$value
  
  ### Render Color bar and Bubbles Viz ####
  
  # The bubles viz displays stock status with color of the buble representing how
  # deep is the gain/loss and size representing the size of last trade.
  # colors selection is in colorscaledf, 6 color buckets used (3 place for over and under -0- ).
  # On download each stock is allocated to a bucket, a seq is used (n=7 from min to max change%)
  # see( fnRefresh)
  
  source("Server/BubblesViz.R", local = T)$value
  
  ### Data Table ####
  
  # Prep data for the data table and sparklines
  
  observeEvent(input$seeDetails, {
    z <- isolate(RVs$tkrdta)
    z$`52 Weeks` <-
      mapply(function(x) {
        paste(RVs$tkrHistoryData[[x]]$spark52wks, collapse = ",")
      }, x = z$Symbol, USE.NAMES = F)
    
    RVs$dtable <- z
    toggleModal(
      session = session,
      modalId = "detailstable",
      toggle = "open"
    )
  })
  
  observeEvent(input$refreshTable, {
    z <- isolate(RVs$tkrdta)
    z$`52 Weeks` <-
      mapply(function(x) {
        paste(RVs$tkrHistoryData[[x]]$spark52wks, collapse = ",")
      }, x = z$Symbol, USE.NAMES = F)
    RVs$dtable <- z
  })
  
  
  # These vars store previous selections, searches and state of the table before data refresh
  previousSelection <- NULL
  tablesearch <- ""
  rttablestate <- NULL
  observeEvent(input$refreshTable, {
    previousSelection <<- input$rttable_rows_selected
    tablesearch <<- input$rttable_search
    rttablestate <<- input$rttable_ordering
  })
  
  source("Server/RenderDataTable.R", local = T)$value
  
  if (UsePreLoadedTkrData) {
    source("Server/RenderCompaniesListDataTable.R", local = T)$value
  }
  
  
  #### Popup chart #####
  #### This chart is displayed within a popup modal when boxes or bubbles are clicked
  
  source("Server/PopupCharts.R", local = T)$value
  
  #### Ticker Details Tab ############
  
  source("Server/TickerDetailsTabServer.R", local = T)$value
  
  #### Portfolio Tab ############
  
  source("Server/PortfolionTabServer_main.R", local = T)$value
  
  #### Risk Free Tab ############
  
  source("Server/RiskFreeTabServer.R", local = T)$value
  
  ### Stop refreshes when modal is open or switching to a tab other than "overview" tab ####
  
  observeEvent({
    input$plot_modal_open
  }
  , {
    refreshObs$suspend()
    
  })
  
  observeEvent({
    input$plot_modal_close
  }
  , {
    delay(500, refreshObs$resume())
  })
  
  observeEvent({
    input$CompniesTable_modal_open
  }
  , {
    refreshObs$suspend()
    
  })
  
  observeEvent({
    input$CompniesTable_modal_close
  }
  , {
    delay(500, refreshObs$resume())
  })
  
  onSessionEnded(function() {
    options(scipen = scipenop)
    options(max.print = maxprint)
    closeAllConnections()
  })
  
  ### End ####
  }

shinyApp(ui = ui, server = server)
