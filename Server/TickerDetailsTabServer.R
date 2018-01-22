###___Generate ticker selection links ####

# Populate A div containing tickers with data to select from to display price history, financials and ratios

output$slct_Tkr_fs <- renderUI({
  shiny::validate(need(length(RVs$t) > 0, "Please Select Tickers and Download Data"))
  RVs$t
  mylinks <- mapply(
    FUN = function(x, y) {
      tags$li(
        actionLink(
          inputId = "lnk",
          label = x,
          style = "border-radius: 8px;"
        )
        ,
        style = "float:left;",
        class = y
      )
    },
    x =  sort(RVs$t),
    y = c("active", rep("notactive", length(RVs$t) - 1)),
    SIMPLIFY = F
  )
})

outputOptions(output, "slct_Tkr_fs", suspendWhenHidden = FALSE)


# Reactive value "RVs$fs_ratio_slctd_tkr" is used to pass through the ticker selected for displaying information, 
# if there were no tickers selected in the sidebar (nothing is populated in previous step), no data displayed,
# if ticker data is downloaded it defaults to the first ticker

observeEvent({
  RVs$t
}, {
  RVs$fs_ratio_slctd_tkr <- sort(RVs$t)[1]
})

observeEvent({
  input$js_tkrlnk[1]
}, {
  RVs$fs_ratio_slctd_tkr <- input$js_tkrlnk[1]
})

# Display company name for the selected ticker ####

output$CompanyName <- renderText({
  shiny::validate(need(!is.na(RVs$fs_ratio_slctd_tkr), "Please select a Company"),
                  need(!is.null(RVs$fs_ratio_slctd_tkr), "Please select a Company"))
  name <- RVs$tkrdta[RVs$tkrdta$Symbol == RVs$fs_ratio_slctd_tkr, 2]
  curr <-
    if (length(RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$currency) != 0) {
      paste(
        "(",
        gsub(
          "Currency ",
          "",
          RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$currency,
          ignore.case = T
        ),
        ")",
        sep = ""
      )
    } else {
      NULL
    }
  name.curr <- paste(name, curr, sep = " ")
  return(name.curr)
})


outputOptions(output, "CompanyName", suspendWhenHidden = FALSE)


# Show Price History or Financials based on selection, price history shown by default, fs dsiplay none by default ####

showElement("tkr_price_history")
hideElement("tkr_fs_ratios")


observeEvent(input$js_price_fs,
             {
               if (input$js_price_fs == "Financial Information") {
                 hideElement("tkr_price_history")
                 showElement("tkr_fs_ratios")
               } else {
                 showElement("tkr_price_history")
                 hideElement("tkr_fs_ratios")
               }
             })


observeEvent(
  {
    input$UseAdjusted
    input$js_tkrlnk
    RVs$fs_ratio_slctd_tkr
  }
  
  ,{
  req(length(RVs$fs_ratio_slctd_tkr)>0)
  RVs$PriceHistory <- 
  if (input$UseAdjusted) {
    RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$AdjHistory[,1:5]
  } else {
    RVs$tkrHistoryData[[RVs$fs_ratio_slctd_tkr]]$History[,1:5]
  }
})

source("Server/TickerDetailsTabServer_FS_div.R", local = T)$value

source("Server/TickerDetailsTabServer_PriceHistory_div.R", local = T)$value
