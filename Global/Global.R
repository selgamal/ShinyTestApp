library(shiny)
library(shinyjs)
library(jsonlite)
library(rvest)
library(lubridate)
library(quantmod)
library(plyr)
library(dplyr)
library(sparkline)
library(shinydashboard)
library(htmlwidgets)
library(formattable)
library(DT)
library(bubbles)
library(dygraphs)
library(shinyjqui)
library(highcharter)
library(tidyquant)
library(reshape2)
library(shinyBS)
library(tseries)
library(Matrix)
library(matrixcalc)

source("Global/fnFunctions/fnRiskFreRateTablesDump.R", local = T)$value
source("Global/fnFunctions/fnGetHistoryDump.R", local = T)$value
source("Global/fnFunctions/fnGetRTDataDump.R", local = T)$value
source("Global/fnFunctions/fnGetSP500tkrsDump.R", local = T)$value
source("Global/fnFunctions/fnAllTkrsAllPeriodsDump.R", local = T)$value
source("Global/fnFunctions/GetIntradayGoogleDump.R", local = T)$value
source("Global/fnFunctions/fnRefreshDataDump.R", local = T)$value
source("Global/fnFunctions/fnHcboxplotModDump.R", local = T)$value
source("Global/fnFunctions/fnoutliersPositionsDump.r", local = T)$value
source("Global/fnFunctions/fnMakeTablePortDump.R", local = T)$value
source("Global/fnFunctions/fnEfficientFrontier_tseriesDump.R",
       local = T)$value

####### Some Settings and Global variables ######

SelectizeMaxSelection <-
  40 # max number of tickers that can be selected in all selectize boxes

# This is to control whether to grap the data from online or use a pre-existing dataset;
# if "UsePreLoadedTkrData" is:
# True: loads and uses an existing dataset,
# Flase: gets the data from yahoo/google/morningstar according to selections
UsePreLoadedTkrData <- T
if (UsePreLoadedTkrData) {
  Preloaded_TkrsHistory <- readRDS("RDSs/tkrsHistoryDataset.rds")
  Preloaded_TkrsRets <- readRDS("RDSs/tkrsReturns.rds")
  Preloaded_indxHistory <- readRDS("RDSs/indxHistoryDataset.rds")
  Preloaded_indxRets <- readRDS("RDSs/indxReturns.rds")
  Preloaded_YieldCurveDta <-
    readRDS("RDSs/YieldCurveRatesDataset.rds")
  Preloaded_TableOCompanies <- readRDS("RDSs/TableOfCompanies.rds")
}
# I used this for testing and it turns out it enhances performance on shinyapps.io, because if multiple
# users are using the app at the same time all sessions will freeze if one user is downloading tickers' data
# directly from the internet, there is no problem of setting it to False if it is used on personal desktop.

# figure min / max date to use in date rages to use in dateRange inputs in app

if (UsePreLoadedTkrData) {
  DateRanges <- data.frame()
  for (t in names(Preloaded_TkrsHistory)) {
    a <- Preloaded_TkrsHistory[[t]]$History
    a <- data.frame(
      tkr = t,
      From = as.Date(min(index(a))),
      To = as.Date(max(index(a))),
      stringsAsFactors = F
    )
    DateRanges <-
      rbind.data.frame(DateRanges, a, stringsAsFactors = F)
  }
  
  MinDate_Global <- min(DateRanges$From)
  MaxDate_Global <- max(DateRanges$To)
  rm(DateRanges, t, a)
} else {
  MinDate_Global <- Sys.Date() - years(10)
  MaxDate_Global <- Sys.Date()
}


# This intializes selections dropdown in the first select box, if using preloaded dataset then gets the names
# of tikcers in the dataset and selections will be limited to available tickers, if internet data is used
# then it just intializes the selection with SP500 tickers, and in this case, selection is not restricted to
# selection list, any ticker symbol can be entered and it will return data as long as it is available
# on yahoo and it will reasonably handle missing data.
sptkrs <-
  if (UsePreLoadedTkrData &&
      length(names(Preloaded_TkrsHistory) > 0)) {
    names(Preloaded_TkrsHistory)
  } else {
    GetSP500tkrs()
  }


# colors used in bubles visual
colscaldf <- data.frame(
  colorid = c(1:7)
  ,
  colrs = c(
    "#C40000",
    "#FF0000",
    "#FF675C",
    "#50EA50",
    "#08A508",
    "#006400",
    "black"
  )
  ,
  stringsAsFactors = F
)

#### function to customize nav-bar based on tips from
#### https://github.com/daattali/advanced-shiny/tree/master/navbar-add-text
navbarPageWithText <- function(..., alert, time) {
  navbar <- navbarPage(...)
  #helpEL <- tags$div(class = "navbar-help", id = "appHelp", help)
  alertEl <- tags$div(class = "navbar-alert", id = "alert1", alert)
  timetEl <- tags$form(class = "navbar-text", id = "timenow", time)
  navbar[[3]][[1]]$children[[1]] <-
    tagAppendChildren(navbar[[3]][[1]]$children[[1]], alertEl, timetEl)
  navbar
}

# Highcharter theme ####

chartColors <-
  c(
    '#7cb5ec',
    '#90ed7d',
    '#f7a35c',
    '#7b7b84',
    '#8085e9',
    '#f15c80',
    '#e4d354',
    '#2b908f',
    '#f45b5b',
    '#91e8e1'
  )

thm <- hc_theme(
  colors = c(
    '#7cb5ec',
    '#90ed7d',
    '#f7a35c',
    '#7b7b84',
    '#8085e9',
    '#f15c80',
    '#e4d354',
    '#2b908f',
    '#f45b5b',
    '#91e8e1'
  ),
  chart = list(
    backgroundColor = "#F0F0F0"
    ,
    plotBorderColor = "#606063"
    ,
    style = list(fontFamily = "Roboto",
                 color = "#3C3C3C"),
    borderRadius = "6px"
  ),
  xAxis = list(
    gridLineWidth = 1,
    gridLineColor = "#D7D7D8",
    gridLineDashStyle = "Dash",
    lineColor = "#D7D7D8",
    minorGridLineColor = "#505053",
    tickColor = "#D7D7D8",
    tickWidth = 1
  ),
  yAxis = list(
    gridLineColor = "#D7D7D8",
    gridLineDashStyle = "Dash",
    lineColor = "#D7D7D8",
    minorGridLineColor = "#505053",
    tickColor = "#D7D7D8",
    tickWidth = 1
    
  )
)
