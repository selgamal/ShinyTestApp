# ShinyTestApp
This app provides some information and analysis for 95 stocks based on daily prices between Jan 1, 2008 to Dec 29, 2017. See app on shinyapps.io [here](https://selgamal.shinyapps.io/ShinyTestApp/).

This app is created for testing purpose; I want to see if I can use shiny apps as a convenient and portable reporting and analysis tool. It includes functionalities I would like to use in my work; in addition to other functionalities I read about from other contributors, and it is all adapted to provide the information about stocks. 

## **WARNING**
This app is NOT intended to support any investment analysis or decisions and there is NO guarantee regarding the accuracy of data or calculations included in this app, the sole purpose of this app is to experiment with and test shinyApps.

## **Data**
The information showing on the app's first tab  "Overview" is downloaded directly from Yahoo and Google finance, and refreshed every 10 seconds. The rest of the analysis is based on the a preloaded datasets containing the daily price history for 95 stocks between Jan 1,2008 to Dec 29, 2017 as mentioned above, the script to create the datasets used in the app is located [here](RDSs/ScriptToCreatePreloadedHistory.R). Also the app can be set to downlaod data directly from yahoo by changing the [Global File](Global/Global.R) and setting variable "UsePreLoadedTkrData" to `FALSE`, and the app will download 10 years price history ending current date,  also max number of stocks handled by the app can be set by variable "SelectizeMaxSelection".

## **Video Instructions**
Video on how to use the app is available [here](https://youtu.be/xuX83kEm24g)
 
 ## **Installation**
 To make sure all packages required to run the app are installed, run the following script:
 ```r
 ## Check for packages needed to run the app
appPkgs <- 
  c("shiny", "shinyjs", "jsonlite", "rvest", "lubridate", "quantmod", "plyr", "dplyr",
  "sparkline", "shinydashboard", "htmlwidgets", "formattable", "DT", "bubbles", 
  "dygraphs", "shinyjqui", "highcharter", "tidyquant", "reshape2", "shinyBS", "tseries",
  "Matrix", "matrixcalc", "corrplot", "htmltab")


## install "bubbles" package from github (https://github.com/jcheng5/bubbles)
install.packages("devtools")
devtools::install_github("jcheng5/bubbles")

## Detect missing packages
missingPkgs <- setdiff(appPkgs, installed.packages()[,"Package"])

## Review packages needed to be installed and install them anyway you like
print(missingPkgs)

################# OR ################################
## for convenience run the following line to install missing packages
## make sure the getOption("repos") points to CRAN (the default)
install.packages(missingPkgs)
 ```
 ## **Run App**
 ```r
 shiny::runGitHub("selgamal/shinytestapp")
 ```
This app is calculation intensive, it is recommended to run locally as shown above, because it might freeze a little bit if multiple users are using it at the same time on shinyapps.io.
 
 All comments, suggestions and ideas are welcomed.
