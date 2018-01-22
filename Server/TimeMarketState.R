###_ Calculate time and Market status and store in RV then render to UI to avoid blurring#####
###__ Get market open/closed text from yahoo finance home page and attach it to Sys.time() string and store in rv####

observeEvent(trgr60(), {
  statmsg <- tryCatch(read_html("http://www.msn.com/en-us/money/markets"), error = function(e) NA)
  if(!all(is.na(statmsg))) { 
    statmsg <- statmsg %>%
      html_nodes('.market-status-text') %>% 
      html_text()
    if (!length(statmsg)) {statmsg <-"Market Stats Not Available"}
  } else {statmsg <- "Market Stats Not Available"}

  RVs$timeNstats <- if(statmsg == "Market Stats Not Available" ) {
    
    tags$span(paste(statmsg), style = "color: lightgray;")
    
  } else  if (grepl("Markets open in|closed", statmsg, ignore.case = T)) {
    tags$span(icon("circle", class = "redO"),
              paste(
                format(Sys.time(), "%a, %b %d, %Y %I:%M %p %Z", tz = "EST")
                ,
                " (US Markets Closed)", #" (",statmsg,")",
                sep = ""
              ))
  } else {
    tags$span(icon("circle", class = "greenO"),
              paste(
                format(Sys.time(), "%a, %b %d, %Y %I:%M %p %Z", tz = "EST")
                ,
                " (US Markets Open)",  #" (",statmsg,")" ,
                sep = ""
              ))
  }
}, ignoreInit = F)

###__ Render timeNStats rv to UI ####
output$time <- renderUI({
  RVs$timeNstats
})