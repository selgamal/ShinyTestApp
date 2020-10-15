# Indexes value boxes ####
output$vbx_sp <-
  renderValueBox({
    shiny::validate(need(
      !is.na(RVs$Indxdta$changeNum1[1]),
      "Quote Data Not Available Currently"
    ))
    valueBox(
      subtitle = tagList(
        tags$span(RVs$Indxdta$Symbol[1],
                  id = "bxsym",
                  style = "display:none"),
        div(
          tags$span(
            style = "display: inline-block;width:65%;word-wrap: break-word;",
            tags$span(RVs$Indxdta$Name[1]),
            tags$br(),
            tags$span(RVs$Indxdta$`Trade Time`[1], style = "font-size:80%")
          )
          ,
          div(
            style = "position: relative;float: right;margin-top: 10px",
            tags$span("1 Day", style = "margin: 5px;font-size: 13px;float: right;") ,
            RVs$indxIntraday[[RVs$Indxdta$Symbol[1]]]$spark
          )
        )
      )
      ,
      value = HTML(paste(
        tags$span(RVs$Indxdta$Last[1], style = "display: inline-flex"),
        tags$span(RVs$Indxdta$xUpDn[1]
                  , style = if (RVs$Indxdta$changeNum1[1] >= 0) {
                    "color:greenyellow;font-size:60%;display: inline-block;"
                  } else {
                    "color:#ffb498;font-size:60%;display: inline-block;"
                  })
      ))
      ,
      color = if (RVs$Indxdta$changeNum1[1] >= 0) {
        "green"
      } else {
        "red"
      }
    )
  })     
output$vbx_nasdaq <-
  renderValueBox({
    shiny::validate(need(
      !is.na(RVs$Indxdta$changeNum1[2]),
      "Quote Data Not Available Currently"
    ))
    valueBox(
      subtitle = tagList(
        tags$span(RVs$Indxdta$Symbol[2],
                  id = "bxsym",
                  style = "display:none"),
        div(
          tags$span(
            style = "display: inline-block;width:65%;word-wrap: break-word;",
            tags$span(RVs$Indxdta$Name[2]),
            tags$br(),
            tags$span(RVs$Indxdta$`Trade Time`[2], style = "font-size:80%")
          )
          ,
          div(
            style = "position: relative;float: right;margin-top: 10px",
            tags$span("1 Day", style = "margin: 5px;font-size: 13px;float: right;") ,
            RVs$indxIntraday[[RVs$Indxdta$Symbol[2]]]$spark
          )
        )
      )
      ,
      value = HTML(paste(
        tags$span(RVs$Indxdta$Last[2], style = "display: inline-flex"),
        tags$span(RVs$Indxdta$xUpDn[2]
                  , style = if (RVs$Indxdta$changeNum1[2] >= 0) {
                    "color:greenyellow;font-size:60%;display: inline-block;"
                  } else {
                    "color:#ffb498;font-size:60%;display: inline-block;"
                  })
      ))
      ,
      color = if (RVs$Indxdta$changeNum1[2] >= 0) {
        "green"
      } else {
        "red"
      }
    )
  })
output$vbx_dji <-
  renderValueBox({
    shiny::validate(need(
      !is.na(RVs$Indxdta$changeNum1[4]),
      "Quote Data Not Available Currently"
    ))
    valueBox(
      subtitle = tagList(
        tags$span(RVs$Indxdta$Symbol[4],
                  id = "bxsym",
                  style = "display:none"),
        div(
          tags$span(
            style = "display: inline-block;width:60%;word-wrap: break-word;",
            tags$span(RVs$Indxdta$Name[4]),
            tags$br(),
            tags$span(RVs$Indxdta$`Trade Time`[4], style = "font-size:80%")
          )
          ,
          div(
            style = "position: relative;float: right;margin-top: 10px",
            tags$span("1 Day", style = "margin: 5px;font-size: 13px;float: right;") ,
            RVs$indxIntraday[[RVs$Indxdta$Symbol[4]]]$spark
          )
        )
      )
      ,
      value = HTML(paste(
        tags$span(RVs$Indxdta$Last[4], style = "display: inline-flex"),
        tags$span(RVs$Indxdta$xUpDn[4]
                  , style = if (RVs$Indxdta$changeNum1[4] >= 0) {
                    "color:greenyellow;font-size:60%;display: inline-block;"
                  } else {
                    "color:#ffb498;font-size:60%;display: inline-block;"
                  })
      ))
      ,
      color = if (RVs$Indxdta$changeNum1[4] >= 0) {
        "green"
      } else {
        "red"
      }
    )
  })

# Session Gainer and Session loser boxes ####

output$vbx_tkrgnr <-
  renderValueBox({
    suppressWarnings(shiny::validate(
      need(nrow(RVs$sessiongainer) > 0, 'Not Enough Data'),
      need(
        !is.na(RVs$sessiongainer$changeNum1[1]),
        "Quote Data Not Available Currently"
      )
    ))
    valueBox(
      subtitle = tagList(
        tags$span(RVs$sessiongainer$Symbol[1],
                  id = "bxsym",
                  style = "display:none"),
        div(
          tags$span(
            style = "display: inline-block;width:65%;word-wrap: break-word;",
            tags$span(RVs$sessiongainer$Name[1]),
            tags$br(),
            tags$span(RVs$sessiongainer$`Trade Time`[1], style = "font-size:80%")
          )
          ,
          div(
            style = "position: relative;float: right;margin-top: 10px",
            tags$span("1 Day", style = "margin: 5px;font-size: 13px;float: right;") ,
            RVs$tkrIntraday[[RVs$sessiongainer$Symbol[1]]]$spark
          )
        )
      )
      ,
      value = HTML(paste(
        tags$span(RVs$sessiongainer$Last[1], style = "display: inline-flex"),
        tags$span(RVs$sessiongainer$xUpDn[1], style = "color:greenyellow;font-size:60%;display:inline-block;")
      ))
      ,
      color = "green"
    )
  })

output$vbx_tkrlosr <-
  renderValueBox({
    suppressWarnings(shiny::validate(
      need(nrow(RVs$sessionloser) > 0, 'Not Enough Data'),
      need(
        !is.na(RVs$sessionloser$changeNum1[1]),
        "Quote Data Not Available Currently"
      )
    ))
    valueBox(
      subtitle = tagList(
        tags$span(RVs$sessionloser$Symbol[1],
                  id = "bxsym",
                  style = "display:none"),
        div(
          tags$span(
            style = "display: inline-block;width:65%;word-wrap: break-word;",
            tags$span(RVs$sessionloser$Name[1]),
            tags$br(),
            tags$span(RVs$sessionloser$`Trade Time`[1], style = "font-size:80%")
          )
          ,
          div(
            style = "position: relative;float: right;margin-top: 10px",
            tags$span("1 Day", style = "margin: 5px;font-size: 13px;float: right;") ,
            RVs$tkrIntraday[[RVs$sessionloser$Symbol[1]]]$spark
          )
        )
      )
      ,
      value = HTML(paste(
        tags$span(RVs$sessionloser$Last[1], style = "display: inline-flex;"),
        tags$span(RVs$sessionloser$xUpDn[1], style = "color:#ffb498;font-size:60%;display: inline-block;")
      ))
      ,
      color = "red"
    )
  })

outputOptions(output, "vbx_sp", suspendWhenHidden = FALSE)
outputOptions(output, "vbx_nasdaq", suspendWhenHidden = FALSE)
outputOptions(output, "vbx_dji", suspendWhenHidden = FALSE)
