### Server for portfolio tab section Two


### Intialize variables used later

# Holds stat msg
RVs$port_sectionTwo_statMsg <- NULL

# holds current optimized portfolio
RVs$OptimizedPort <- NULL

# holds portfolio to display in section two based on "select portfolio to display"
RVs$Port_sectionTwo_savedPorts <- NULL

#holds seclect port to display
RVs$port_sectionTwo_slctdport <- list()
RVs$port_sectionTwo_displayPortObs <- 0


output$port_setionTwo_RfNote <- renderUI({
  tags$span(
            style = "font-size: 10px;font-style: italic;", HTML("<i class=\"fa fa-info-circle\"></i>"),
            paste("Annual RF should be between 0 and ",  Max_Rf/100, " (max in yield curve table)", sep = "")
  )
})


#update risk free numeric input to be within the limits of yield curve table

updateNumericInput(
  session = session,
  inputId = "port_sectionTwo_Optim_slct_Rf",
  max = Max_Rf/100
)

updateNumericInput(
  session = session,
  inputId = "port_SectionTwo_perf_slct_Rf",
  max = Max_Rf/100
)



## Save box help, hint, validate and do save/delete 

RVs$port_sectionTwo_specs_saveBoxHelp <- ""
RVs$port_sectionTwo_specs_saveBoxHelp_style <- ""


observeEvent(
  input$port_sectionTwo_selectAction,
  {
    a <- switch(
      input$port_sectionTwo_selectAction,
      "1" = "<i class=\"fa fa-question-circle\"></i> To Save currently optimized portfolio, select \"Unsaved\" from dropdown list, input a name and click 'Save' (Existing portfolio can be re-saved under different names)",
      "2" = "<i class=\"fa fa-question-circle\"></i> To Delete an existing portfolio, select it from dropdownlist and click 'Delete'"
    )
    
    b <- switch(
      input$port_sectionTwo_selectAction,
      "1" = "color:'';",
      "2" = "color:red;"
    )
    
    RVs$port_sectionTwo_specs_saveBoxHelp <- a
    RVs$port_sectionTwo_specs_saveBoxHelp_style <- b
  }
)


output$port_sectionTwo_specs_saveBoxHelp <- renderUI(
  tags$span(
    HTML(RVs$port_sectionTwo_specs_saveBoxHelp),
    style = RVs$port_sectionTwo_specs_saveBoxHelp_style
  )
)

observeEvent(
  input$port_sectionTwo_selectAction,
  {
    
    switch(
      input$port_sectionTwo_selectAction,
      "1" = {
        updateSelectInput(
          session = session,
          inputId = "port_sectionTwo_specs_selectPort",
          choices = if(length(RVs$Port_sectionTwo_savedPorts$Unsaved)>0){"Unsaved"} else {NULL}
        )
        enable("port_sectionTwo_specs_enterName")
        updateActionButton(
          session = session,
          inputId = "port_sectionTwo_specs_doAction",
          icon = icon("floppy-o"),
          label = "Save"
        )
      },
      "2" = {
        updateTextInput(
          session = session,
          inputId = "port_sectionTwo_specs_enterName", 
          value = ""
        )
        updateSelectizeInput(
          session = session,
          inputId = "port_sectionTwo_specs_selectPort",
          choices = if(length(names(RVs$Port_sectionTwo_savedPorts)[names(RVs$Port_sectionTwo_savedPorts)!="Unsaved"])>0){
            names(RVs$Port_sectionTwo_savedPorts)[names(RVs$Port_sectionTwo_savedPorts) !="Unsaved"]
          } else {""},
          selected = ""
        )
        updateActionButton(
          session = session,
          inputId = "port_sectionTwo_specs_doAction",
          icon = icon("trash-o"),
          label = "Delete"
        )
        runjs(HTML("document.getElementById('port_sectionTwo_specs_enterName').style.backgroundColor =''"))
        disable("port_sectionTwo_specs_enterName")
      }
        
    )
  }
)

observeEvent(
  input$port_sectionTwo_specs_doAction,
  {
    port_sectionTwo_displayPortObs$suspend()
    if(input$port_sectionTwo_selectAction == 1) {
      if(nchar(input$port_sectionTwo_specs_selectPort)==0){
        runjs(HTML("$('#port_sectionTwo_specs_saveResults > div:nth-child(2) > div > div > div:nth-child(3) > div > div > div > div.selectize-input').css('background-color', 'tomato')"))

        RVs$port_sectionTwo_specs_saveBoxHelp <- "<i class=\"fa fa-exclamation-triangle\"></i> Optimize or select a portfolio to Save"
        RVs$port_sectionTwo_specs_saveBoxHelp_style <- "color:red;"
        
      } else {
        if(nchar(input$port_sectionTwo_specs_enterName)==0 || 
           input$port_sectionTwo_specs_enterName %in% names(RVs$Port_sectionTwo_savedPorts) ) {
          runjs(HTML("document.getElementById('port_sectionTwo_specs_enterName').style.backgroundColor ='tomato'"))

          RVs$port_sectionTwo_specs_saveBoxHelp <- "<i class=\"fa fa-exclamation-triangle\"></i> Enter a unique name to save portfolio"
          RVs$port_sectionTwo_specs_saveBoxHelp_style <- "color:red;"
          
        } else {
          if(length(names(RVs$Port_sectionTwo_savedPorts)[names(RVs$Port_sectionTwo_savedPorts)!="Unsaved"])>=10){

            RVs$port_sectionTwo_specs_saveBoxHelp <- "<i class=\"fa fa-exclamation-triangle\"></i> Only 10 Portfolios can be saved"
            RVs$port_sectionTwo_specs_saveBoxHelp_style <- "color:red;"
            
          } else {
            
            if(is.null(RVs$Port_sectionTwo_savedPorts[[input$port_sectionTwo_specs_selectPort]]$OptimRes)) {
              RVs$port_sectionTwo_specs_saveBoxHelp <- "<i class=\"fa fa-exclamation-triangle\"></i> No Data for this portfolio, please check inputs and re-try"
              RVs$port_sectionTwo_specs_saveBoxHelp_style <- "color:red;"
            } else {
            runjs(HTML("document.getElementById('port_sectionTwo_specs_enterName').style.backgroundColor =''"));
            runjs(HTML("$('#port_sectionTwo_specs_saveResults > div:nth-child(2) > div > div > div:nth-child(3) > div > div > div > div.selectize-input').css('background-color', '')"))
            SaveName <- input$port_sectionTwo_specs_enterName
            slcdPort <- input$port_sectionTwo_specs_selectPort
            RVs$Port_sectionTwo_savedPorts[[SaveName]] <- RVs$Port_sectionTwo_savedPorts[[slcdPort]]
            RVs$Port_sectionTwo_savedPorts[[SaveName]]$PortName <- SaveName
            if(slcdPort =="Unsaved") {
              RVs$Port_sectionTwo_savedPorts$Unsaved <- NULL
            }
            RVs$port_sectionTwo_specs_saveBoxHelp <- paste(SaveName, " Saved!", sep = "")
            RVs$port_sectionTwo_specs_saveBoxHelp <- paste("<i class=\"fa fa-info-circle\"></i>", " ", SaveName, " Saved!", sep = "")
            RVs$port_sectionTwo_specs_saveBoxHelp_style <- "color:'';"
            newChoices <- if("Unsaved" %in% names(RVs$Port_sectionTwo_savedPorts) ) {
              c("Unsaved", names(RVs$Port_sectionTwo_savedPorts)[names(RVs$Port_sectionTwo_savedPorts) !="Unsaved"])
            } else {names(RVs$Port_sectionTwo_savedPorts)}
            
            updateSelectInput(
              session = session,
              inputId = "port_sectionTwo_specs_slctSaved",
              choices = newChoices, 
              selected = newChoices[length(newChoices)] 
            )
            
            updateSelectizeInput(
              session = session,
              inputId = "port_sectionTwo_specs_selectPort",
              choices = newChoices, 
              selected = newChoices[length(newChoices)]
            )
            }
          }
          
        } 
      }
      
      
      
    } else {
      if (input$port_sectionTwo_selectAction == 2) {
        if(input$port_sectionTwo_specs_selectPort == "Unsaved" ||
           nchar(input$port_sectionTwo_specs_selectPort)==0) {
          runjs(HTML("$('#port_sectionTwo_specs_saveResults > div:nth-child(2) > div > div > div:nth-child(3) > div > div > div > div.selectize-input').css('background-color', 'tomato')"))
          RVs$port_sectionTwo_specs_saveBoxHelp <- "<i class=\"fa fa-exclamation-triangle\"></i> Select a portfolio to delete."
          RVs$port_sectionTwo_specs_saveBoxHelp_style <- "color:red;"
          
        } else {
          runjs(HTML("$('#port_sectionTwo_specs_saveResults > div:nth-child(2) > div > div > div:nth-child(3) > div > div > div > div.selectize-input').css('background-color', '')"))
          slcdPort <- input$port_sectionTwo_specs_selectPort
          
          RVs$Port_sectionTwo_savedPorts[slcdPort] <- NULL
          RVs$port_sectionTwo_specs_saveBoxHelp <- paste("<i class=\"fa fa-info-circle\"></i>", " ", slcdPort, " Deleted!", sep = "")
          RVs$port_sectionTwo_specs_saveBoxHelp_style <- "color:'red';"

          newChoices <- if("Unsaved" %in% names(RVs$Port_sectionTwo_savedPorts) ) {
            c("Unsaved", names(RVs$Port_sectionTwo_savedPorts)[names(RVs$Port_sectionTwo_savedPorts) !="Unsaved"])
          } else {names(RVs$Port_sectionTwo_savedPorts)} # Keep unsaved always first
          
          updateSelectInput(
            session = session,
            inputId = "port_sectionTwo_specs_slctSaved",
            choices = newChoices, 
            selected = newChoices[length(newChoices)] 
          )
          
          updateSelectizeInput(
            session = session,
            inputId = "port_sectionTwo_specs_selectPort",
            choices = newChoices[newChoices !="Unsaved"],
            selected = newChoices[length(newChoices)]
          )
      
        }
      }
    }
    port_sectionTwo_displayPortObs$resume() 
    
  }
  
)


# Portfolio Optimization section (Section Two)

## Enable disable upper and lower bounds of assets weights
observeEvent(
  {
    input$port_sectionTwo_Optim_UseMinMax
  },
  
  {
    if(input$port_sectionTwo_Optim_UseMinMax) {
      enable("port_sectionTwo_Optim_MaxWt")
      enable("port_sectionTwo_Optim_MinWt")
    } else {
      updateNumericInput(session = session,
                         inputId = "port_sectionTwo_Optim_MaxWt",
                         value = NA)
      updateNumericInput(session = session,
                         inputId = "port_sectionTwo_Optim_MinWt",
                         value = NA)
      runjs(HTML(
        "
        document.getElementById('port_sectionTwo_Optim_MaxWt').style.backgroundColor ='';
        document.getElementById('port_sectionTwo_Optim_MinWt').style.backgroundColor ='';
        if($('#portMaxMinContstraintText').next('div.tooltip:visible').length) {
          $('#portMaxMinContstraintText').click();
        }
        
        "
      ))
      disable("port_sectionTwo_Optim_MaxWt")
      disable("port_sectionTwo_Optim_MinWt")
      RVs$portOptimSpecs$port_optim_Minwt <-  NULL
      RVs$portOptimSpecs$port_optim_Maxwt <-  NULL
    }
  }
)


## Pulling inputs from Section One

observeEvent(
  {
    input$btn_PortSpecfill
  },
  {
    updateSelectizeInput(session = session, 
                         inputId = "port_sectionTwo_Optim_slctTkrs", 
                         selected = input$slct_CompareTkrSlct
    )
    updateSelectInput(session = session,
                      inputId = "port_sectionTwo_Optim_retfreq",
                      selected = input$slct_retfreq
    )
    updateDateRangeInput(session = session,
                         inputId = "port_sectionTwo_Optim_daterange",
                         start = RVs$port_Charts_Lims[1],
                         end = RVs$port_Charts_Lims[2]
    )
  }
)





## Alert user to click "Get optimal Portfolio" button when inputs change by changing button color

observeEvent(
  {
    input$port_sectionTwo_Optim_slctTkrs
    input$port_sectionTwo_Optim_daterange
    input$port_sectionTwo_Optim_retfreq
    input$port_sectionTwo_Optim_MaxWt
    input$port_sectionTwo_Optim_MinWt
    input$port_sectionTwo_Optim_slct_Rf
    RVs$portOptimSpecs
  },
  {
    updtInputLst <- list(
      port_optim_tkrs = input$port_sectionTwo_Optim_slctTkrs,
      port_optim_DateRange = input$port_sectionTwo_Optim_daterange,
      port_optim_ReturnFreq = input$port_sectionTwo_Optim_retfreq,
      port_optim_Maxwt = if(is.na(input$port_sectionTwo_Optim_MaxWt)) {NULL} else {input$port_sectionTwo_Optim_MaxWt},
      port_optim_Minwt = if(is.na(input$port_sectionTwo_Optim_MinWt)) {NULL} else {input$port_sectionTwo_Optim_MinWt},
      port_optim_Rf = input$port_sectionTwo_Optim_slct_Rf / switch(input$port_sectionTwo_Optim_retfreq,"1" = 252, "2" = 52,"3" = 12)
    )
    
    if(!identical(updtInputLst,RVs$portOptimSpecs)) {
      runjs(HTML("document.getElementById('port_sectionTwo_OptimBtn').style.backgroundColor = 'tomato'"))
    } else {
      runjs(HTML("document.getElementById('port_sectionTwo_OptimBtn').style.backgroundColor = ''"))
    }
  } #, ignoreInit = T
  
  
)

#Clean up when all tickers are removed

observeEvent(
  RVs$t,
  {
    RVs$port_optim_checkinputs <- c()
    RVs$portOptimSpecs <- NULL
  }
)

## On clicking "Get Optimal Portfolio" Validate inputs and return either a list of valid inputs or
## a vector with invalid inputs containing text for what is worng...

observeEvent({
  input$port_sectionTwo_OptimBtn
},
{
  RVs$port_sectionTwo_statMsg <- NULL
  RVs$OptimizedPort <- NULL
  RVs$port_optim_checkinputs <- c()
  RVs$portOptimSpecs <- list(
    port_optim_tkrs = c(), #input$port_sectionTwo_Optim_slctTkrs,
    port_optim_DateRange = input$port_sectionTwo_Optim_daterange,
    port_optim_ReturnFreq = input$port_sectionTwo_Optim_retfreq,
    port_optim_Maxwt = NULL,
    port_optim_Minwt = NULL,
    port_optim_Rf = 0
  )
  
  if(length(input$port_sectionTwo_Optim_slctTkrs)<2) {
    runjs(HTML("$('#PortOptim_Slctrs > div:nth-child(1) .selectize-input').css('background-color', 'rgb(250, 139, 119)')"))
    RVs$port_optim_checkinputs <- c(RVs$port_optim_checkinputs, "Tickers (Select 2 or more tickers)")
  } else {
    runjs(HTML("$('#PortOptim_Slctrs > div:nth-child(1) .selectize-input').css('background-color', '')"))
    
    RVs$portOptimSpecs$port_optim_tkrs <- sort(input$port_sectionTwo_Optim_slctTkrs)
  }
  
  if(input$port_sectionTwo_Optim_daterange[1] > input$port_sectionTwo_Optim_daterange[2] ||
     (input$port_sectionTwo_Optim_daterange[2] - input$port_sectionTwo_Optim_daterange[1])<2) {
    runjs(HTML("$('#port_sectionTwo_Optim_daterange > div > input').css('background-color', 'rgb(250, 139, 119)')"))
    RVs$port_optim_checkinputs <- c(RVs$port_optim_checkinputs, "Date Range (From Date must be before To Date and must yield more than one period of returns according to selected periodicity)")
  } else {
    runjs(HTML("$('#port_sectionTwo_Optim_daterange > div > input').css('background-color', 'white')"))
    
    RVs$portOptimSpecs$port_optim_DateRange <- input$port_sectionTwo_Optim_daterange
  }

  
  if(input$port_sectionTwo_Optim_UseMinMax) {
    opt_maxwt <- input$port_sectionTwo_Optim_MaxWt
    if(is.na(opt_maxwt)) {
    } else {
      if (!is.na(opt_maxwt) &&
          opt_maxwt > 1 ||
          opt_maxwt < 0 ||
          (
            length(input$port_sectionTwo_Optim_slctTkrs) * opt_maxwt
          ) <= 1) {
        runjs(
          HTML(
            "$('#port_sectionTwo_Optim_MaxWt').css('background-color', 'rgb(250, 139, 119)')"
          )
        )
        
        RVs$port_optim_checkinputs <- c(RVs$port_optim_checkinputs, "Max Weight")
        #RVs$portOptimSpecs$port_optim_Maxwt <-  NULL
      } else {
        runjs(HTML(
          "$('#port_sectionTwo_Optim_MaxWt').css('background-color', '')"
        ))
        RVs$portOptimSpecs$port_optim_Maxwt <-  opt_maxwt
      }
    }
    
    opt_minwt <- input$port_sectionTwo_Optim_MinWt
    if(is.na(opt_minwt)) {
    } else {
      if (!is.na(opt_minwt) &&
        opt_minwt > 1 ||
        opt_minwt < 0 ||
        (
          length(input$port_sectionTwo_Optim_slctTkrs) * opt_minwt
        ) >= 1) {
        runjs(
          HTML(
            "$('#port_sectionTwo_Optim_MinWt').css('background-color', 'rgb(250, 139, 119)')"
          )
        )
        RVs$port_optim_checkinputs <- c(RVs$port_optim_checkinputs, "Min Weight")
      } else {
        runjs(HTML(
          "$('#port_sectionTwo_Optim_MinWt').css('background-color', '')"
        ))
        RVs$portOptimSpecs$port_optim_Minwt <-  opt_minwt
      }
    }
    
    if(!anyNA(c(opt_minwt, opt_maxwt))) {
      if(opt_minwt>opt_maxwt) {
        runjs(
          HTML(
            "$('#port_sectionTwo_Optim_MinWt').css('background-color', 'rgb(250, 139, 119)');
             $('#port_sectionTwo_Optim_MaxWt').css('background-color', 'rgb(250, 139, 119)')
            "
          )
        )
        RVs$port_optim_checkinputs <- c(RVs$port_optim_checkinputs, "Min Weight > Max weight!")
      }
    }
    
    runjs(
      HTML(
        "
        if(!$('#portMaxMinContstraintText').next('div.tooltip:visible').length) {
        if(document.getElementById('port_sectionTwo_Optim_MaxWt').style.backgroundColor =='rgb(250, 139, 119)' ||
        document.getElementById('port_sectionTwo_Optim_MinWt').style.backgroundColor =='rgb(250, 139, 119)'
        ) {
        $('#portMaxMinContstraintText').click();
        };
        } else {
        if($('#portMaxMinContstraintText').next('div.tooltip:visible').length) {
        if(document.getElementById('port_sectionTwo_Optim_MaxWt').style.backgroundColor =='' &&
        document.getElementById('port_sectionTwo_Optim_MinWt').style.backgroundColor ==''
        ) {
        $('#portMaxMinContstraintText').click();
        }
        
        };
        }
        
        "
      )
      )
  }

  
  
  if(is.na(input$port_sectionTwo_Optim_slct_Rf) || input$port_sectionTwo_Optim_slct_Rf > Max_Rf/100 || input$port_sectionTwo_Optim_slct_Rf < 0 ) {
    runjs(HTML("
               
               if(!$('#port_Optim_Rf_title').next('div.tooltip:visible').length) {
               $('#port_Optim_Rf_title').click();}"))
    runjs(
      HTML(
        "$('#port_sectionTwo_Optim_slct_Rf').css('background-color', 'rgb(250, 139, 119)')"
      )
    )
    #RVs$portOptimSpecs$port_optim_Rf <- 0
    RVs$port_optim_checkinputs <- c(RVs$port_optim_checkinputs, "Risk Free")

  } else {
    runjs(HTML(
      "$('#port_sectionTwo_Optim_slct_Rf').css('background-color', '')"
    ))
    
    Rf.freq <- switch(
      input$port_sectionTwo_Optim_retfreq,
      "1" = 252,
      "2" = 52,
      "3" = 12
    )
    RVs$portOptimSpecs$port_optim_Rf <- input$port_sectionTwo_Optim_slct_Rf / Rf.freq
    
  }
  
  # If all input validated (no erros stored in RVs$port_optim_checkinputs) Get rets of selected tickers 
  # based on portfolio specs (tickers, dates and periodicity are the only relevant inputs in this step)
  
  if (!length(RVs$port_optim_checkinputs)) {
    datRange <- paste(RVs$portOptimSpecs$port_optim_DateRange, collapse = "/")
    
    RVs$Port_Optim_asstRets <- RVs$rets_assts_allperiods$Rets[[as.numeric(RVs$portOptimSpecs$port_optim_ReturnFreq)]][,RVs$portOptimSpecs$port_optim_tkrs][datRange]
    
    RVs$OptimizedPort <- fnEfficientFrontier_tseries(
      Rets = RVs$Port_Optim_asstRets,
      Rf = RVs$portOptimSpecs$port_optim_Rf,
      MinWt = RVs$portOptimSpecs$port_optim_Minwt,
      MaxWt = RVs$portOptimSpecs$port_optim_Maxwt,
      Npoints = 500,
      TrgtRetsLevels = NULL
    )
  }
  
  # Show stats of selected specs and input validation
  RVs$port_sectionTwo_statMsg <-  
    if (length(RVs$port_optim_checkinputs)>0) {
      #runjs(HTML("document.getElementById('port_sectionTwo_OptimStat').style.color = ''"))
      c("Review following and re-try:\n *", paste(RVs$port_optim_checkinputs, collapse = "\n * "))
    } else {
      if(length(RVs$OptimizedPort)>0) {
        #runjs(HTML("document.getElementById('port_sectionTwo_OptimStat').style.color = ''"))
        freq_stat <- switch(RVs$portOptimSpecs$port_optim_ReturnFreq,
                            "1"="Daily Returns",
                            "2"="Weekly Returns",
                            "3"="Monthly Returns")
        optim_wts <- RVs$OptimizedPort$`Optimal Portfolio`$`Weights Allocation`
        optim_wts <- paste(optim_wts$symbol, optim_wts$wt, sep = "=", collapse = " ")
        minRisk_wts <- RVs$OptimizedPort$`Min. Risk Portfolio`$`Weights Allocation`
        minRisk_wts <- paste(minRisk_wts$symbol, minRisk_wts$wt, sep = "=", collapse = " ")
        
        c(
          "Successful for ", nrow(RVs$OptimizedPort$`Efficient Frontier Points`), 
          " return levels"," ", "out of ", RVs$OptimizedPort$`Number of Points Used`, " between ", 
          round(min(RVs$OptimizedPort$`Efficient Frontier Points`$retLevel),5),
          " and ", 
          round(max(RVs$OptimizedPort$`Efficient Frontier Points`$retLevel),5), "\n",
          "Assets: ","[", length(RVs$portOptimSpecs$port_optim_tkrs) ,"] ", paste(RVs$portOptimSpecs$port_optim_tkrs, collapse = " "), "\n",
          "Returns Date Range and Frequency: From ",
          paste(min(RVs$portOptimSpecs$port_optim_DateRange)), 
          " to ", 
          paste(max(RVs$portOptimSpecs$port_optim_DateRange)), " ",
          freq_stat, "\n",
          "Risk Free Rate: ", round(RVs$portOptimSpecs$port_optim_Rf,4)," " ,gsub(" Returns","",freq_stat), " (",
          "Annual = ",input$port_sectionTwo_Optim_slct_Rf,")" ,"\n",
          "Max Weight: ", if(is.null(RVs$portOptimSpecs$port_optim_Maxwt)) {NA} else {RVs$portOptimSpecs$port_optim_Maxwt}, "\n",
          "Min Weight: ", if(is.null(RVs$portOptimSpecs$port_optim_Minwt)) {NA} else {RVs$portOptimSpecs$port_optim_Minwt}, "\n",
          "Optimal Portfolio Weights: ", optim_wts, "\n",
          "Min. Risk Portfolio Weights: ", minRisk_wts
        )
      }
    }
  ## store results in displayed portfolio as unsaved
  RVs$Port_sectionTwo_savedPorts$Unsaved <- list(
    OptimRes = RVs$OptimizedPort,
    Stats = RVs$port_sectionTwo_statMsg,
    PortName = "Unsaved"
  )
  
  ## update select input to display currently optimized portfolio
  
  newChoices <- if("Unsaved" %in% names(RVs$Port_sectionTwo_savedPorts) ) {
    c("Unsaved", names(RVs$Port_sectionTwo_savedPorts)[names(RVs$Port_sectionTwo_savedPorts) !="Unsaved"])
  } else {names(RVs$Port_sectionTwo_savedPorts)} # Keep Unsaved as first name always
  
  updateSelectInput(
    session = session,
    inputId = "port_sectionTwo_specs_slctSaved",
    choices = newChoices,
    selected = "Unsaved"
  )
  
  updateSelectInput(
    session = session,
    inputId = "port_sectionTwo_selectAction",
    selected = 1
  )
  
  updateSelectizeInput(
    session = session,
    inputId = "port_sectionTwo_specs_selectPort",
    choices = newChoices,
    selected = "Unsaved"
  )
  
  })

output$port_sectionTwo_NumSaved <- renderText({
  a <- length(names(RVs$Port_sectionTwo_savedPorts)[names(RVs$Port_sectionTwo_savedPorts)!="Unsaved"])
  a <- if(a<=0) {0} else {a}
  return(a)
})

### using intermidiate variable to spit out selected portfolio to avoid rendering output twice (when selection changes and when dataset changes)

port_sectionTwo_displayPortObs <- observeEvent(
  {
    input$port_sectionTwo_specs_slctSaved
     RVs$Port_sectionTwo_savedPorts
  },
  {

    RVs$port_sectionTwo_slctdport <- RVs$Port_sectionTwo_savedPorts[[input$port_sectionTwo_specs_slctSaved]]
  }
)


observeEvent(
  RVs$port_sectionTwo_slctdport,
  {
    if(is.null(RVs$port_sectionTwo_slctdport$OptimRes)) {
      hideElement("port_SectionTwo_ChartsRowTwo")
      hideElement("port_SectionTwo_ChartsRowOne")
    } else {
      showElement("port_SectionTwo_ChartsRowTwo")
      showElement("port_SectionTwo_ChartsRowOne")
    }
  }
)


output$port_sectionTwo_OptimStat <- renderPrint({
  if(is.null(RVs$port_sectionTwo_slctdport$Stats)) { 
  runjs(HTML("document.getElementById('port_sectionTwo_OptimStat').style.color = 'rgba(128, 128, 128, 0.81)'"))
  cat("Specs of successfuly optimized portfolio appear here...")
  } else {
    if(grepl(
      "Successful",
      RVs$port_sectionTwo_slctdport$Stats[1]
    )) {
      runjs(HTML("document.getElementById('port_sectionTwo_OptimStat').style.color = ''"))
      cat(
        RVs$port_sectionTwo_slctdport$Stats[-c(1,4,5, 6)]
        , sep = "") 
    } else {
      runjs(HTML("document.getElementById('port_sectionTwo_OptimStat').style.color = 'rgba(128, 128, 128, 0.81)'"))
      cat("Specs of successfuly optimized portfolio appear here...")
    }
  }
})

output$port_sectionTwo_OptimResultMsg <- renderPrint({
  if(is.null(RVs$port_sectionTwo_slctdport$Stats)) { 
    runjs(HTML("document.getElementById('port_sectionTwo_OptimResultMsg').style.color = 'rgba(128, 128, 128, 0.81)'"))
    cat("Optimization Results/Errors appear here...")
  } else {
    runjs(HTML("document.getElementById('port_sectionTwo_OptimResultMsg').style.color = ''"))
    cat(
      head(RVs$port_sectionTwo_slctdport$Stats,10)
      , sep = "")
  }
})


### Portfolio Efficient frontier

output$port_SectionTwo_Eff <- renderHighchart({
  portdta <- RVs$port_sectionTwo_slctdport
  a <- portdta$OptimRes
  shiny::validate(need(
    nrow( a$`Efficient Frontier Points`
      #RVs$OptimizedPort$`Efficient Frontier Points`
         )>0, "NO DATA"))
  
  ports <-
    rbind.data.frame(
      a$`Optimal Portfolio`$`Solver Result`[,c("stdDev","expReturn")],
      a$`Min. Risk Portfolio`$`Solver Result`[,c("stdDev","expReturn")],
      a$`Equal Weights Portfolio`$`Solver Result`[c("stdDev","expReturn")],
      make.row.names = F
    )
  
  ports$Port <- c("Optimal Port.", "Min. Risk Port.", "Equal Weights Port." )
  
  ports <- na.omit(ports)
  ports$stdDev <- as.numeric(ports$stdDev)
  ports$expReturn <- as.numeric(ports$expReturn)
  
  
  highchart() %>%
    hc_add_series(
      data = a$`Efficient Frontier Points`[,c("stdDev","expReturn")],
        #RVs$OptimizedPort$`Efficient Frontier Points`[,c("stdDev","expReturn")],
      hcaes( x = stdDev*100, y = expReturn*100),
      type = "spline",
      name = "Eff Frontier",
      tooltip = list(
        headerFormat = "{series.name}<br>",
        pointFormat = "
        <span style=\"color:{point.color}\">\u25CF</span> Risk: <b>{point.x:,.2f}%</b><br/>
        <span style=\"color:{point.color}\">\u25CF</span> Return: <b>{point.y:,.2f}%</b><br/>
        
        "
      )
    ) %>%
    hc_add_series(
      data = ports,
      hcaes( x = stdDev*100, y = expReturn*100, group = Port),
      type = "scatter",
      dataLabels = list(enabled = T, format = "{point.Port}", style = list(textOutline = '0px')),
      marker = list(symbol ="triangle-down"),
      zIndex = 1,
      tooltip = list(
        headerFormat = "{series.name}<br>",
        pointFormat = "
        <span style=\"color:{point.color}\">\u25CF</span> Risk: <b>{point.x:,.2f}%</b><br/>
        <span style=\"color:{point.color}\">\u25CF</span> Return: <b>{point.y:,.2f}%</b><br/>
        
        "
      )
      ) %>%
    hc_add_series(
      data = a$`Individual Risk And Return`, # RVs$OptimizedPort$`Individual Risk And Return`,
      hcaes( x = Risk*100, y = Return*100, y2 = symbol, y3 = wtsOptim*100, y4 = wtsMinrsk*100),
      type = "scatter",
      name = "Individual Stocks",
      #dataLabels = list(enabled = T, format = "{point.y2}", style = list(textOutline = '0px')),
      marker = list(symbol = "cross", fillColor = "red", lineWidth ='1px'),
      tooltip = list(
        headerFormat = "",
        pointFormat = "
      </span><b>{point.y2}</b><br/>
      <span style=\"color:{point.color}\">\u25CF</span> Risk: <b>{point.x:,.2f}%</b><br/>
      <span style=\"color:{point.color}\">\u25CF</span> Return: <b>{point.y:,.2f}%</b><br/>
      <span style=\"color:{point.color}\">\u25CF</span> Wt. Optimal Port.: <b>{point.y3:,.2f}%</b><br/>
      <span style=\"color:{point.color}\">\u25CF</span> Wt. Min. Risk Port.: <b>{point.y4:,.2f}%</b><br/>
      "
      )
      
    ) %>%
    hc_add_series(
      data = a$`Capital Allocation Line`, #RVs$OptimizedPort$`Capital Allocation Line`,
      hcaes( x = Risk*100, y = Return*100),
      type = "line",
      name = "CAL",
      marker = list(enabled = F),
      zoneAxis = 'x',
      #dashStyle = "shortdot",
      zones = list(list(value = a$`Optimal Portfolio`$`Solver Result`$stdDev*100, #RVs$OptimizedPort$`Optimal Portfolio`$`Solver Result`$stdDev*100, 
                        dashStyle = "Solid"),
                   list(dashStyle = "shortdot")
      ),
      tooltip = list(
        headerFormat = "{series.name}<br>",
        pointFormat = "
        <span style=\"color:{point.color}\">\u25CF</span> Risk: <b>{point.x:,.2f}%</b><br/>
        <span style=\"color:{point.color}\">\u25CF</span> Return: <b>{point.y:,.2f}%</b><br/>
        
        "
      )
    ) %>% 
    hc_yAxis(
      labels = list(style = list(fontSize = "8px"),
                    format = '{value:,.2f}%'),
      title = list(text = "Returns", style = list(`font-size`="9px"))
    ) %>%
    hc_xAxis(
      labels = list(style = list(fontSize = "8px"),
                    format = '{value:,.2f}%'),
      title = list(text = "Risk (StDev. Returns)", style = list(`font-size`="9px"))
    ) %>%
    hc_legend(itemStyle = list(fontSize = "9px"))%>%
    hc_add_theme(thm) %>%
    hc_subtitle(
      text = portdta$PortName
    )
  
  
})

### Portfolio Pie Chart

output$port_SectionTwo_Pie <- renderHighchart({
  shiny::validate(need(nrow(
    RVs$port_sectionTwo_slctdport$OptimRes$`Optimal Portfolio`$`Weights Allocation`
    )>0, "NO DATA"))
  input$port_sectionTwo_pie_slct
  portdta <- RVs$port_sectionTwo_slctdport
  a <- portdta$OptimRes
  a <- switch(
    input$port_sectionTwo_pie_slct,
    "1" = a$`Optimal Portfolio`$`Weights Allocation`,
    "2" = a$`Min. Risk Portfolio`$`Weights Allocation`
  ) 
  shiny::validate(need(nrow(a)>0, "NO DATA"))
  names(a) <- c("name", "y" )
  x <- dplyr::filter(a, y>0)
  z <-dplyr::filter(a, y==0)

  highchart() %>%
    hc_chart(type = "pie") %>% 
    hc_add_series(data = x,
                  name = "Portfolio",
                  allowPointSelect = T,
                  cursor =  'pointer',
                  size = if(nrow(z)){"50%"} else {"70%"},
                  center = if(nrow(z)){c("50%", "40%")} else {c("50%", "50%")}
    ) %>%
    hc_add_series(data = z, visible = F, showInLegend = if(nrow(z)){T} else {F}
    ) %>%
    hc_plotOptions(
      pie = list(
        dataLabels = list(
          enabled = T,
          format = "<span style=\"color:{point.color}\">{point.name}:{percentage:,.2f}%</span>",
          style = list(textOutline = '0px')
        ),
        showInLegend = F,
        colors = c('#7cb5ec', '#90ed7d', '#f7a35c', '#7b7b84', '#8085e9', 
                   '#f15c80', '#e4d354', '#2b908f', '#f45b5b', '#91e8e1')
      )
    ) %>%
    hc_tooltip(
      headerFormat = "",
      pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {point.name}: <b>{point.percentage:,.2f}%"
    ) %>%
    hc_legend(
      enabled = T,
      floating= T,
      title =if(nrow(z)) {
        list(text = "Zero Weights:", style = list(`font-size` = '9px', color = "red"))
        } else {""},
      symbolHeight= 0,
      symbolWidth= 0,
      symbolRadius= 0,
      squareSymbol= F,
      itemStyle = list(`font-size` = '9px', cursor = 'default'),
      backgroundColor= '#e3e3e3',
      itemHiddenStyle = list(color = '')
    ) %>%
    hc_subtitle(
      text = portdta$PortName
    )
  
  
  
})

source("Server/PortfolionTabServer_SectionTwo_PortPerformance.R", local = T)$value