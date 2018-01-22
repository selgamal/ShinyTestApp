


# Render Data Table ####

output$CompaniesList <- DT::renderDataTable({
  req(UsePreLoadedTkrData)
  tbl <- Preloaded_TableOCompanies
  datatable(
    tbl,
    class = "compact stripe",
    options = list(
      paging = FALSE,
      scrollY = "400px",
      scrollX = F,
      columnDefs = list(list(
        className = 'dt-head-left', targets = 0:4
      ))
    ),
    selection = "multiple"
  )
  
}, server = F)


output$selectedCompanies <- renderPrint({
  x <-
    Preloaded_TableOCompanies$Ticker[input$CompaniesList_rows_selected]
  req(length(x) > 0)
  x <- split(x, ceiling(seq_along(x) / 15))
  x <- mapply(append, x, "\n", SIMPLIFY = F)
  x$`1` <- c("", x$`1`)
  do.call(cat, x)
  
})

observeEvent(input$addComp,
             {
               t <- input$input_tkr
               t <- unique(c(t, Preloaded_TableOCompanies$Ticker[input$CompaniesList_rows_selected]))
               updateSelectizeInput(session = session,
                                    inputId = "input_tkr",
                                    selected = t)
             })


observeEvent({
  input$btn_CompanyList_help
},
toggleModal(
  session = session,
  modalId = "Companiestable",
  toggle = "open"
))

observeEvent({
  input$btn_CompanyList
},
toggleModal(
  session = session,
  modalId = "Companiestable",
  toggle = "open"
))
