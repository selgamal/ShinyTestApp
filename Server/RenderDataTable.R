
# JS string for column def for sparklines
coldef <-
  "function(data, type, full){return '<span class=spark>' + data + '</span>'  }"

# JS for draw callback for sparklines
drCallback <-
  "function (oSettings, json) {$('.spark:not(:has(canvas))').sparkline('html', {type: 'line', zeroColor: 'black', width:'50px'});}"

# Render Data Table ####


output$rttable <- DT::renderDataTable({
  DT::datatable(
    RVs$dtable[, c(1:4, 14, 22, 6:7, 8, 23, 9, 24, 10, 11, 19:20, 26:30, 33,34)]
    ,
    options = list(
      fnDrawCallback = JS(drCallback),
      autoWidth = F,
      columnDefs = list(
        list(className = 'dt-center', targets = c(2:22))
        ,
        list(width = '70px', targets = 0)
        ,
        list(
          visible = FALSE,
          targets = c(13, 14, 16, 17, 18, 19, 20,21)
        )
        ,
        list(width = '40px', targets = 5)
        ,
        list(iDataSort = 16, targets = 5)
        ,
        list(iDataSort = 17, targets = 9)
        ,
        list(iDataSort = 18, targets = 11)
        ,
        list(iDataSort = 21, targets = 12)
        ,
        list(iDataSort = 20, targets = 3)
        ,
        list(iDataSort = 19, targets = 4)
        ,
        list(orderable = F, targets = 22)
        ,
        list(render = DT::JS(coldef), targets = 22)
        ,
        list(width = '60px', targets = 22)
        ,
        list(width = '150px', targets = 1)
        ,
        list(width = '15px', targets = 2)
      )
      ,
      rowCallback = DT::JS(
        'function(row, data) {
        if (data[5].includes("+")) {
        $("td:eq(5)", row).css({"color": "green", "font-weight": "bold"});
        } else {
        $("td:eq(5)", row).css({"color": "red", "font-weight": "bold"});}
        
        if (data[9].includes("+")) {
        $("td:eq(9)", row).css({"color": "green", "font-weight": "bold"});
        } else {
        $("td:eq(9)", row).css({"color": "red", "font-weight": "bold"});}
        
        if (data[11].includes("+")) {
        $("td:eq(11)", row).css({"color": "green", "font-weight": "bold"});
        } else {
        $("td:eq(11)", row).css({"color": "red", "font-weight": "bold"});}
        }'
        )
      ,
      paging = FALSE
      ,
      fixedHeader = TRUE
      ,
      search = list(search = tablesearch)
      
      )
    ,
    rownames = FALSE
    
    ,
    selection = list(
      mode = "multiple",
      target = "row",
      selected = previousSelection
    )
    )
  })
