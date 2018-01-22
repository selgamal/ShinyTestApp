# This plot acts as a color scale legend for the bubbles viz

output$barplot <-
  renderPlot({
    shiny::validate(need(nrow(RVs$tkrdta) > 1, ""))
    
    options(scipen = 999999)
    par(mar = c(3, 1, 1, 1) + .1, bg = "transparent")
    plot(
      x = c(0:6),
      y = rep(1, 7),
      type = 'n'
      ,
      bty = 'n'
      ,
      xaxt = 'n'
      ,
      xlab = ''
      ,
      yaxt = 'n'
      ,
      ylab = ''
      ,
      xaxs = "i"
      ,
      yaxs = "i"
      ,
      ylim = c(0, 1)
    )
    rect(
      xleft = c(0:6),
      ybottom = 0 ,
      xright = c(1:7),
      ytop = 1 ,
      col = colscaldf$colrs,
      border = NA
    )
    axis(
      mgp = c(0, 0.5, 0),
      cex.axis = .6,
      side = "1",
      col.ticks = "black",
      tck = -.2,
      c(0:6) ,
      las = 1,
      labels = paste(round(RVs$seq, 3), "%", sep = "")
    )
    title(
      "Change %",
      adj = 0,
      font.main = 1,
      cex.main = .8
    )
    title(
      sub = "Circle Size: Last Bid Size",
      cex.sub = .8,
      line = 2,
      adj = 0
    )
  })

# Bubbles Viz ####
output$bublz <-
  renderBubbles({
    shiny::validate(need(nrow(RVs$tkrdta) > 1, message = "PLEASE SELECT 2 OR MORE TICKRS"))
    RVs$bblz$bblSize <- ifelse(RVs$bblz$LastSizeNum == 0, 100, RVs$bblz$LastSizeNum*100) 
    
    bubbles(
      sqrt(abs(RVs$bblz$bblSize))
      ,
      RVs$bblz$Symbol
      ,
      key = RVs$bblz$Symbol
      ,
      tooltip = paste(
        RVs$bblz$Symbol, 
        "\n",
        "Last Trade Change: ",
        RVs$bblz$`% Change`,
        "\n",
        "Last Trade Price: ",
        RVs$bblz$Last,
        "\n",
        "Bid Size: ",
        RVs$bblz$`Last Size`,
        sep = ""
      )
      ,
      color = c(RVs$bblz$colrs)
      ,
      textColor = "white"
    )
    
  })
