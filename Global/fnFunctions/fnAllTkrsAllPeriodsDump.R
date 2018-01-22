fnAllTkrsAllPeriods <-
  function(x, type) {
    namesy <- c()
    y <- mapply(function (z) {
      namex <- switch(
        z,
        "^GSPC" = "S&P500",
        "^IXIC" = "NASDAQ",
        "^DJI" = "DJI",
        "SPY" = "SPY",
        z
      )
      namesy <<- c(namesy, namex)
      d.close <- x[[z]][[type]]$Close
      names(d.close) <- namex
      w.close <- to.weekly(d.close, OHLC = F)
      index(w.close) <-
        as.Date(ceiling_date(ymd(index(w.close)), unit = "week") - days(1)) # Last day of week (Sat)
      names(w.close) <- namex
      m.close <- to.monthly(d.close, OHLC = F)
      index(m.close) <-
        as.Date(as.yearmon(index(m.close))) + months(1) - days(1) # last day of month
      names(m.close) <- namex
      d.ret <- dailyReturn(d.close, leading = F)
      if (nrow(d.ret) > 1) {
        d.ret <- d.ret[-1, ]
      }
      names(d.ret) <- namex
      w.ret <- weeklyReturn(d.close, leading = F)
      if (nrow(w.ret) > 1) {
        w.ret <- w.ret[-1, ]
      }
      index(w.ret) <-
        as.Date(ceiling_date(ymd(index(w.ret)), unit = "week") - days(1)) # Last day of week (Sat)
      names(w.ret) <- namex
      m.ret <- monthlyReturn(d.close, leading = F)
      if (nrow(m.ret) > 1) {
        m.ret <- m.ret[-1, ]
      }
      index(m.ret) <-
        as.Date(as.yearmon(index(m.ret))) + months(1) - days(1) # last day of month
      names(m.ret) <- namex
      
      return(list(
        Closings = list(
          daily = d.close,
          weekly = w.close,
          monthly = m.close
        ),
        Rets = list(
          daily = d.ret,
          weekly = w.ret,
          monthly = m.ret
        )
      ))
    },
    z = names(x), SIMPLIFY = F)
    
    d.close <- do.call(merge.xts, sapply(sapply(y, '[', 1L), '[', 1L))
    names(d.close) <- namesy
    w.close <- do.call(merge.xts, sapply(sapply(y, '[', 1L), '[', 2L))
    names(w.close) <- namesy
    m.close <- do.call(merge.xts, sapply(sapply(y, '[', 1L), '[', 3L))
    names(m.close) <- namesy
    
    d.ret <- do.call(merge.xts, sapply(sapply(y, '[', 2L), '[', 1L))
    names(d.ret) <- namesy
    w.ret <- do.call(merge.xts, sapply(sapply(y, '[', 2L), '[', 2L))
    names(w.ret) <- namesy
    m.ret <- do.call(merge.xts, sapply(sapply(y, '[', 2L), '[', 3L))
    names(m.ret) <- namesy
    
    return(list(
      Closings = list(
        daily = d.close,
        weekly = w.close,
        monthly = m.close
      ),
      Rets = list(
        daily = d.ret,
        weekly = w.ret,
        monthly = m.ret
      )
    ))
  }
