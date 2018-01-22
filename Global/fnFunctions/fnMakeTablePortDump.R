

fnCreateMetricsTbl <- function(Ra, Rb, Rf = 0, p = 0.95) {
  tablex <-
    rbind(
      CAPM.beta(Ra = Ra, Rb = Rb, Rf = Rf),
      TreynorRatio(Ra = Ra, Rb = Rb, Rf = Rf),
      SharpeRatio(
        R = Ra,
        Rf = Rf,
        p = p,
        FUN = "StdDev"
      ),
      VaR(R = Ra, p = p, method = "modified"),
      CVaR(R = Ra, p = p, method = "modified"),
      matrix(
        colMeans(Ra),
        nrow = 1,
        dimnames = list("Mean", c(names(Ra)))
      ),
      StdDev(Ra)
    )
  
  tablex.df <-
    data.frame(
      Ticker = rownames(t(tablex)),
      t(tablex),
      check.names = F,
      row.names = NULL
    )
  
  names(tablex.df) <-
    c("Ticker",
      "Beta",
      "Treynor Ratio",
      "Sharpe Ratio",
      "VaR",
      "ES",
      "Mean",
      "StdDev")
  return(tablex.df)
}


