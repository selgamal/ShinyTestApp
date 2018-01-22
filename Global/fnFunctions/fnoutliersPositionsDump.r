outliersPositions <-
function(x) {
  qnt <- quantile(x, probs = c(.25,.75), na.rm = T)
  H <- 1.5 * IQR(x)
  pos <- sort(c(which(x < (qnt[1]-H)),which(x > (qnt[2]+H))))
  return(pos)
}
