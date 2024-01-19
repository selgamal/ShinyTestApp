GetSP500tkrs <-
function() {
  
  library(xml2)
  
  library(quantmod)
  
  library(stringr)
  
  url <-  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  
  xp <-   "//*[@id=\"mw-content-text\"]/div/table[1]" #"//*[@id=\"mw-content-text\"]/table[1]"
  
  x <- xml2::read_html(url) %>% html_nodes(xpath = xp) %>% html_table() %>% as.data.frame()
  
  x <-  sort(x[, 1])
  
  remove("url", "xp")
  return(x)
  
}
