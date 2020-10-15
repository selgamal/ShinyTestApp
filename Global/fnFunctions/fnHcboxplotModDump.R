hcboxplot.mod <-
  function (x = NULL, var = NULL, var2 = NULL, outliers = TRUE, 
            ...) 
  {
    stopifnot(is.numeric(x))
    if (is.null(var)) 
      var <- NA
    if (is.null(var2)) 
      var2 <- NA
    df <- data_frame(x, g1 = var, g2 = var2)
    get_box_values <- function(x) {
      boxplot.stats(x)$stats %>% t() %>% as.data.frame() %>% 
        setNames(c("low", "q1", "median", "q3", "high"))
    }
    get_outliers_values <- function(x) {
      boxplot.stats(x)$out
    }
    series_box_ <- df %>% group_by_("g1", "g2") %>% do(data = get_box_values(.$x))
      
      # browser()
    series_box <- series_box_ %>%   tidyr::unnest(data) %>% group_by_("g2") %>% do(data = list_parse(rename_(select_(., 
                                                                            "-g2"), name = "g1"))) %>% mutate(type = "boxplot") %>% 
      mutate_(id = "as.character(g2)")
    if (length(list(...)) > 0) 
      series_box <- highcharter:::add_arg_to_df(series_box, ...)
    series_out_ <- df %>% group_by_("g1", "g2") %>% do(data = get_outliers_values(.$x))
    
    series_out <- series_out_ %>% tidyr::unnest(data) %>% group_by_("g2") %>% do(data = list_parse(select_(., 
                                                                    name = "g1", y = "data"))) %>% mutate(type = "scatter") %>% 
      mutate(linkedTo = "as.character(g2)")
    if (length(list(...)) > 0) 
      series_out <- highcharter:::add_arg_to_df(series_out, ...)
    if (!rlang::has_name(list(...), "color")) {
      colors <- colorize(seq(1, nrow(series_box)))
      colors <- hex_to_rgba(colors, alpha = 0.75)
    }
    if (!rlang::has_name(list(...), "name")) {
      series_box <- rename_(series_box, name = "g2")
      series_out <- rename_(series_out, name = "g2")
    }
    hc <- highchart() %>% hc_chart(type = "bar") %>% hc_xAxis(type = "category") %>% 
      hc_plotOptions(series = list(marker = list(symbol = "circle")))
    hc <- hc_add_series_list(hc, list_parse(series_box))
    if (is.na(var2) || is.na(var)) {
      hc <- hc %>% hc_xAxis(categories = "") %>% hc_plotOptions(series = list(showInLegend = FALSE))
    }
    if (outliers && nrow(series_out)) 
      hc <- hc_add_series_list(hc, list_parse(series_out))
    hc
  }