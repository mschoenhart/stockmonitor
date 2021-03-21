#' Parse historical stock prices from ariva.de
#' A wrapper for getQuoteAriva
#' Example: data <- getQuotesAriva(c("eurostoxx-50-index", "euro_stoxx_50_volatility_vstoxx-index"), "1.1.2021")$Close
getQuotesAriva <-
  function(symbols = c("eurostoxx-50-index", "euro_stoxx_50_volatility_vstoxx-index"),
           start = "1.1.2021",
           end = NULL,
           what = "Close",
           symbolnames = NULL)
  {
    # create timeseries object
    ts <- xts()
    # merge all the security quotes together at the right dates
    for (s in symbols) {
      cat("Loading ",s,"\n")
      q <- getQuoteAriva(s, start, end)
      ts <- merge(ts, q[, what], all = T)
    }
    # optionally use your own symbol names
    if (is.null(symbolnames))
      names(ts) <- make.names(symbols)
    else
      names(ts) <- symbolnames
    return(ts)
  }
