#' Parse historical stock prices for one security from ariva.de
#' Example: data <- getQuoteAriva("eurostoxx-50-index", "1.1.2021")$Close
getQuoteAriva <-
  function(symbol = "eurostoxx-50-index",
           start = "1.1.2021",
           end = NULL)
  {
    # read and parse url
    url <-
      paste0("https://www.ariva.de/", symbol, "/historische_kurse")
    # https://stackoverflow.com/questions/23430547/htmlparse-fails-to-load-external-entity
    doc <-
      htmlParse(rawToChar(GET(url)$content), encoding = "UTF-8")
    
    #extract attributes for the stock for historic download
    atbts <- xpathSApply(doc, '//form[@name="histcsv"]')[[1]]
    security <-
      as.character(xmlGetAttr(xpathSApply(atbts, '//input[@name="list"]')[[1]], "value")) # former name="secu"
    boerseid <-
      as.character(xmlGetAttr(xpathSApply(atbts, '//input[@name="boerse_id"]')[[1]], "value"))
    
    # download the historic values into a data.frame
    res <-
      read.table(
        paste0(
          "https://www.ariva.de/quote/historic/historic.csv?list=",
          security,
          "&boerse_id=",
          boerseid,
          ifelse(is.null(start), "", paste0("&min_time=", start)),
          ifelse(is.null(end), "", paste0("&max_time=", end)),
          "&clean_split=1&clean_payout=0&clean_bezug=1",
          "&trenner=%3B&go=Download"
        ),
        stringsAsFactors = FALSE,
        sep = ";",
        header = TRUE,
        colClasses = "character"
      )
    # column names
    names(res) <-
      c("Date", "Open", "High", "Low", "Close", "Amount", "Volume")
    # convert comma , to .
    res$Open <-
      as.numeric(gsub(",", ".", gsub(".", "", res$Open, fixed = TRUE), fixed = TRUE))
    res$High <-
      as.numeric(gsub(",", ".", gsub(".", "", res$High, fixed = TRUE), fixed = TRUE))
    res$Low <-
      as.numeric(gsub(",", ".", gsub(".", "", res$Low, fixed = TRUE), fixed = TRUE))
    res$Close <-
      as.numeric(gsub(",", ".", gsub(".", "", res$Close, fixed = TRUE), fixed = TRUE))
    res$Amount <-
      as.numeric(gsub(",", ".", gsub(".", "", res$Amount, fixed = TRUE), fixed = TRUE))
    res$Volume <-
      as.numeric(gsub(",", ".", gsub(".", "", res$Volume, fixed = TRUE), fixed = TRUE))
    res$Date <- format(as.Date(res$Date))
    
    # create and return time series object
    ts <- as.xts(res[, -1], order.by = as.POSIXct(res[, 1]))
    return(ts)
  }
