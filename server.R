#
# packages
#
library(timeSeries)
library(zoo)
library(xts)
library(httr)
library(XML)
library(PerformanceAnalytics) # Geltner Returns
library(stringr)

#
# external functions for scraping
#
source("getQuoteAriva.R")
source("getQuotesAriva.R")

#
# shiny Server Functionality
#
shinyServer(function(input, output, clientData, session)
{
  #
  # init gui
  #
  # read security ticker names
  securitydata <-
    read.csv(SECURITYFILE,
             colClasses = "character",
             fileEncoding = "UTF-8") # security ticker names
  # update checkbox
  updateSelectInput(session, "benchmarkselectinput", choices = securitydata$name)
  updateCheckboxGroupInput(session,
                           "securitycheckbox",
                           choices = securitydata$name,
                           selected = securitydata$name[1:length(securitydata$name)])
  #
  # internal data structure
  #
  rv <-
    reactiveValues() # save reactive data like price data
  rv$scaling <- SCALING # scaling of financial data for stats
  rv$pricedata <- xts() # empty timeseries
  # init price data
  withProgress(message = 'Loading financial data', value = 1,
               {
                 if (file.exists(DATAFILE))
                   data <- readRDS(DATAFILE)
                 
                 datediff <-
                   ifelse(format(Sys.Date(), "%u") == "7", 2, 1)
                 if (!file.exists(DATAFILE) ||
                     (format(max(time(data)), "%Y-%m-%d") < format(Sys.Date() - datediff, "%Y-%m-%d")) ||
                     (format(min(time(data)), "%Y-%m-%d") > format(as.Date(DATASTARTDATE), "%Y-%m-%d"))) {
                   cat("Loading data from Ariva\n")
                   data <-
                     getQuotesAriva(securitydata$ticker,
                                    DATASTARTDATE,
                                    symbolnames = make.names(securitydata$name))
                   saveRDS(data, file = DATAFILE)
                 }
                 rv$priceData <- data
               })
  #
  # reactive functions
  #
  returnData <- reactive({
    idx <-
      format(time(rv$priceData), "%Y-%m-%d") >= input$daterange[1] &
      format(time(rv$priceData), "%Y-%m-%d") <= input$daterange[2]
    if (length(rv$priceData[idx,]) > 0) {
      #log returns
      #data <- diff(log(na.locf(rv$priceData, na.rm = T)), na.pad = F) # Last Observation Carried Forward for prices
      #simple returns
      data <-
        simpleret(na.locf(rv$priceData[idx,], na.rm = F)) # Last Observation Carried Forward for prices
    }
    else
      data <- xts()
    return(data)
  })
  #
  #output functions
  #
  # chart
  output$chartplot <- renderDygraph({
    # check for empty timeseries and input selections
    if ((length(returnData()) > 0) &&
        (!is.null(input$securitycheckbox)) &&
        (input$benchmarkselectinput != "")) {
      # timeseries starting point and values
      sp <- returnData()[1,]
      sp[1,] <- INDEXSTARTVALUE
      time(sp) <- time(sp) - 60 * 60 * 24 # 1 day before
      data <- rbind(sp, cumprod(1 + returnData()) * 100)
      selection <- make.names(unique(c(
        input$benchmarkselectinput, input$securitycheckbox
      )))
      bm <- make.names(input$benchmarkselectinput)
      
      dygraph(data[, selection]) %>%
        dySeries(bm, strokeWidth = 10) %>%
        dyRangeSelector() %>%
        dyAxis("y", valueRange = c(min(data), max(data) * 1.1)) %>%
        dyLegend(show = "always", labelsSeparateLines = T) %>%
        dyOptions(
          fillGraph = T,
          fillAlpha = 0.1,
          strokeWidth = 2
        ) %>%
        dyHighlight(
          highlightCircleSize = 4,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = T,
          highlightSeriesOpts = list(strokeWidth = 4)
        )
    }
  })
  # chart
  output$benchmarkplot <- renderDygraph({
    # not empty timeseries and input selections
    if ((length(returnData()) > 0) &&
        (!is.null(input$securitycheckbox)) &&
        (input$benchmarkselectinput != "")) {
      # timeseries starting point and values
      sp <- returnData()[1,]
      sp[1,] <- INDEXSTARTVALUE
      time(sp) <- time(sp) - 60 * 60 * 24 # 1 day before
      data <- rbind(sp, cumprod(1 + returnData()) * 100)
      selection <- make.names(unique(c(
        input$benchmarkselectinput, input$securitycheckbox
      )))
      bm <- make.names(input$benchmarkselectinput)
      data <- data / as.vector(data[, bm]) - 1
      
      dygraph(data[, selection]) %>%
        dySeries(bm, strokeWidth = 10) %>%
        dyRangeSelector() %>%
        dyAxis("y", valueRange = c(min(data), max(data) * 1.1)) %>%
        dyLegend(show = "always", labelsSeparateLines = T) %>%
        dyOptions(
          fillGraph = T,
          fillAlpha = 0.1,
          strokeWidth = 2
        ) %>%
        dyHighlight(
          highlightCircleSize = 4,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = T,
          highlightSeriesOpts = list(strokeWidth = 4)
        )
    }
  })
  # stats
  output$stats <- renderDataTable({
    idx <-
      format(time(rv$priceData), "%Y-%m-%d") >= input$daterange[1] &
      format(time(rv$priceData), "%Y-%m-%d") <= input$daterange[2]
    if (length(rv$priceData[idx,]) > 0) {
      datatable(
        calcstats(rv$priceData[idx,], rv$scaling),
        extensions = c("Buttons", "FixedColumns", "Scroller"),
        options = list(
          searching = F,
          # FixedColumns
          scrollX = T,
          fixedColumns = list(leftColumns = 1),
          # Scroller
          deferRender = T,
          scrollY = 685,
          scroller = T,
          # Buttons
          dom = 'Bfrtip',
          buttons = list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )
          )
        )
      )
    }
  })
  # table
  output$table <- renderDataTable({
    idx <-
      format(time(rv$priceData), "%Y-%m-%d") >= input$daterange[1] &
      format(time(rv$priceData), "%Y-%m-%d") <= input$daterange[2]
    if (length(rv$priceData[idx,]) > 0) {
      d <- data.frame(Date = format(time(rv$priceData)[idx], "%Y-%m-%d"),
                      rv$priceData[idx, ],
                      row.names = NULL)
      datatable(
        d,
        extensions = c("Buttons", "FixedColumns", "Scroller"),
        options = list(
          searching = F,
          # FixedColumns
          scrollX = T,
          fixedColumns = list(leftColumns = 2),
          # Scroller
          deferRender = T,
          scrollY = 685,
          scroller = T,
          # Buttons
          dom = 'Bfrtip',
          buttons = list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )
          )
        )
      )
    }
  })
  #download button table
  output$downloadTable <- downloadHandler(
    #validate(!(length(rv$priceData) > 0), "No data available!"),
    filename = function() {
      paste0(Sys.Date(), "_StockMonitor_Data.csv")
    },
    content = function(file) {
      write.csv2(
        rv$priceData,
        file,
        col.names = TRUE,
        row.names = FALSE,
        na = "n.a."
      )
    }
  )
  #download button chart
  output$downloadChart <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_StockMonitor_Chart.png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = output$chartplot,
        device = "png",
        width = 18,
        height = 9
      )
    }
  )
})

#
# functions
#

# symbols to display names
convertsymbols <- function(symbols) {
  str_to_title(trimws(gsub(
    "index|aktie", "", chartr("_-", "  ", symbols)
  )))
}

# stable calc (also for vector) simple return from zoo-object
simpleret <- function(z) {
  return(z[-1,] / coredata(z[-nrow(z),]) - 1)
}

# calc statistics
calcstats <- function(prices, scaling = 252) {
  data <- simpleret(na.locf(prices, na.rm = T)) # simple returns
  observations <- nrow(prices) - colSums(is.na(prices))
  #fromdate <-colCummins(!is.na(prices))
  #colCumsums(!is.na(prices))
  rmean <- 100 * ((colMeans(data) + 1) ^ scaling - 1)
  rstd <- 100 * colStdevs(data) * sqrt(scaling)
  shortfall <- rmean / rstd
  acorr1 <-
    apply(data, 2, function(x)
      acf(as.numeric(x), lag.max = 1, plot = F)[1][[1]])
  datag <- Return.Geltner(data)[2:nrow(data), ]
  rmeang <- 100 * ((colMeans(datag) + 1) ^ scaling - 1)
  rstdg <- 100 * colSds(datag) * sqrt(scaling)
  stats <-
    t(round(
      data.frame(
        observations,
        rmean,
        rstd,
        shortfall,
        100 * pnorm(shortfall, lower.tail = F),
        100 * t(VaR(data, p = 0.95, method = "gaussian")),
        100 * t(VaR(data, p = 0.95, method = "modified")),
        100 * t(VaR(data, p = 0.95, method = "historical")),
        100 * t(VaR(data, p = 0.99, method = "gaussian")),
        100 * t(VaR(data, p = 0.99, method = "modified")),
        100 * t(VaR(data, p = 0.99, method = "historical")),
        100 * colSums(data > 0) / nrow(data),
        100 *
          t(maxDrawdown(data)),
        colSkewness(data),
        3 + colKurtosis(data),
        acorr1,
        rmeang,
        rstdg
      ),
      2
    ))
  rownames(stats) <-
    c(
      "Observations",
      "Average Return p.a.%",
      "Standard Deviation p.a.%",
      "Sharpe Ratio (rf=0) p.a.",
      "Shortfall Probability (1 year)%",
      "Value@Risk 95% Delta-Normal p.a.%",
      "Value@Risk 95% Cornish-Fisher p.a.%",
      "Value@Risk 95% Historical p.a.%",
      "Value@Risk 99% Delta-Normal p.a.%",
      "Value@Risk 99% Cornish-Fisher p.a.%",
      "Value@Risk 99% Historical p.a.%",
      "Positive Returns%",
      "Maximum Drawdown%",
      "Skewness",
      "Kurtosis",
      "Autocorrelation Lag 1",
      "Geltner Adj. Average Return p.a.%",
      "Geltner Adj. Standard Deviation p.a.%"
    )
  return(stats)
}
