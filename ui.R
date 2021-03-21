#
# packages
#
library(shiny)
library(shinythemes) # superseded by bslib
library(shinyBS)
library(dygraphs)
library(DT)

#
# shiny UI handler
#
shinyUI(fluidPage(
  title = "Stock Market Monitor",
  theme = "yeti",
  # bslib::bs_theme(bootswatch = "yeti")
  # https://shiny.rstudio.com/gallery/shiny-theme-selector.html
  # https://bootswatch.com/yeti/
  
  # Application title
  titlePanel(title = div(
    "Stock Market Monitor",
    img(style = "float: right; padding-right: 0px; margin-top: -15px; width: 200; height: 51px;",
        src = "smartcube.gif")
  )),
  
  # Sidebar with input parameters
  sidebarLayout(
    sidebarPanel(
      # date range
      sliderInput(
        "daterange",
        label = h4(
          "Date Range",
          popify(
            icon("question-circle-o"),
            title = "Explanation",
            content = "Select the date range for the Charts, Benchmark, Stats and PriceTable panels. The pricing values will be recalculated to start with value 100 for better comparison. Note the slider beyond the Chart and Benchmark panels for further changing the window size.",
            placement = "right"
          )
        ),
        min = as.Date(DATASTARTDATE),
        max = Sys.Date(),
        value = c(as.Date(INDEXSTARTDATE), Sys.Date())
      ),
      
      #Creates a line
      hr(style = LINESTYLE),
      
      selectInput(
        "benchmarkselectinput",
        label = h4(
          "Benchmark",
          popify(
            icon("question-circle-o"),
            title = "Explanation",
            content = "Select one benchmark for comparison to individual security performance",
            placement = "right"
          )
        ),
        choices = ""
      ),
      
      #Creates a line
      hr(style = LINESTYLE),
      
      checkboxGroupInput(
        "securitycheckbox",
        label = h4(
          "Securities",
          popify(
            icon("question-circle-o"),
            title = "Explanation",
            content = "Select one or more securities for comparison to benchmark performance",
            placement = "right"
          )
        ),
        choices = "",
        inline = F
      ),
      
      #Creates a line
      hr(style = LINESTYLE),
      
      h4("Datafeed options"),
      
      radioButtons("datafeedradiobutton",
                   "",
                   c("Ariva"), # "Bloomberg", "Yahoo"
                   inline = TRUE),
      
      #Creates a line
      hr(style = LINESTYLE),
      
      downloadButton('downloadTable', 'Save table')
      #downloadButton('downloadChart', 'Save chart')
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("Chart", div(dygraphOutput("chartplot")),  # height = 480
               icon = icon("line-chart")),
      # https://shiny.rstudio.com/reference/shiny/0.11/icon.html
      # https://stackoverflow.com/questions/32686195/shinydashboard-some-font-awesome-icons-not-working
      # https://fontawesome.com/v4.7.0/icons/
      # https://fontawesome.com/v4.7.0/cheatsheet/
      
      tabPanel("Benchmark", div(dygraphOutput("benchmarkplot")), icon = icon("area-chart")),
      
      tabPanel("Stats", div(dataTableOutput("stats")), icon = icon("calculator")),
      
      tabPanel("PriceTable", div(dataTableOutput("table")), icon = icon("table"))
    ))
  )
))
