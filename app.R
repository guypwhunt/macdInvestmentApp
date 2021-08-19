#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
source("functions.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MACD Analysis"),
    sidebarPanel(
        textInput("symbol", "Stock Symbol", "^FTSE"),
        selectInput("source", "Data Source", c("yahoo")),
        selectInput("time", "Time Period", c("daily", "weekly", "monthly")),
        actionButton("analyseButton", "Analyse")

    ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Index Funds",br(), tableOutput('indexFunds')),
                        tabPanel("Industry ETFs",br(), tableOutput('industryEtf')),
                        tabPanel("Geographical ETFs",br(), tableOutput('geographicalEtf')),
                        tabPanel("MACD Analysis",br(), br(), plotlyOutput('macdPlot')),
                        tabPanel("Price Analysis",br(), br(), plotlyOutput('pricePlot'))
                        )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$analyseButton, {
        timeSeries <- getTimeSeries(input$symbol, input$source, input$time)
        timeSeries <- formatTimeSeries(timeSeries)
        try({macd <- calculateMacd(timeSeries)
        output$macdPlot <- renderPlotly({ plotMacd(macd, input$symbol)})})
        output$pricePlot <- renderPlotly({plotPrice(timeSeries, input$symbol)})
    })

    output$symbolText <- renderUI({
        symbolText()
    })

    output$indexFunds <- renderTable(indexFunds())

    output$industryEtf <- renderTable(industryEtfs())

    output$geographicalEtf <- renderTable(geographicalEtfs())

}

# Run the application
shinyApp(ui = ui, server = server)
