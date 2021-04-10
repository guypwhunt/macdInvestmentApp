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
        actionButton("analyseButton", "Analyse"),
        br(),
        br(),
        uiOutput("symbolText"),
    ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("MACD Analysis",br(), br(), plotlyOutput('macdPlot'))#,
                        )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$analyseButton, {
        timeSeries <- getTimeSeries(input$symbol)
        timeSeries <- formatTimeSeries(timeSeries)
        macd <- calculateMacd(timeSeries)
        output$macdPlot <- renderPlotly({ plotMacd(macd, input$symbol)})
    })

    output$symbolText <- renderUI({
        symbolText()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
