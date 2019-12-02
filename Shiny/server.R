#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
source("analysis.R")

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    output$undernourishPlot <- renderPlotly({
        undernourished_p(input$year)
    })
    output$exportsPlot <- renderPlotly({
        exports_p()
    })
    output$globalratesPlot <- renderPlotly({
        globalrates_p()
    })
    
    
})