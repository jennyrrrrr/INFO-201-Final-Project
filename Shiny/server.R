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
  output$undernourish_plot <- renderPlotly({
    undernourished_p(input$year)
  })
  output$exports_plot <- renderPlotly({
    exports_p()
  })
  output$globalrates_plot <- renderPlotly({
    globalrates_p()
  })

  output$analysis_plot <- renderPlotly({
    analysis_plot()
  })
})
