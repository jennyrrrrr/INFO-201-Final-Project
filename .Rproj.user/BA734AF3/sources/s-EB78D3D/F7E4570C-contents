#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("Undernourishment Around the World"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = 2000,
                        max = 2016,
                        value = 2010)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("undernourishPlot")
        )
    )
))
