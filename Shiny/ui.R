#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    titlePanel("Interplay Between Food Importation/Exportation 
               and Hunger Statistics Around the Planet"),
    navbarPage("Project: Hungry",
        tabPanel("Global Undernourishment", 
                 mainPanel(
                     plotlyOutput("globalratesPlot"),
                     "Human undernutrition — defined as dietary energy 
                     consumption that falls under the minimum energy
                     requirement defined as necessary[1] — and hunger are
                     issues that are prevalent even today. As
                     you can see from the graph above, global rates of 
                     malnutrition are still very high and in recent years have
                     reversed from steadily decreasing to increasing. By 
                     nature of this human problem, all humans are direct
                     stakeholders in this issue: should food supply run short,
                     people will starve. This also means that the whole of
                     humanity is indirect stakeholders as well, with aligning
                     values. Without proper nutrition and hunger, people 
                     cannot begin to focus on more intellectual pursuits that
                     involve the advancement of society, instead of focusing
                     their efforts on self-survival.",
                     br(),
                     br(),
                     withTags(b("Here we will explore how
                     one country’s malnutrition rate is related to the amount
                     of the country’s food supply that is imported and 
                     exported.")),
                     )
                 ),
        tabPanel("Undernourishment by Country", sidebarLayout(
                             sidebarPanel(
                                 sliderInput("year",
                                             "Year:",
                                             sep = "",
                                             min = 2000,
                                             max = 2016,
                                             value = 2010)
                             ),
                             # Show a plot of the generated distribution
                             mainPanel(
                                 plotlyOutput("undernourishPlot"),
                                 plotlyOutput("exportsPlot")
                                 )
                             )
                 ),
        tabPanel("Food Exports by Country",
                 mainPanel(
                     )
                 ),
        tabPanel("Analysis: Hunger and Food Production"),
        tabPanel("References",
                 mainPanel(
                     "Dataset 1:",
                     a("Imports and exports",
                       href = 
                           "http://www.fao.org/faostat/en/#data/TI"),
                     br(),
                     "Created by the Food and Agriculture Organization of the
                     United Nations (FAO), this trade database includes
                     observations as countries with the following variables:
                     year, export quantity, export value, import quantity and
                     import value (all of the foodstuffs). The trade database
                     includes all food and agricultural products 
                     imported/exported annually by all the countries in the
                     world.
                     This dataset is approximately 650 observations. It is 
                     great for a short span of time (the entries are from 
                     2017), but will not be very indicative of the past.",
                     br(),br(),
                     
                     "Dataset 2:",
                     a("Undernourishment by country",
                       href = "https://ourworldindata.org/grapher/prevalence-of-undernourishment"),
                     br(),
                     "This dataset measures the share of the population that
                     has a caloric intake which is insufficient to meet the
                     minimum energy requirements necessary for a given
                     individual. Countries with undernourishment under 2.5% are
                     automatically given a value of 2.5%. This may be a 
                     weakness — hypothetically, a stellar country where no one
                     is undernourished will still have a 2.5% undernourishment
                     rate — small, but noticeable nonetheless.",
                     br(),br(),
                     
                     "Dataset 3:",
                     a("Global amount of undernourishment",
                       href = 
                       "https://ourworldindata.org/grapher/global-population-defined-as-undernourished?time=1991..2017"),
                     br(),
                     "Gathered by ourworldindata, this dataset details the 
                     total number of people who are defined as undernourished
                     in the world. An individual is considered to be 
                     undernourished when dietary energy consumption is less
                     than a pre-determined threshold. This threshold is
                     country-specific and is measured in terms of the number
                     of kilocalories required to conduct sedentary or light
                     activities. A weakness of this data is that the
                     definition of how many kilocalories are required per
                     person are needed per day may be off. For example,
                     perhaps individuals burn more calories in the heat and
                     temperature of a country was not considered in the data."
                 )
                 )
    ),
))


