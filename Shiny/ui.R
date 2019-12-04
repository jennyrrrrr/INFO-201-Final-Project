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
library(shinythemes)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = shinytheme("cosmo"),
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
                     one country’s undernutrition rate is related to the amount
                     of the country’s food supply that is imported and 
                     exported.")),
                     )
                 ),
        tabPanel("Undernourishment ", sidebarLayout(
                             position = "right",
                             sidebarPanel(
                            withTags(b("Undernourishment:")),
                            br(),
                             withTags(i("Hover the cursor on the map to see
                             the country name and the detailed value of the
                             percentage of undernourishment population in the
                             country(%)")),
                            br(),br(),
                            "In the world undernourishment map, we used the
                            gradient palette to color the percentage of
                            undernourishment population by country. The
                            countries with a deeper color mean that it has a
                            much more severe undernourishment situation during
                            the selected year and vice versa.",
                            br(),br(),
                            "Comparing the global undernourishment level by
                            years, it is clearly seen that over the past few
                            years(from 2000 to 2016), the distribution of the
                            undernourishment over the world hasn’t been
                            changing a lot. The Africa and Asia areas have
                            consistently been experiencing a high level of
                            undernourishment over the years. In contrast, America,
                            Oceania, and Europe area always been having a low
                            percentage of the undernourishment population over
                            the years. Take the year 2016 as an example, the
                            Central African Republic has the highest number of
                            the percentage of undernourishment
                            population(61.8%), Canada, United States, Brazil,
                            Japan, and most of the European and Oceania
                            countries have the lowest percentage of 2.5%.",
                            br(),br(),
                            withTags(i("The most comprehensive dataset we found
                            from the official databases is the undernourishment
                            dataset ranging from 2000 to 2016 and the food
                            export dataset in 2017. As it is clearly shown in
                            the undernourishment data visualization, the
                            distribution of the global undernourishment level
                            hasn’t been changing a lot over the past 16 years,
                            thus it could be assumed that 2017 would have a
                            similar undernourishment level as for 2016.")),
                            br(),br(),
                            withTags(b("Exports:")),
                            br(),
                            withTags(i("Hover the cursor on the map to see the
                            country name and the detailed value of the export
                            value in the country(M$)")),
                            br(),br(),
                            "Looking into the export world map, for 2017,  the
                            United States has the highest export value(~168M$),
                            most of the Asia, America, and Oceania countries
                            have an export value of around 50M$, and most of
                            the African countries has an export value of lower
                            than 1M$.",
                            br(),br(),
                            withTags(b("Conclutions:")),
                            br(),
                            "It is important to notice these graphs have an
                            inverse relationship. The countries with high
                            exportation values have low undernourishment rates
                            and countries with low exportation seem to have the
                            highest rates of undernourishment. It should be
                            noted that",
                            withTags(b("correlation does not mean causation."))
                             ),
                             # Show a plot of the generated distribution
                             mainPanel(fluidRow(
                                 column(3,sliderInput("year",
                                                      "Year:",
                                                      sep = "",
                                                      min = 2000,
                                                      max = 2016,
                                                      value = 2016)),
                                 column(12,plotlyOutput("undernourishPlot")),
                                 column(12,plotlyOutput("exportsPlot"))
                                 ))
                             )
                 ),
        tabPanel("Analysis: Hunger vs. Food Trade", sidebarLayout(
            position = "right",
            sidebarPanel("Pictured [left] is a graph of food success per
                         country. We define food success as the percentage
                         makeup of exported goods compared to total trade,
                         divided by the percentage of the population that is
                         undernourished. Here is the formula:",
                         withMathJax(helpText("$$\\frac{\\frac{Exported Goods($)}{Imported Goods - Exported Goods ($)}*100%}{Percent Undernourishment}$$")),
                         br(),
                         "We believe that this value is most indicative of
                         food success. For example, a country with very low
                         undernourishment rates and high export numbers in
                         relation to import numbers is probably a country that
                         is well fed and can afford to export foodstuffs. Thus,
                         we classify such a country as high success. New 
                         Zealand is an example, with a food success value of
                         36.50.",
                         br(),
                         br(),
                         "Note that the range of food success values has been
                         contrived to run from 0 to 40. If a country exports
                         no goods, it will have a 0 food success value. If a
                         country only exports goods, and has minimum
                         undernourishment (2.5% as defined by the data provided
                         by ourworldindata.org), it will have an index of 40.
                         We did this because countries with higher populations
                         would have higher export/import numbers, so using
                         percentages of food trade would best equalize
                         countries.",
                         br(),br(),
                         withTags(b("ANALYSES AND CONCLUSIONS")),
                         br(),
                         "General trends can be seen in the data — for example
                         , we can see that Europe has a very high food success
                         value for the large majority of its countries. In fact,
                         the majority of the top 25th percentile of food success
                         values are European countries. Similar conclusions can
                         be drawn for the rest of the regions. As expected, this
                         data is heavily skewed toward higher food success
                         values.",
                         br(),br(),
                         "The data results are generally not too surprising.
                         More developed regions of the world have higher food
                         success values, due to low undernourishment rates and
                         high export percentages. Some countries — Japan, for
                         example — have low food success values yet low
                         undernourishment values. This is due to relatively low
                         exports compared to imports. However, this raises the
                         question: why are these exports so low? Is it due to
                         culture? Government policy? Isolationism? Maybe the
                         country simply doesn’t have land suitable for food
                         production? Organizations can wield this information
                         to delve deeper to address possible issues in a 
                         country’s food trade.",
                         br(),br(),
                         "This work can be used when providing humanitarian aid
                         to countries — for example, if a country has high
                         export rates but also high undernourishment, perhaps
                         something is to be said about disproportionate
                         benefit: lots of exporting is going on to generate
                         wealth for the rich, yet many people in that country
                         go hungry. Governments and coalitions can then shape
                         policy accordingly to ensure that proper nourishment
                         for the country takes priority over making the rich
                         richer."
                         
            ),mainPanel(
                     plotlyOutput("analysisPlot", width = 800, height = 1000)
                 ))
            ),
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


