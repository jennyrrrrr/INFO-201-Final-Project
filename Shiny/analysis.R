library(dplyr)
library(plotly)
library(ggplot2)
library(ggmap)
library(countrycode)
library(shiny)
# ======================================================
# Read in data
# ======================================================
undernourished_df <- read.csv("Data/prevalence-of-undernourishment.csv")
colnames(undernourished_df)[colnames(undernourished_df) == "Prevalence.of.undernourishment....of.population.....of.population."] <- "Percent_Undernourished"
imp_exp_df <- read.csv("Data/FAOSTAT_data_11-13-2019.csv")
world_undernourished_df <- read.csv("Data/undernourished.csv", 
                                    stringsAsFactors = FALSE)
colnames(world_undernourished_df)[colnames(world_undernourished_df) == "Number.of.people.undernourished..FAO.SOFI..2018....World.Bank..2017.."] <- "Number_Undernourished"
# ======================================================
# Global number of people who are undernourished line graph.
# ======================================================
globalrates_p <- function() {
  p1 <- ggplot() + 
  geom_line(aes(
    y = Number_Undernourished, 
    x = Year), data = world_undernourished_df) +
    scale_x_continuous(breaks=seq(1991,2017,2)) + 
    scale_y_continuous(labels = scales::comma)
  theme(text=element_text(family="Tahoma"))
  p1 + labs(title = "Global Number of People Who<br>Are Undernourished", 
            x = "Year", y = "Amount of People", 
            caption = "Source: UN FAO (2018); UN FAO (2017); World Bank (2017)")
}

# ======================================================
# Create heat map for rates of undernourishment
# ======================================================
# geo styling
l <- list(color = toRGB("grey"), width = 0.5)
# specify map projection/options
geo <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

undernourished_p <- function(year) {
  undernourished_df %>% 
    filter(Year == year) %>% 
    plot_geo() %>%
    add_trace(
      z = ~Percent_Undernourished, color = ~Percent_Undernourished, colors = 'Reds',
      text = ~Entity, locations = ~Code, marker = list(line = l)
    ) %>%
    colorbar(title = 'Percent of Population', tickprefix = '%') %>%
    layout(
      title = paste("World Undernourishment in", year),
      geo = geo
  )

}

# ======================================================
# Create export df and heat map for global export values
# ======================================================
exp_df <- imp_exp_df %>% 
  group_by(Area, Element) %>% 
  summarize(Year = mean(Year), Value = sum(Value)) %>%
  filter(Element == "Export Value Base Quantity")

imp_df <- imp_exp_df %>% 
  group_by(Area, Element) %>% 
  summarize(Year = mean(Year), Value = sum(Value)) %>%
  filter(Element == "Import Value Base Period Quantity")

modified_imp_exp_df <- merge(exp_df,imp_df, by = "Area")
colnames(modified_imp_exp_df)[colnames(modified_imp_exp_df)=="Value.x"] <- "Export.Value"
colnames(modified_imp_exp_df)[colnames(modified_imp_exp_df)=="Value.y"] <- "Import.Value"
modified_imp_exp_df$Code <- countrycode(modified_imp_exp_df$Area, 'country.name', 'genc3c')
modified_imp_exp_df$imp_exp_difference <- modified_imp_exp_df$Export.Value - modified_imp_exp_df$Import.Value

geo <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

exports_p <- function(){
  plot_geo(modified_imp_exp_df) %>%
  add_trace(
    z = ~Export.Value, color = ~Export.Value, colors = 'Greens',
    text = ~Area, locations = ~Code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Export Value Quantity', tickprefix = '$') %>%
  layout(
    title = 'Exports in 2017',
    geo = geo
  )
}


