library(dplyr)
library(plotly)
library(ggplot2)
library(ggmap)
library(countrycode)

hunger_index_df <- read.csv("data/global-hunger-index.csv")
undernourished_df <- read.csv("data/prevalence-of-undernourishment.csv")
colnames(undernourished_df)[colnames(undernourished_df)=="Prevalence.of.undernourishment....of.population.....of.population."] <- "Percent_Undernourished"
imp_exp_df <- read.csv("data/FAOSTAT_data_11-13-2019.csv")

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
undernourished_p

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

geo <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

exports_p <- plot_geo(modified_imp_exp_df) %>%
  add_trace(
    z = ~Export.Value, color = ~Export.Value, colors = 'Greens',
    text = ~Area, locations = ~Code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Export Value Quantity', tickprefix = '$') %>%
  layout(
    title = 'Exports in 2017',
    geo = geo
  )
exports_p


