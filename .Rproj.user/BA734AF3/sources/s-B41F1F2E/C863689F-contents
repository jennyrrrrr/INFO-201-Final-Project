library(dplyr)
library(plotly)
library(ggplot2)

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

p <- plot_geo(undernourished_df) %>%
  add_trace(
    z = ~Percent_Undernourished, color = ~Percent_Undernourished, colors = 'Reds',
    text = ~Entity, locations = ~Code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Percent of Population', tickprefix = '%') %>%
  layout(
    title = 'Undernourishment in the World',
    geo = geo
  )
p
