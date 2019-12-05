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
colnames(undernourished_df)[colnames(undernourished_df) == 
"Prevalence.of.undernourishment....of.population.....of.population."] <-
  "Percent_Undernourished"
imp_exp_df <- read.csv("Data/FAOSTAT_data_11-13-2019.csv")
world_undernourished_df <- read.csv("Data/undernourished.csv",
  stringsAsFactors = FALSE
)
colnames(world_undernourished_df)[colnames(world_undernourished_df) == 
"Number.of.people.undernourished..FAO.SOFI..2018....World.Bank..2017.."] <-
  "Number_Undernourished"
regions_df <- read.csv("Data/countryContinent.csv")

# ======================================================
# Global number of people who are undernourished line graph.
# ======================================================
globalrates_p <- function() {
  p1 <- ggplot() +
    geom_line(aes(
      y = Number_Undernourished,
      x = Year
    ), data = world_undernourished_df) +
    scale_x_continuous(breaks = seq(1991, 2017, 2)) +
    scale_y_continuous(labels = scales::comma)
  theme(text = element_text(family = "Tahoma"))
  p1 + labs(
    title = "Global Number of People Who<br>Are Undernourished",
    x = "Year", y = "Amount of People",
    caption = "Source: UN FAO (2018); UN FAO (2017); World Bank (2017)"
  )
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
  projection = list(type = "Mercator")
)

undernourished_p <- function(year) {
  undernourished_df %>%
    filter(Year == year) %>%
    plot_geo() %>%
    add_trace(
      z = ~Percent_Undernourished, color = ~Percent_Undernourished,
      colors = "Reds",
      text = ~Entity, locations = ~Code, marker = list(line = l)
    ) %>%
    colorbar(title = "Percent of Population", tickprefix = "%") %>%
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

modified_imp_exp_df <- merge(exp_df, imp_df, by = "Area")
colnames(modified_imp_exp_df)[colnames(modified_imp_exp_df) ==
  "Value.x"] <- "Export.Value"
colnames(modified_imp_exp_df)[colnames(modified_imp_exp_df) ==
  "Value.y"] <- "Import.Value"
modified_imp_exp_df$code <- countrycode(
  modified_imp_exp_df$Area,
  "country.name", "genc3c"
)
modified_imp_exp_df$imp_exp_difference <- modified_imp_exp_df$Export.Value -
  modified_imp_exp_df$Import.Value


geo <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = "Mercator")
)

exports_p <- function() {
  plot_geo(modified_imp_exp_df) %>%
    add_trace(
      z = ~Export.Value, color = ~Export.Value, colors = "Greens",
      text = ~Area, locations = ~code, marker = list(line = l)
    ) %>%
    colorbar(title = "Export Value Quantity", tickprefix = "$") %>%
    layout(
      title = "Exports in 2017",
      geo = geo
    )
}

# ======================================================
# Food Success per country vs hunger graph
# ======================================================

# Calculating ratio of import to export where the ratio = (import / export)
# Data is from 2017 ONLY
imp_exp_ratio <- merge(exp_df, imp_df, by = "Area")
colnames(imp_exp_ratio)[colnames(imp_exp_ratio) == "Value.x"] <- "Export.value"
colnames(imp_exp_ratio)[colnames(imp_exp_ratio) == "Value.y"] <- "Import.value"

imp_exp_ratio$code <- countrycode(imp_exp_ratio$Area, "country.name", "genc3c")
colnames(imp_exp_ratio)[colnames(imp_exp_ratio) == "code"] <- "country_code"

recent_undernour <- undernourished_df %>%
  filter(undernourished_df$Year == 2016)
colnames(recent_undernour)[colnames(recent_undernour) == "Code"] <-
  "country_code"

joined_dataframe <- merge(imp_exp_ratio, recent_undernour,
  by.x = "country_code", by.y = "country_code"
)

joined_dataframe$relative_trade <-
  round((100 * joined_dataframe$Export.value /
    (joined_dataframe$Import.value + joined_dataframe$Export.value))
  / joined_dataframe$Percent_Undernourished, 2)


# Prepare regions df, only want code_3, continent, country columns
colnames(regions_df)[colnames(regions_df) == "code_3"] <- "Code"
colnames(regions_df)[colnames(regions_df) == "continent"] <- "Continent"
regions_df_cleaned <- regions_df %>%
  select(Code, country, Continent)

# Add country/continent dataframe
primed_dataframe <- joined_dataframe %>%
  select(
    country_code, Area, relative_trade, Percent_Undernourished,
    Import.value, Export.value
  ) %>%
  arrange(desc(relative_trade)) %>%
  mutate(Area = factor(Area, Area))

joined_dataframe <- merge(
  x = joined_dataframe, y = regions_df_cleaned,
  by.x = "country_code", by.y = "Code"
)
joined_dataframe <- joined_dataframe %>%
  arrange(desc(relative_trade))

analysis_plot <- function() {
  p4 <- ggplot(joined_dataframe, aes(
    x = reorder(Area, -relative_trade), y = relative_trade, fill = Continent,
    text = paste(
      "Country:", Area, "<br>", "Relative Trade:", relative_trade, "<br>",
      "Import in $:", Import.value, "<br>",
      "Export in $:", Export.value, "<br>", "Percent Undernourished:",
      Percent_Undernourished
    )
  )) +
    theme_dark() +
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    ) +
    labs(
      y = "Food Success Value (see analysis for more info)",
      x = "Country",
      title = "Food Success and Percent Undernourishment by Country"
    ) +
    coord_flip() +
    geom_col(aes(color = Continent), color = "black") +
    theme(plot.title = element_text(hjust = 0.5))
  final_plot <- ggplotly(p4, tooltip = c("text"))
  return(final_plot)
}
analysis_plot()