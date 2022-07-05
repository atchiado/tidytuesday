library(tidyverse)
library(ggtext)
library(showtext)
library(sf)
library(devtools)
library(urbnmapr)
font_add_google("Lato")
showtext_auto()


## Load data --------------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 27)
new_construction <- tuesdata$new_construction

# Load county data for mapping
county_data <- urbnmapr::counties

# #Clean data --------------------
construction_data <- left_join(new_construction, county_data, by = c("county" = "county_name"))
                               