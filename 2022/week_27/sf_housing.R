library(tidyverse)
library(ggtext)
library(showtext)
library(sf)
font_add_google("Lato")
showtext_auto()


# Load data ---------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 27)
rent_data <- tuesdata$rent
permit_data <- tuesdata$sf_permits
construction_data <- tuesdata$new_construction


# 