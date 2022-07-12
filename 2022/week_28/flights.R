library(tidyverse)
library(ggtext)
library(showtext)
font_add_google("Lato")
showtext_auto()


## Load data --------------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 28)