library(tidyverse)
library(tidytuesdayR)
library(fuzzyjoin)
library(ggstream)
library(colorspace)
library(ggtext)
library(cowplot)
library(janitor)

## Load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 24)
drought <- tuesdata$drought


## Clean data
drought_tbl <- drought %>%
                clean_names() %>%
                  mutate(date = str_remove(date, "^d_"),
                         date = as.Date(date, format = "%Y%m%d"),
                         state = str_replace(state, "-", " "),
                         state = str_to_title(state))


## Create viz
# Set theme
theme_set(theme_minimal(base_family = "Lato", base_size = 12))

theme_update(plot.title = element_text(color = "grey10", size = 25, face = "bold",
                                       margin = margin(t = 15)),
             plot.subtitle = element_text(color = "grey30", size = 12, lineheight = 1.35,
                                              margin = margin(t = 10, b = 20)),
             plot.caption = element_text(color = "grey30", size = 8, lineheight = 1.2, 
                                         hjust = 0, margin = margin(t = 20)),
             axis.title = element_blank(),
             axis.text.y = element_blank(),)
