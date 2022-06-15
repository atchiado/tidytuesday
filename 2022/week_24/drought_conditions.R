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
         state = str_to_title(state)) %>%
  pivot_longer(cols = c(d0:w4), names_to = "Code", values_to = "Value") %>%
  select(-x0) %>%
  filter(Code  != "x9") %>%
  mutate(Level = case_when(Code == "d4" ~ "Exceptional Dry",
                           Code == "d3" ~ "Extreme Dry",
                           Code == "d2" ~ "Severe Dry",
                           Code == "d1" ~ "Moderate Dry",
                           Code == "d0" ~ "Abnormally Dry",
                           Code == "w0" ~ "Abnormally Wet",
                           Code == "w1" ~ "Moderate Wet",
                           Code == "w2" ~ "Severe Wet",
                           Code == "w3" ~ "Extreme Wet",
                           Code == "w4" ~ "Exceptional Wet")) %>%
  mutate(Condition = if_else(Level %in% c("Abnormally Wet", "Moderate Wet", "Severe Wet",
                                          "Extreme Wet", "Exceptional Wet"), "Wet", "Dry")) %>%
  mutate(fill = case_when(Level == "Exceptional Dry" ~ "#C7EF34FF",
                          Level == "Extreme Dry" ~ "#FABA39FF",
                          Level == "Severe Dry" ~ "#F66B19FF",
                          Level == "Moderate Dry" ~ "#CB2A04FF",
                          Level == "Abnormally Dry" ~ "#7A0403FF",
                          Level == "Abnormally Wet" ~ "#36AAF9FF",
                          Level == "Moderate Wet" ~ "#1AE4B6FF",
                          Level == "Severe Wet" ~ "#30123BFF",
                          Level == "Extreme Wet" ~ "#4662D7FF",
                          Level == "Exceptional Wet" ~ "#72FE5EFF"))

utah_tbl = filter(drought_tbl, state == "Utah")
  


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
             axis.text.y = element_blank(),
             plot.background = element_rect(fill = "grey98", color = "grey98"),
             panel.background = element_rect(fill = "grey98", color = "grey98"),
             panel.grid.major.x = element_line(color = "grey80", size = 0.3),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.spacing.y = unit(0, "lines"),
             strip.text.y = element_blank(),
             legend.position = "bottom",
             legend.text = element_text(size = 9, color = "grey40"),
             legend.box.margin = margin(t = 30),
             legend.background = element_rect(color = "grey40", size = .3, fill = "grey95"),
             legend.key.height = unit(.25, "lines"),
             legend.key.width = unit(2.5, "lines"),
             plot.margin = margin(rep(20, 4)))

# Define color palette
palette <- c("#FFB400", lighten("#FFB400", .25, space = "HLS"),
             "#C20008", lighten("#C20008", .2, space = "HLS"),
             "#13AFEF", lighten("#13AFEF", .25, space = "HLS"),
             "#8E038E", lighten("#8E038E", .2, space = "HLS"),
             "#595A52", lighten("#595A52", .15, space = "HLS"))

# Plot data
ggplot(utah_tbl, aes(x = date, y = Value, fill = Level)) +
  geom_stream()
  block
  
