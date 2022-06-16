library(tidyverse)
library(tidytuesdayR)
library(fuzzyjoin)
library(ggstream)
library(colorspace)
library(ggtext)
library(grid)
library(shadowtext)
library(showtext)
library(cowplot)
library(janitor)
font_add_google("Lato")
showtext_auto()


## Load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 24)
drought <- tuesdata$drought


## Clean data
drought_tbl <- drought %>%
  clean_names() %>%
  mutate(date = substr(date, 3, 6),
         state = str_replace(state, "-", " "),
         state = str_to_title(state)) %>%
  rename(x0 = 0, x9 = -9) %>%
  subset(select = -c(x0, x9)) %>%
  group_by(date, state) %>%
  summarise(d0 = mean(d0),
            d1 = mean(d1),
            d2 = mean(d2),
            d3 = mean(d3),
            d4 = mean(d4),
            w0 = mean(w0),
            w1 = mean(w1),
            w2 = mean(w2),
            w3 = mean(w3),
            w4 = mean(w4)) %>%
  pivot_longer(cols = c(d0:w4), names_to = "Code", values_to = "Value") %>%
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
                                          "Extreme Wet", "Exceptional Wet"), "Wet", "Dry"))

stream_tbl = filter(drought_tbl, state == c("California", "Utah", "Arizona", "Nevada"), date >= 1972)
stream_tbl$date = as.numeric(stream_tbl$date)
stream_tbl$Level <- ordered(stream_tbl$Level, levels = c("Exceptional Dry", "Extreme Dry", "Severe Dry",
                                                     "Moderate Dry", "Abnormally Dry",
                                                     "Abnormally Wet", "Moderate Wet", "Severe Wet",
                                                     "Extreme Wet", "Exceptional Wet"))


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
             panel.grid = element_blank(),
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
palette <- c("#7A0403FF", "#CB2A04FF", "#F66B19FF", "#FABA39FF", "#EFE350FF", 
             "#95D840FF", "#29AF7FFF", "#30123BFF", "#404788FF", "#481567FF")

# Plot data
ggplot(stream_tbl, aes(x = date, y = Value, fill = Level)) +
  geom_stream(type = "proportional") +
  scale_fill_manual(values = palette) +
  facet_grid(state ~ ., scales = "free_y", space = "free")
  
