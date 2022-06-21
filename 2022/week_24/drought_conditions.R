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
                                          "Extreme Wet", "Exceptional Wet"), "Wet", "Dry")) %>%
  group_by(date, state, Code) %>%
  mutate(mean = mean(Value, na.rm = TRUE)) %>%
  mutate(mean = case_when(Code %in% c("w0", "w1", "w2", "w3", "w4") ~ -mean, TRUE ~ mean))

stream_tbl = filter(drought_tbl,
                    state %in% c("Utah", "Arizona", "Nevada", "Alabama", "Louisiana", "Mississippi") &date >= 1990)
stream_tbl$date = as.numeric(stream_tbl$date)
stream_tbl$Level <- ordered(stream_tbl$Level, levels = c("Exceptional Dry", "Extreme Dry", "Severe Dry",
                                                         "Moderate Dry", "Abnormally Dry",
                                                         "Exceptional Wet", "Extreme Wet", "Severe Wet",
                                                         "Moderate Wet", "Abnormally Wet"))


## Create viz
# Set theme
theme_set(theme_minimal(base_family = "Lato", base_size = 12))

theme_update(axis.title = element_blank(),
             axis.ticks = element_line(color = "grey70", size = .5),
             axis.ticks.length.y = unit(0, "lines"),
             axis.ticks.length.x = unit(0.4, "lines"),
             plot.background = element_rect(fill = "grey98", color = "grey98"),
             panel.background = element_rect(fill = "grey98", color = "grey98"),
             panel.grid = element_blank(),
             panel.spacing.y = unit(0, "lines"),
             strip.text.y = element_blank(),
             axis.text.y = element_blank(),
             legend.position = "bottom",
             legend.text = element_text(size = 9, color = "grey40"),
             legend.box.margin = margin(t = 30),
             legend.background = element_rect(color = "grey40", size = .3, fill = "grey95"),
             legend.key.height = unit(.25, "lines"),
             legend.key.width = unit(2.5, "lines"),
             plot.margin = margin(10, 40, 20, 3),
             plot.title = element_text(color = "grey10", size = 25, face = "bold",
                                       hjust = 0.045, margin = margin(t = 15)),
             plot.subtitle = element_text(color = "grey30", size = 11, lineheight = 1.35,
                                          hjust = 0.09, margin = margin(t = 10, b = 20)),
             plot.title.position = "plot",
             plot.caption.position = "plot",
             plot.caption = element_text(color = "grey30", size = 9, lineheight = 1.2, 
                                         hjust = 0.07, margin = margin(t = 20)))

# Define color palette
palette <- c("#7A0403FF", "#CB2A04FF", "#F66B19FF", "#FABA39FF", "#C7EF34FF",
             "#30123BFF", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF", "#72FE5EFF")

levels <- c("Alabama", "Arizona", "Louisiana", "Mississippi", "Nevada", "Utah")
labels <- tibble(date = 1987.2, value = 0,
                 state = factor(levels, levels = levels),
                 label = c("Alabama", "Arizona", "Louisiana", "Mississippi", "Nevada", "Utah"))


# Plot data
ggplot(stream_tbl, aes(x = date, y = mean, fill = Level)) +
  geom_area() +
  scale_fill_manual(values = palette) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  geom_text(data = labels, aes(date, value, label = label), family = "Lato", inherit.aes = FALSE,
            size = 4, color = "grey30", lineheight = .85, hjust = 0) +
  facet_grid(state ~ ., scales = "free_y", space = "free") +
  labs(title = "Historical Drought Conditions (1990-2022)",
       subtitle = "Graph depicts historical trends of drought conditions for the six states in the contiguous U.S. with the driest and wettest conditions on average.
     The 10 categorizations are determined by the Standardized Precipitation Index (SPI), which characterizes meteorological drought on a range of timescales, ranging
      from 1 to 72 months, for the lower 48 U.S. states. The SPI is the number of standard deviations that observed cumulative precipitation deviates from the climatological
 average. NOAA's National Centers for Environmental Information produce the 9-month SPI values below on a monthly basis, going back to 1895.",
       caption = "Visualization: Anthony Chiado  •  Data: US National Integrated Drought Information System  •  Code: atchiado/tidytuesday on GitHub  • Created for R4DS #tidytuesday")
  
  
