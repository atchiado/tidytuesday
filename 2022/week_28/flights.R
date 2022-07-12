library(tidyverse)
library(lubridate)
library(ggtext)
library(showtext)
library(rcartocolor)
font_add_google("Lato")
showtext_auto()


## Load data --------------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 28)
flights <- tuesdata$flights


## Clean data --------------------
flight_data <- flights %>%
  rename(airport = APT_NAME) %>% 
  filter(airport %in% c("iGA Istanbul Airport", "Istanbul Atatürk", "Paris-Charles-de-Gaulle", "Amsterdam - Schiphol",
                     "Frankfurt", "Madrid - Barajas", "London - Heathrow", "Barcelona", "Paris-Orly",
                     "Palma de Mallorca", "Munich")) %>%
  mutate(airport = recode(airport,
                          "iGA Istanbul Airport" = "Istanbul Airport",
                          "Istanbul Atatürk" = "Istanbul Airport")) %>%
  mutate(year_week = str_c(isoyear(FLT_DATE),
                           "/",
                           formatC(isoweek(FLT_DATE), format = "f", digits = 0, width = 2, flag = "0"))) %>%
  mutate(week_day = wday(FLT_DATE, week_start = 1)) %>% 
  select(airport, year_week, week_day, FLT_TOT_1) %>%
  group_by(airport, year_week, week_day) %>%
  summarise(flight_total = sum(FLT_TOT_1)) %>%
  ungroup() %>% 
  complete(airport, year_week, week_day) %>% 
  mutate(flight_total = replace_na(flight_total, 0))

# Order airport factor levels
fact_levs <- flight_data %>% 
  group_by(airport) %>% 
  summarise(n = sum(flight_total)) %>% 
  arrange(-n) %>% 
  pull(airport)

# Set aiport factor levels
flight_data <- flight_data %>% 
  mutate(airport = factor(airport, levels = fact_levs))
    

## Data viz --------------------
# Set theme
theme_update(axis.title = element_blank(),
             plot.background = element_rect(fill = "grey98", color = "grey98"),
             panel.background = element_rect(fill = "grey98", color = "grey98"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),
             axis.line.y.left = element_blank(),
             panel.spacing.x = unit(1, "cm" ),
             panel.spacing.y = unit(1, "cm" ),
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(),
             strip.text.y = element_text(family = "Lato", size = 12, face = "bold", hjust = 0.5, angle = 180, color = "grey30"),
             strip.background = element_rect(fill = "grey98"),
             legend.position = "bottom",
             legend.background = element_rect(fill = "grey98"),
             legend.title = element_text(size = 10, face = "bold", color = "grey30"),
             legend.text = element_text(family = "Lato", size = 8, color = "grey30"),
             legend.margin = margin(t = 5),
             plot.margin = margin(10, 60, 20, 40),
             plot.title = element_text(family = "Lato", color = "grey10", size = 25, face = "bold",
                                       margin = margin(t = 15)),
             plot.subtitle = element_text(family = "Lato", color = "grey30", size = 12, lineheight = 1.35,
                                              margin = margin(t = 10, b = 25)),
             plot.title.position = "plot",
             plot.caption.position = "plot",
             plot.caption = element_text(family = "Lato", color = "grey30", size = 8, lineheight = 1.2, 
                                         hjust = 0, margin = margin(t = 20)))

# Plot data
ggplot(flight_data, aes(x = year_week, y = week_day, fill = flight_total)) +
  geom_raster() +
  facet_wrap(~ airport, ncol = 1, strip.position = "left") +
  scale_y_reverse(breaks = 7:1,
                     labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
                      sec.axis = sec_axis(~.,
                                          breaks = 7:1,
                                          labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))) +
  scale_x_discrete(breaks = c("2016/01", "2017/01", "2018/01", "2019/01", "2020/01", "2021/01", "2022/01"), 
                   labels = 2016:2022) +
  scale_fill_carto_c(name = "Number of daily flights", palette = "SunsetDark", direction = 1, limits = c(0, 1500)) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = "Bay Area Housing Development",
       subtitle = "The graph depicts construction rates for single and multi-family housing by county in the San Francisco Bay Area. Rates indicate the\npercentage of total housing production for each county that is made up by each specific type of housing. Negative values indicate\nthat a given county reduced the amount of the specified type of housing over the evaluation period from 2008-2018.",
       caption = "Visualization: Anthony Chiado  •  Data: data.sfgov.org & Kate Pennington  •  Code: atchiado/tidytuesday on GitHub  • Created for R4DS #tidytuesday")

  