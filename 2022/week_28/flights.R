library(tidyverse)
library(lubridate)
library(ggtext)
library(showtext)
library(RColorBrewer)
font_add_google("Lato")
showtext_auto()


## Load data --------------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 28)
flights <- tuesdata$flights


## Clean data --------------------
# Identify min and max dates in the data
max_date = max(flights$FLT_DATE)
min_date = min(flights$FLT_DATE)

# Wrangle data to be useful for viz
flight_data <- flights %>%
  mutate(date_group = ifelse(FLT_DATE < ymd("2020-03-07"), "pre-covid",
                             ifelse(FLT_DATE > ymd("2021-10-17"), "post-covid", "mid-covid"))) %>%
  filter(APT_NAME %in% c("iGA Istanbul Airport", "Istanbul Atatürk", "Paris-Charles-de-Gaulle",
                        "Amsterdam - Schiphol", "Frankfurt", "Madrid - Barajas", "London - Heathrow",
                        "Athens", "Lisbon", "Rome - Fiumicino", "Vienna", "Zürich", "Kiev - Boryspil",
                        "Oslo - Gardermoen", "Brussels", "Copenhagen - Kastrup", "Dublin",
                        "Stockholm - Arlanda","Warszawa - Chopina")) %>%
  mutate(APT_NAME = recode(APT_NAME,
                         "iGA Istanbul Airport" = "Istanbul Airport",
                         "Istanbul Atatürk" = "Istanbul Airport",
                         "Paris-Charles-de-Gaulle" = "Charles De Gaule Airport",
                         "Amsterdam - Schiphol" = "Amsterdam Airport Schiphol",
                         "Frankfurt" = "Frankfurt am Main Airport",
                         "Madrid - Barajas" = "Adolfo Suárez Madrid–Barajas Airport",
                         "London - Heathrow" = "Heathrow Airport",
                         "Athens" = "Athens International Airport",
                         "Lisbon" = "Lisbon Airport",
                         "Rome - Fiumicino" = "Leonardo da Vinci Fiumicino Airport",
                         "Vienna" = "Vienna International Airport",
                         "Zürich" = "Zürich Airport",
                         "Kiev - Boryspil" = "Boryspil International Airport",
                         "Oslo - Gardermoen" = "Oslo Airport Gardermoen",
                         "Brussels" = "Brussels Airport",
                         "Copenhagen - Kastrup" = "Copenhagen Airport",
                         "Dublin" = "Dublin Airport",
                         "Stockholm - Arlanda" = "Stockholm Arlanda Airport",
                         "Warszawa - Chopina" = "Warsaw Chopin Airport")) %>%
  group_by(APT_NAME, date_group) %>%
  summarise(flight_total = sum(FLT_TOT_1), .groups = "drop") %>%
  rename(airport = APT_NAME) %>%
  pivot_wider(names_from = date_group, values_from = flight_total) %>%
  mutate(pre_covid_monthly = `pre-covid` / ((interval(min_date, ymd("2020-03-08")) %/% days(1)) / 30.4375),
         post_covid_monthly = `post-covid` / ((interval(ymd("2021-10-18"), max_date) %/% days(1)) / 30.4375),
         recovery_rate = post_covid_monthly / pre_covid_monthly)

# Order airports by recovery rate for viz
flight_data$airport <- factor(flight_data$airport)
flight_data
flight_data$airport <- fct_reorder(flight_data$airport, -flight_data$recovery_rate)
    

## Data viz --------------------
# Set theme
theme_set(theme_minimal(base_family = "Lato"))
theme_update(axis.title = element_blank(),
             plot.background = element_rect(fill = "grey98", color = "grey98"),
             panel.background = element_rect(fill = "grey98", color = "grey98"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),
             axis.line.y.left = element_blank(),
             panel.spacing.x = unit(0.8, "cm" ),
             panel.spacing.y = unit(0.1, "cm" ),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(),
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),
             strip.text = element_text(family = "Lato", size = 8, face = "bold", hjust = 0.5, color = "grey30"),
             strip.background = element_rect(fill = "grey98", color = "grey98"),
             legend.position = "bottom",
             legend.background = element_rect(fill = "grey98", color = "grey98"),
             legend.title = element_text(size = 10, hjust = 0.5, face = "bold", color = "grey30"),
             legend.text = element_text(family = "Lato", size = 8, color = "grey30"),
             legend.margin = margin(t = 0),
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
ggplot(flight_data, aes(fill = recovery_rate, ymin = 0, ymax = recovery_rate, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymin = 0, ymax = 1, xmax = 2, xmin = 1), fill = "grey98", color = "grey20", size = 0.3) +
  geom_rect() + 
  geom_segment(aes(y = 1, yend = 1, x = 0.5, xend = 2), color = "grey20", size = 0.6) +
  coord_polar(theta = "y", start = 3 * pi / 2) +
  xlim(0, 2) +
  ylim(0, 2) +
  facet_wrap(~ airport, labeller = labeller(airport = label_wrap_gen(width = 25)), nrow = 3) +
  scale_fill_gradient2(low ="#e75a49", mid = "#fcdc5c", high = "#bbe576",
                       midpoint = 0.65, labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Flight Traffic Recovery Rate",
                               title.position = "top",
                               barwidth = unit(75, units = "mm"),
                               barheight = unit(3.5, unit = "mm"))) +
  labs(title = "EU Flight Traffic Recovery",
       subtitle = "The graph depicts the recovery rates of pre-COVID-19 commercial flight volume for the busiest airport in 18 major European\nUnion countries. A full gauge would indicate a 100% flight traffic recovery rate, meaning that since October 18th, 2021 an\nairport has averaged the same number of monthly flights as it did before March 8th, 2020.",
       caption = "Visualization: Anthony Chiado  •  Data: Eurocontrol  •  Code: atchiado/tidytuesday on GitHub  • Created for R4DS #tidytuesday")

  