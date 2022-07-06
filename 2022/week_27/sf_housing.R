library(tidyverse)
library(ggtext)
library(showtext)
library(sf)
library(devtools)
library(urbnmapr)
library(viridis)
library(ggrepel)
font_add_google("Lato")
showtext_auto()


## Load data --------------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 27)
new_construction <- tuesdata$new_construction

# Load county data for mapping
county_data <- urbnmapr::counties


## Clean data --------------------
# Create new columns to specify production rates
new_construction$sf_rate <- new_construction$sfproduction / new_construction$totalproduction
new_construction$mf_rate <- new_construction$mfproduction / new_construction$totalproduction

# Join to county_data to get coordinates and pivot production rates for facet wrapping
construction_data <- left_join(new_construction, county_data, by = c("county" = "county_name")) %>%
  subset(select = -c(cartodb_id, the_geom, the_geom_webmercator, source, order, hole,
                     piece, state_abbv, state_fips, fips_class, state_name, mhproduction)) %>%
  rename("Single Family" = sf_rate,
         "Multi-family" = mf_rate) %>%
  pivot_longer(cols = 'Single Family':"Multi-family",
               names_to = "construction_type",
               values_to = "production_rate") %>%
  filter(year > 2008)


## Create viz --------------------
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
             strip.text = element_text(family = "Lato", size = 12, face = "bold", hjust = 0.5, color = "grey30"),
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
ggplot(construction_data, aes(long, lat, group = group, fill = production_rate)) +
  geom_polygon(color = "grey20") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  facet_wrap(~ construction_type) +
  scale_fill_viridis("Construction Rate", option = "inferno", labels = scales::percent) +
  guides(fill = guide_colorbar(title.position = "top",
                               barwidth = unit(100, units = "mm"),
                               barheight = unit(3.5, unit = "mm"))) +
  labs(title = "Bay Area Housing Development",
       subtitle = "The graph depicts construction rates for single and multi-family housing by county in the San Francisco Bay Area. Rates indicate the\npercentage of total housing production for each county that is made up by each specific type of housing. Negative values indicate\nthat a given county reduced the amount of the specified type of housing over the evaluation period from 2008-2018.",
       caption = "Visualization: Anthony Chiado  •  Data: data.sfgov.org & Kate Pennington  •  Code: atchiado/tidytuesday on GitHub  • Created for R4DS #tidytuesday")

