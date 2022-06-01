library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(ggrepel)
library(showtext)
font_add_google("Lato")
showtext_auto()


## Load data ---------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 22)
poll_data <- tuesdata$poll
reputation_data <- tuesdata$reputation


## Clean data ---------------
# Rename 2022_rq variable
names(poll_data)[names(poll_data) == "2022_rq"] <- "rq_2022"

# Remove NA values from rq variable
rq_data <- poll_data[!is.na(poll_data$rq),]


## Create viz ---------------
# Define companies to be highlighted on graph
highlights <- c("Amazon.com", "Apple", "Facebook", "Google", "Microsoft", "Twitter")

# Create group column to identify highlighted vars
grouped_rq_data <- rq_data %>%
                     mutate(group = if_else(company %in% highlights, company, "other"),
                            group = as.factor(group)) %>%
                     mutate(group = fct_relevel(group, "other", after = Inf),
                            name_lab = if_else(year == 2021, company, NA_character_))

# Define graph theme
theme_update(axis.title = element_blank(),
             axis.text = element_text(color = "grey40"),
             axis.text.x = element_text(size = 12, margin = margin(t = 2)),
             axis.text.y = element_text(size = 10, margin = margin(r = 2)),
             axis.ticks = element_line(color = "grey94", size = .5),
             axis.ticks.length.x = unit(1.3, "lines"),
             axis.ticks.length.y = unit(.7, "lines"),
             panel.grid = element_blank(),
             plot.margin = margin(10, 5, 10, 20),
             plot.background = element_rect(fill = "grey98", color = "grey98"),
             panel.background = element_rect(fill = "grey98", color = "grey98"),
             plot.title = element_text(color = "grey10", size = 20, face = "bold",
                                       margin = margin(t = 10)),
             plot.subtitle = element_text(color = "grey30", size = 12,
                                              lineheight = 1.1,
                                              margin = margin(t = 8, b = 25)),
             plot.title.position = "plot",
             plot.caption.position = "plot",
             plot.caption = element_text(color = "grey30", size = 10,
                                         lineheight = 0.5, hjust = 0,
                                         margin = margin(t = 10)),
             legend.position = "none")

# Plot viz
ggplot(grouped_rq_data %>% filter(group != "other"),
       aes(x = year, y = rq, group = company)) +
  geom_vline(xintercept = seq(2017, 2021, by = 1), color = "grey94", size = .6) +
  geom_segment(data = tibble(y = seq(40, 90, by = 10), x1 = 2017, x2 = 2021),
               aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = FALSE,
               color = "grey94", size = .6) +
  geom_line(data = grouped_rq_data %>% filter(group == "other"),
            color = "grey75", size = .6, alpha = .5) +
  geom_line(aes(color = group), size = .9) +
  geom_text_repel(aes(color = group, label = name_lab), family = "Lato",
                  fontface = "bold", size = 4, direction = "y", xlim = c(2021.2, NA),
                  hjust = 0, segment.size = .7, segment.alpha = .5,
                  segment.linetype = "dotted", box.padding = .4,
                  segment.curvature = -0.1, segment.ncp = 3, segment.angle = 20) +
  coord_cartesian(clip = "off", ylim = c(40, 90)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2017, 2021.7), 
                     breaks = seq(2017, 2021, by = 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(40, 90, by = 10),
                     labels = glue::glue("{format(seq(40, 90, by = 10))}")) +
  scale_color_manual(values = c(rcartocolor::carto_pal(n = 9, name = "Bold"), "grey50")) +
  labs(title = "5-Year Consumer Rating Trends",
       subtitle = "The chart visualizes changes in consumer RQ Rating for America's 100 most prominent companies over the last five years, with notable companies
highlighted. RQ Rating is a measure of the Axios Harris Poll 100, which is published by Axios annually as a way of gauging public market perceptions.
Social media companies are viewed poorly by the public in comparison to other notable tech companies.",
       caption = "Visualization: Anthony Chiado  •  Data: Axios and Harris  •  Code: atchiado/tidytuesday on GitHub  • Created for R4DS #tidytuesday")


