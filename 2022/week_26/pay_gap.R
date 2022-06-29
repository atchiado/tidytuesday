library(tidyverse)
library(ggtext)
library(tools)
library(showtext)
font_add_google("Lato")
showtext_auto()

## Load data ---------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 26)
paygap <- tuesdata$paygap


## Data cleaning ---------------
# Select universities to be displayed in the graph
school_names <- c("Durham University", "Imperial College London", "King's College London",
                  "THE UNIVERSITY OF MANCHESTER", "University of Cambridge", "University of London",
                  "University of Oxford", "University of St Andrews", "University of York")

# Wrangle data to tidy format
pay_data <- paygap %>%
  filter(employer_name %in% school_names) %>%
  select(employer_name, male_lower_quartile, female_lower_quartile,
         male_lower_middle_quartile,female_lower_middle_quartile, male_upper_middle_quartile,
         female_upper_middle_quartile, male_top_quartile, female_top_quartile) %>%
  pivot_longer(cols = male_lower_quartile:female_top_quartile,
               names_to = c("gender", "quartile"),
               names_pattern = "(.*?)_(.*)",
               values_to = "proportion") %>%
  group_by(employer_name, gender, quartile) %>% 
  summarise(proportion = mean(proportion))

# Order quartile factor levels
pay_data$quartile <- as.factor(pay_data$quartile)
pay_data$quartile <- factor(pay_data$quartile, levels = c("lower_quartile", "lower_middle_quartile",
                                                          "upper_middle_quartile", "top_quartile"))
levels(pay_data$quartile) <- c("Bottom", "Lower Middle", "Upper Middle", "Top")

# Case correction for some variables
pay_data$employer_name <- recode(pay_data$employer_name,
                                 "THE UNIVERSITY OF MANCHESTER" = "The University of Manchester")
pay_data$gender <- str_to_title(pay_data$gender)


## Data viz ---------------
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
             axis.text.y = element_text(family = "Lato", size = 9),
             axis.text.x = element_text(family = "Lato", size = 9),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(),
             strip.text = element_text(family = "Lato", size = 11, hjust = 0.5, color = "grey30"),
             strip.background = element_rect(fill = "grey90"),
             legend.position = "bottom",
             legend.title = element_blank(),
             legend.text = element_text(family = "Lato", size = 10, face = "bold", color = "grey40"),
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

# Set x axis labels
quartile_labs <- c("Bottom\nQuartile", "Lower\nMiddle\nQuartile",
                   "Upper\nMiddle\nQuartile", "Top\nQuartile")

# Plot data
ggplot(pay_data, aes(x = quartile, y = proportion * 100, fill = gender)) +
  geom_col(position = "fill") +
  facet_wrap( ~ employer_name, ncol = 4) +
  scale_fill_manual(values = c("Male" = "#485e92", "Female" = "#e17575")) +
  scale_x_discrete(labels = quartile_labs) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "The Gender Pay Gap at Prominent British Universities",
       subtitle = "The graph depicts the proportions of male and female university employees that make up each of four salary quartiles. In general, although most universities\nemploy more females than males, male employees make up a comparatively greater proportion of the upper quartiles, while female employees make up a\ncomparatively greater proportion of the lower quartiles. The data source tracks salaries in the United Kingdom from 2018-2022.",
       caption = "Visualization: Anthony Chiado  •  Data: UK Government's Gender Pay Gap Service  •  Code: atchiado/tidytuesday on GitHub  • Created for R4DS #tidytuesday")
  