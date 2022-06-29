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
school_names <- c("Durham University", "Imperial College London", "King's College London",
                  "THE UNIVERSITY OF MANCHESTER", "Trinity College", "University of Cambridge",
                  "University of London", "University of Oxford", "University of York")

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

pay_data$quartile <- as.factor(pay_data$quartile)
pay_data$quartile <- factor(pay_data$quartile, levels = c("lower_quartile", "lower_middle_quartile",
                                                          "upper_middle_quartile", "top_quartile"))
levels(pay_data$quartile) <- c("Bottom", "Lower Middle", "Upper Middle", "Top")

pay_data$employer_name <- str_to_title(pay_data$employer_name)


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
             panel.spacing.y = unit(0.5, "cm" ),
             axis.text.y = element_blank(),
             axis.text.x = element_text(family = "Lato", size = 8),
             plot.margin = margin(10, 40, 20, 40),
             plot.title = element_text(color = "grey10", size = 25, face = "bold",
                                       margin = margin(t = 15)),
             plot.subtitle = element_text(color = "grey30", size = 12, lineheight = 1.35,
                                              margin = margin(t = 10, b = 20)),
             plot.title.position = "plot",
             plot.caption.position = "plot",
             plot.caption = element_text(color = "grey30", size = 8, lineheight = 1.2, 
                                         hjust = 0, margin = margin(t = 20)),
             legend.position = "none")

# Plot data
ggplot(pay_data, aes(x = quartile, y = proportion, fill = gender)) +
  geom_col(position = "fill") +
  facet_wrap( ~ employer_name, ncol = 4) +
  scale_fill_manual(values = c("male" = "#519481", "female" = "#e7b96e"))

                    