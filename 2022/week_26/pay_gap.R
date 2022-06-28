library(tidyverse)

## Load data ---------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 26)
paygap <- tuesdata$paygap


## Data cleaning ---------------
school_ids <- c("13185", "15943", "13172", "14396", "6539", "296", "12620", "12943", "14890")

pay_data <- paygap %>%
  filter(employer_id %in% school_ids) %>%
  select(employer_name, employer_id, diff_mean_hourly_percent, diff_median_hourly_percent,
         diff_mean_bonus_percent, diff_median_bonus_percent, male_bonus_percent,
         female_bonus_percent, male_lower_quartile, female_lower_quartile,
         male_lower_middle_quartile,female_lower_middle_quartile, male_upper_middle_quartile,
         female_upper_middle_quartile, male_top_quartile, female_top_quartile, date_submitted)


## Data viz ---------------
# Set theme
theme_set(theme_minimal(base_family = "Lato"))

theme_update(axis.title = element_blank(),
             plot.background = element_rect(fill = "grey98", color = "grey98"),
             panel.background = element_rect(fill = "grey98", color = "grey98"),
             panel.grid.major.x = element_line(color = "grey80", size = 0.3),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             axis.ticks.length = unit(0, "mm"),
             axis.line.y.left = element_line(color = "grey30"),
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
ggplot(pay_data, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(position = "fill")