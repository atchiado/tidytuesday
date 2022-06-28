library(tidyverse)

## Load data ---------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 26)
paygap <- tuesdata$paygap


## Data cleaning ---------------
company_ids <- c("6366", "1551", "14262", "549", "399", "657", "5126", "1089", "1203", "17044",
                 "11241", "12141", "826", "14843", "7623", "8236", "8868", "16")

pay_data <- paygap %>%
  filter(employer_id %in% company_ids) %>%
  select(employer_name, employer_id, diff_mean_hourly_percent, diff_median_hourly_percent,
         diff_mean_bonus_percent, diff_median_bonus_percent, male_bonus_percent,
         female_bonus_percent, male_lower_quartile, female_lower_quartile,
         male_lower_middle_quartile,female_lower_middle_quartile, male_upper_middle_quartile,
         female_upper_middle_quartile, male_top_quartile, female_top_quartile)
