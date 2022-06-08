library(tidyverse)
library(ggtext)
library(grid)
library(shadowtext)
library(showtext)
font_add_google("Lato")
showtext_auto()


## Load data ---------------
tuesdata <- tidytuesdayR::tt_load('2022-06-07')
static_list <- tuesdata$static_list


## Clean data --------------
top_companies <- static_list %>%
                   rename(Contributions = 'Amount Contributed Across States',
                          Pride = 'Pride?') %>% 
                   subset(Contributions >= 45000 & Company!="Grand Total")


## Create viz ---------------
# Set theme
theme_set(theme_minimal(base_family = "Lato"))

theme_update(axis.title = element_blank(),
             axis.text = element_text(color = "grey40"),
             plot.background = element_rect(fill = "grey98", color = "grey98"),
             panel.background = element_rect(fill = "grey98", color = "grey98"),
             panel.grid.major.x = element_line(color = "grey80", size = 0.3),
             axis.ticks.length = unit(0, "mm"),
             axis.line.y.left = element_line(color = "black"),
             axis.text.y = element_blank(),
             axis.text.x = element_text(family = "Lato", size = 16))
  
# Plot data
ggplot(top_companies, aes(Contributions, Company)) +
  geom_col(width = 0.6) +
  scale_x_continuous(limits = c(0, 601500),
                     breaks = seq(0, 601500, by = 1000), 
                     expand = c(0, 0),
                     position = "top") +
  scale_y_discrete(expand = expansion(add = c(0, 0.5)))
