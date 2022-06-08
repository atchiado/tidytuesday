library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(ggrepel)
library(grid)
library(shadowtext)
library(showtext)
font_add_google("Lato")
showtext_auto()


## Load data ---------------
tuesdata <- tidytuesdayR::tt_load('2022-06-07')
static_list <- tuesdata$static_list


## Clean data --------------
# Rename variables of interest
top_companies <- static_list %>%
                   rename(Contributions = 'Amount Contributed Across States',
                          Pride = 'Pride?') %>% 
                   subset(Contributions >= 70000 & Company!="Grand Total")

# Order data decreasing by contribution amount
top_companies$Company <- factor(top_companies$Company,
                                levels = top_companies$Company[order(top_companies$Contributions)])


## Create viz ---------------
# Define pride companies to be highlighted on graph
highlights <- c("Toyota", "AT&T", "Comcast", "Amazon", "FedEx", "State Farm")

# Create group column to identify highlighted vars
grouped_companies <- top_companies %>%
                       mutate(group = if_else(Company %in% highlights, Company, "other"),
                              group = as.factor(group))

# Set theme
theme_set(theme_minimal(base_family = "Lato"))

theme_update(axis.title = element_blank(),
             plot.background = element_rect(fill = "grey98", color = "grey98"),
             panel.background = element_rect(fill = "grey98", color = "grey98"),
             panel.grid.major.x = element_line(color = "grey80", size = 0.3),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             axis.ticks.length = unit(0, "mm"),
             axis.line.y.left = element_line(color = "black"),
             axis.text.y = element_blank(),
             axis.text.x = element_text(family = "Lato", size = 6),
             plot.margin = margin(10, 40, 20, 40),
             plot.title = element_text(color = "grey10", size = 25, face = "bold",
                                       margin = margin(t = 15)),
             plot.subtitle = element_markdown(color = "grey30", size = 12, lineheight = 1.35,
                                              margin = margin(t = 10, b = 20)),
             plot.title.position = "plot",
             plot.caption.position = "plot",
             plot.caption = element_text(color = "grey30", size = 8, lineheight = 1.2, 
                                         hjust = 0, margin = margin(t = 20)))
  
# Plot data
ggplot(top_companies, aes(x = Contributions, y = Company)) +
  geom_col(width = 0.6) +
  scale_x_continuous(limits = c(0, 650000), breaks = seq(0, 650000, by = 50000), 
                     expand = c(0, 0), position = "top") +
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  geom_shadowtext(data = subset(top_companies, Contributions < 250000),
                  aes(Contributions, y = Company, label = Company),
                  hjust = 0, nudge_x = 4000, color = "grey40", bg.color = "grey98",
                  bg.r = 0.5, family = "Lato", size = 4) + 
  geom_text(data = subset(top_companies, Contributions >= 250000),
            aes(0, y = Company, label = Company), hjust = 0, nudge_x = 4000,
            color = "white", family = "Lato", size = 4) +
  labs(title = "Rainbow Capitalism",
       subtitle = "Graph depicts the top donators to anti-LGBTQ politicians by Pride-supporting and non Pride-supporting companies",
       caption = "Visualization: Anthony Chiado  •  Data: Data For Progress  •  Code: atchiado/tidytuesday on GitHub  • Created for R4DS #tidytuesday")
  
