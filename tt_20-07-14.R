# Library ----

library(tidyverse)
library(janitor)
library(gganimate)

# Load data ----
tuesdata <- tidytuesdayR::tt_load(2020, week = 29)

astronauts <- tuesdata$astronauts

# Clean data ----

# Taken from tt repo
astronauts <- astronauts %>% 
  clean_names() %>% 
  filter(!is.na(number)) %>%  # remove last row (all values are NA)
  mutate(
    military_civilian = if_else(military_civilian == "Mil", "military", "civilian")
  )

# Add features ----

astronauts <- astronauts %>% 
  mutate(age_on_mission = year_of_mission - year_of_birth) %>% 
  select(year_of_mission, age_on_mission, sex, hours_mission)

# Plot ----

my_animation <- astronauts %>% 
  ggplot(aes(x = year_of_mission, y = age_on_mission, colour = as.factor(sex))) +
  geom_point(aes(size = hours_mission, 
                 alpha = 0.1)) +
  theme_minimal() +
  guides(alpha = 'none',
         size = guide_legend(override.aes = list(colour = '#c95136'))) +
  labs(size = 'Hours per \nmission', colour = 'Gender') +
  xlab('Year of mission') +
  ylab('Age of astronaut') +
  labs(title = 'Age of astronaut on mission and\nthe length of mission (in hours)\n',
       caption = 'Source: https://data.mendeley.com/datasets/86tsnnbv2w/1 | TidyTuesday | by @marcuskleverman') +
  theme(plot.background = element_rect(fill = '#0a0a0a'),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(colour = 'white', hjust = 0.5, size = 20),
        plot.caption = element_text(colour = 'white'),
        axis.text.x= element_text(color = 'white'), 
        axis.text.y= element_text(color = 'white'),
        axis.title.x = element_text(colour = 'white'),
        axis.title.y = element_text(colour = 'white'),
        axis.line = element_line(colour = 'white'),
        axis.ticks = element_line(colour = 'white'),
        legend.text = element_text(colour = 'white'),
        legend.title = element_text(colour = 'white')) +
  transition_time(year_of_mission) +
  shadow_mark()


animate(my_animation, width = 8, height = 6, units = "in", res = 150)

anim_save("astronaut.gif")
