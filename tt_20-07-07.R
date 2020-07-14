library(tidyverse)

# test
# Load and format data ----

# Code taken from TidyTuesday repo to get started
raw_arabica <- read_csv("https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/arabica_data_cleaned.csv") %>% 
  janitor::clean_names()

raw_robusta <- read_csv("https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/robusta_data_cleaned.csv",
                        col_types = cols(
                          X1 = col_double(),
                          Species = col_character(),
                          Owner = col_character(),
                          Country.of.Origin = col_character(),
                          Farm.Name = col_character(),
                          Lot.Number = col_character(),
                          Mill = col_character(),
                          ICO.Number = col_character(),
                          Company = col_character(),
                          Altitude = col_character(),
                          Region = col_character(),
                          Producer = col_character(),
                          Number.of.Bags = col_double(),
                          Bag.Weight = col_character(),
                          In.Country.Partner = col_character(),
                          Harvest.Year = col_character(),
                          Grading.Date = col_character(),
                          Owner.1 = col_character(),
                          Variety = col_character(),
                          Processing.Method = col_character(),
                          Fragrance...Aroma = col_double(),
                          Flavor = col_double(),
                          Aftertaste = col_double(),
                          Salt...Acid = col_double(),
                          Balance = col_double(),
                          Uniform.Cup = col_double(),
                          Clean.Cup = col_double(),
                          Bitter...Sweet = col_double(),
                          Cupper.Points = col_double(),
                          Total.Cup.Points = col_double(),
                          Moisture = col_double(),
                          Category.One.Defects = col_double(),
                          Quakers = col_double(),
                          Color = col_character(),
                          Category.Two.Defects = col_double(),
                          Expiration = col_character(),
                          Certification.Body = col_character(),
                          Certification.Address = col_character(),
                          Certification.Contact = col_character(),
                          unit_of_measurement = col_character(),
                          altitude_low_meters = col_double(),
                          altitude_high_meters = col_double(),
                          altitude_mean_meters = col_double()
                        )) %>% 
  janitor::clean_names() %>% 
  rename(acidity = salt_acid, sweetness = bitter_sweet,
         aroma = fragrance_aroma, body = mouthfeel,uniformity = uniform_cup)


all_ratings <- bind_rows(raw_arabica, raw_robusta) %>% 
  select(-x1) %>% 
  select(total_cup_points, species, everything())

all_ratings %>% 
  skimr::skim()

# Modify data ----

df_mod <- all_ratings %>% 
  mutate(grading_year = as.integer(str_sub(all_ratings$grading_date, -4,-1)),
         is_lbs = str_detect(bag_weight, 'lbs'), # Detect when in lbs
         bag_weight_numeric = as.numeric(gsub("([0-9]+).*$", "\\1", bag_weight)), # Keep only numerics
         bag_weight_kg = round(as.numeric(ifelse(is_lbs == TRUE, # Get all weights in kg (two decimals)
                                                 bag_weight_numeric * 0.45359237, 
                                                 bag_weight_numeric)), digits = 2),
         total_weight = bag_weight_kg * number_of_bags, # Get total weight used for measuring
         total_weight_log = log(total_weight),
         country_of_origin = case_when(grepl('United States', country_of_origin) ~ 'United States', # Fix the US states
                                       grepl('Tanzania', country_of_origin) ~ 'Tanzania',
                                       grepl('Cote ', country_of_origin) ~ 'Cote de Ivoire', 
                                       TRUE ~  as.character(country_of_origin)))


# Filter and group to get data in format for plot ----
df_bar_plot <- df_mod %>% 
  filter(country_of_origin != 'NA') %>% 
  filter(total_weight > 0) %>% 
  group_by(country_of_origin) %>% 
  summarise(sum_total_weight = sum(total_weight_log),
            mean_cup_points = mean(total_cup_points))


# Plot total coffee ----

df_bar_plot %>% 
  ggplot(aes(reorder(country_of_origin, sum_total_weight, sum), sum_total_weight, fill = country_of_origin)) +
  geom_col() +
  coord_flip() + 
  theme_classic() +
  ggtitle('Total weight of coffee per country wasted for rating purposes') +
  ylab('Total weight of rated coffee in kg (log scale)') +
  labs(caption = 'Data source: Coffee Quality Database \n Twitter: @marcuskleverman') +
  theme(plot.background = element_rect(fill = '#0a0a0a'),
        panel.background = element_blank(),
        plot.title = element_text(colour = 'white', hjust = 0.5, size = 20),
        plot.caption = element_text(colour = 'white'),
        axis.text.x= element_text(color = 'white'), 
        axis.text.y= element_text(color = 'white'),
        axis.title.x = element_text(colour = 'white'),
        axis.line = element_line(colour = 'white'),
        axis.ticks = element_line(colour = 'white'),
        legend.position = 'none',
        panel.grid.minor = element_line(colour = 'white'))

# Save plot
ggsave('total_coffe.png', dpi = 400)

# New plot -----