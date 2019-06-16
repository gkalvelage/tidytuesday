library("tidyverse")
library("here")
library("maps")

# load data
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

# check data
glimpse(meteorites)
sapply(meteorites, function(x) sum(is.na(x)))

# remove NAs, filter location and time, convert mass
meteorites_clean <- meteorites %>%
  drop_na(geolocation, year, mass) %>%
  filter(long >= -11 & long <= 32 & lat >= 38 & lat <= 70) %>%
  filter(year >= 1913 & year <= 2013) %>%
  mutate(mass_kg = mass / 1000)

# plot meteorites over time
ggplot(data = meteorites_clean, aes(x = year, y = mass_kg, color = fall)) +
  geom_point() +
  guides(color = guide_legend(title = NULL)) +
  xlab("Year") +
  ylab("Mass (in kg)") +
  labs(title = "Known Meteorite Impacts in Europe (1913-2013)",
       subtitle = paste("There are", nrow(meteorites_clean), "recorded impacts",
                        "in Europe for the past 100 years."),
       caption = "Data: NASA | Graphic/Code: @gkalvelage") +
  theme_classic()

ggsave("2019-06-11_meteorites_timeline.png", path = here("figures"), dpi = 320)

# get world map
world <- map_data("world")

# map meteorite impacts
ggplot(data = world, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey30",  color = "white", size = 0.1) +
  geom_point(data = meteorites_clean, 
             aes(x = long, y = lat, color = mass_kg, size = mass_kg, group = NULL), 
             shape = 19) +
  scale_size_continuous(name = "Mass (in kg)") +
  scale_color_gradient(name = "Mass (in kg)", low = "#FD8D3C", high = "#800026") +
  guides(color = guide_legend()) +
  geom_label(data = filter(meteorites_clean, mass_kg >= 100), 
            aes(x = long, y = lat, group = NULL, label = name), 
            size = 3, vjust = 0, hjust = 1, nudge_x = -.3, 
            label.padding = unit(.15, "lines")) +
  coord_fixed(xlim = c(-11, 32),  ylim = c(38, 70), ratio = 1.3) +
  labs(title = "Known Meteorite Impacts in Europe (1913-2013)",
       subtitle = paste("With few exceptions, meteorites were very small.",
                        "Names shown for meteorites \u2265 100 kg."),
       caption = "Data: NASA | Graphic/Code: @gkalvelage") +
  theme_classic() + 
  theme(axis.title = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        strip.text = element_text(size = 12),
        panel.border = element_rect(fill = NA))

ggsave("2019-06-11_meteorites_map.png", path = here("figures"), scale = 2, dpi = 320,
       width = 4)
