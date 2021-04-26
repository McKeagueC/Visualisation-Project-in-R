#### Inspired by u/gkaramanis visualisation on volcano eruptions ####

#Load relevant libraries
library(tidyverse)
library(scales)
library(cowplot)
library(ggforce)

#Read in dataset
terror <- read.csv(file.choose())

#Filter dataset by year
terror10 <- filter (terror, iyear > 2015)
world_map <- map_data("world")

#Create map plot 
map_plot <- ggplot(terror10) +
  ggtitle("Global Terrorism Attacks since 2015")+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey30", colour = "grey45", size = 0.1) +
  geom_point(aes(longitude, latitude, colour = attacktype1_txt), size = 0.7, shape = 17, alpha = 0.8) +
  scale_colour_viridis_d(option = "magma", guide = guide_legend(title = "Primary Attack Type", nrow = 6, title.position = "top", keyheight = 0.7, keywidth = 0.8, override.aes = list(size = 1.2))) +
  coord_fixed() +
  labs(caption = "Terrorist Attacks | Source: The Global Terrorism Database | Graphic: Callum McKeague") +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    plot.background = element_rect(fill = "grey45", colour = "grey45"),
    legend.position = c(0.14, 0.22),
    legend.text = element_text(colour = "grey90", size = 6.5),
    legend.title = element_text(hjust = 0.02, colour = "grey90", size = 7, margin = margin(0, 0, 5, 0)),
    legend.spacing.x = unit(0.3, 'cm'),
    legend.spacing.y = unit(-0.1, 'lines'),
    plot.caption = element_text(colour = "grey90", size = 5, hjust = 0.5),
    plot.margin = margin(5, 25, 5, 25),
    plot.title = element_text(colour = "grey90", size = 15)
  )

#Form data for attack bar chart
attacks <- terror10 %>%
  group_by(attacktype1_txt) %>% 
  summarise(n=n()) %>% 
  mutate(
    freq = n / sum(n),
    attacktype1_txt = str_replace(attacktype1_txt, " / ", "\n")
  ) %>% 
  top_n(5, freq) %>% 
  mutate(attacktype1_txt = fct_reorder(attacktype1_txt, freq))

#Create attackplot from attacks data
attackplot <- ggplot(attacks) +
  geom_col(aes(x = freq, y = attacktype1_txt), fill = "grey90", width = 0.6) +
  geom_text(aes(x = freq, y = attacktype1_txt, label = percent(freq, accuracy = 0.1)), hjust = 0, nudge_x = 0.01, family = "IBM Plex Mono", size = 2, colour = "grey90") +
  geom_text(aes(x = freq, y = attacktype1_txt, label = attacktype1_txt), hjust = 0, vjust = 0.5, family = "IBM Plex Mono", size = 1.8, colour = "grey90", nudge_x = 0.12, lineheight = 0.9) +
  xlim(0, 1) +
  labs(title = toupper("Most common attack types")) +
  theme_void(base_family = "IBM Plex Sans", base_size = 5) +
  theme(
    plot.title = element_text(colour = "grey90", family = "IBM Plex Sans Bold", hjust = 0.08, margin = margin(0, 0, 2, 0))
  )

#Form data for list of countries with the most attacks
countries_list <- terror10 %>% 
  count(country_txt, sort = TRUE) %>% 
  top_n(10, n) %>% 
  mutate(country_txt = fct_reorder(country_txt, n))

#Create plot using geom_text with data from countries_list
countries_list_plot <- ggplot(countries_list) +
  geom_text(aes(x = 0, y = country_txt, label = paste0(country_txt, " ", n)), family = "IBM Plex Sans", size = 2, hjust = 1, colour = "grey90") +
  labs(title = toupper("Countries with\nthe most attacks")) +
  xlim(-1, 0) +
  theme_void(base_size = 5) +
  theme(
    plot.title = element_text(colour = "grey90", family = "IBM Plex Sans Bold", hjust = 0.93, margin = margin(0, 0, 3, 0))
  )

#Combine all 3 plots and save as a png to current folder
ggdraw(map_plot) +
  draw_plot(attackplot, x = 0.005, y = 0.40, height = 0.2, width = 0.3) +
  draw_plot(countries_list_plot, x = 0.79, y = 0.47, height = 0.24, width = 0.2) +
  ggsave('test34.png', dpi = 370, width = 12, height = 5.9)
