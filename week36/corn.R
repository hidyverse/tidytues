library(tidyverse)
library(ggdist)
library(grid)
library(png)
library(ggtext)
library(ragg)
library(pdftools)
library(ggimage)
library(showtext)

## Add the font with the corresponding font faces

font_add("Filetto", regular = "filetto_regular.ttf")
## Automatically use showtext to render plots
showtext_auto()
## ggplot theme
theme_set(theme_minimal(base_family = "Filetto", base_size = 16))

theme_update(
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  plot.background = element_rect(color = NA, fill = "#faf9f5")
)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')

long_crops <- key_crop_yields %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower())




long_cropland <- arable_land %>% 
  select(code = Code,
         year = Year,
         entity = Entity,
         land = `Arable land needed to produce a fixed quantity of crops ((1.0 = 1961))`) %>% 
 right_join(long_crops, by = c("entity", "year", "code"))

summary(long_crops)

long_cropland2 = long_cropland %>%
  filter(code == "USA") %>% 
  group_by(crop, year) %>% 
  mutate(meanx = mean(crop_production, na.rm = T)) %>% 
  group_by(crop) %>% 
  mutate(med = median(crop_production, na.rm =T),
         min = min(crop_production),
         max = max(crop_production)) %>% 
  ungroup() %>% 
  mutate(crop = fct_reorder(crop,med),
         y = as.numeric(crop) - 0.3) %>% 
  filter(!is.na(meanx)) 

long_cropsum = long_cropland2 %>% 
  group_by(crop) %>% 
  slice(1) 

cropyear = long_cropland2 %>% 
  group_by(crop) %>% 
  arrange(year, crop_production) %>% 
  slice(1)


long_cropland3 = long_cropland2 %>% 
  mutate(img = "tractor.png")

ggplot(long_cropland3,
      aes(x = meanx, 
          y = y
          )) +
  #### text crop
  geom_text(
    data = long_cropsum,
    aes(
      x = min, 
      y = y,
      label = crop
    ),
    fontface = "bold",
    vjust = 0,
    hjust = -0.01,
    size = 10,
    color = "#d9bf77"
  ) +
  ## stripe
  stat_interval(
    aes(y = y - .05),
    orientation = "horizontal",
    .width = c(.25, .5, .95, 1),
    stroke = 0,
    size = 4
  )  + 
  ## dots
  stat_dots(
    quantiles = NA,
    orientation = "horizontal",
    normalize = "none",
    scale = .87,
    alpha = .8,
    color = "#d8ebb5",
    fill = "#d8ebb5"
  ) +
  scale_x_log10() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(
    values = c("#d9bf77", "#d8ebb5", "#639a67", "#2b580c")
  ) +
  geom_image(aes(x = max,
                 y = y + .18,
                 image = img),
             size = .05)


