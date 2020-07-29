penguins.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

library(tidyverse)
library(skimr)
library(extrafont)
library(ggpubr)
library(grid)
# font_import()
   

library(showtext)
## Add the font with the corresponding font faces
font_add("Waltograph", regular = "waltograph42.otf")
font_add("Filetto", regular = "filetto_regular.ttf")
## Automatically use showtext to render plots
showtext_auto()


library(jpeg)

peng_background <- readJPEG("background.jpg")



skim(penguins.csv)

peng = penguins.csv %>% 
  mutate(year = as.factor(year),
         sex = as.factor(sex)) %>%
  mutate_if(is.character, as.factor) %>% 
  drop_na()


arrows <- 
  tibble(
    x1 = c(3.13, 2.1, 1.13),
    x2 = c(3.0, 2.0, 1.0),
    y1 = c(5000, 5300, 7000), 
    y2 = c(4800, 4950, 6500)
  )


g = peng %>% 
  ggplot(aes(island, 
             body_mass_g)) +
 background_image(peng_background)  +
  geom_dotplot(binaxis = "y",
               stackdir = "center", 
               alpha = .7,
               binwidth = 150,
               aes(color = species,
               fill = species)) +
  geom_boxplot(width = 0.2,
               alpha = .5,
               aes(color = species)) +
  scale_color_manual(values = c("#005c99",
                               "#63a96a",
                               "#fad00a")) +
  scale_fill_manual(values = c("#005c99",
                                "#63a96a",
                                "#fad00a")) +
  scale_x_discrete(labels = c("Biscoe\nIsland", "Dream\nIsland",  "Torgersen\nIsland") ) + 
  coord_flip() +
  theme(legend.position = "none", 
        axis.text = element_text(family = "Filetto",
                                 size = 20)) +
  labs(y = "Body Mass (g)",x = "") +
  ylim(c(2500, 7500)) +
  annotate(
    "text",x = 1.3, y =7000, family = "Waltograph", size = 20,
    color =  "#fad00a",
    label = "Gentoo"
  ) +
  annotate(
    "text", x = 2.3, y = 6000, family = "Waltograph", size = 20,
    color =  "#63a96a",
    label = "Chinstrap"
  ) +
  annotate(
    "text", x = 3.3, y = 5000, 
   family = "Waltograph",
   size = 20, 
   lineheight = .6, 
    color =  "#005c99",
    label = "Adélie")



g2 = g + 
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 1,
    color = c("#005c99",
              "#63a96a",
              "#fad00a"), curvature = -0.3
  ) 

ggsave(filename = "rplot.png", g2)

                                   
