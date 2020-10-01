library(skimr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(tvthemes)
library(maditr)
library(ggwordcloud)
library(png)
library(jpeg)

library(showtext)
## Add the font with the corresponding font faces
font_add("Slayer",
         regular = "slayer11.TTF")
## Automatically use showtext to render plots
showtext_auto() ## You need to do this at the beginning of a session.

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')


skim(avatar)

ladies = avatar %>% 
  filter(character %in% c("Katara",
                          "Toph",
                          "Azula",
                          "Suki",
                          "Ty Lee",
                          "Mai"))

original_seasons <- ladies %>%
  unnest_tokens(word, full_text)

tidy_seasons = original_seasons %>% 
  anti_join(get_stopwords())


positive <- get_sentiments("bing")

positive_words = tidy_seasons %>%
  semi_join(positive) %>%
  group_by(character) %>% 
  add_count(word, sort = TRUE) 

data = left_join(positive_words, scene_description) %>% 
  filter(grepl("*bend*", scene_description) |
           grepl("*bend*", character_words) |
           character %in% c("Suki",
                            "Mai", 
                            "Ty Lee", 
                            "Azula"))


get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

azula <- get_png("week 33/azula.png")
katara = get_png("week 33/katara.png")
tylee = get_png("week 33/tylee.png")
toph = get_png("week 33/toph.png")
mai = get_png("week 33/mai.png")
suki = get_png("week 33/suki.png")


peng_background <- readJPEG("week 33/background.jpg")


data %>% 
  group_by(chapter) %>%
  mutate(mean = mean(imdb_rating)) %>% 
  ggplot(aes(x = mean,
             y = fct_reorder(character, 
                         desc(mean)),
             alpha = character,
             fill = character,
             color = character
             )) +
  # background_image(peng_background) + 
  geom_density_ridges(scale = 3) +
  scale_x_continuous(limits = c(7.3,10.3)) +
  scale_alpha_manual(values = c(.4, .4,.4, .4, .4, .4), guide = F) +
  scale_color_manual(values = c("#a10000",
                                "#5897FA",
                                "#722B0D",
                                "#6C9753",
                                "#3C501D",
                                "#F2846D")) +
  scale_fill_manual(values = c("#a10000",
                               "#5897FA",
                               "#722B0D",
                               "#6C9753",
                               "#3C501D",
                               "#F2846D")) + 
   theme_avatar(
    text.font = "Slayer",
    legend.position = "none"
  ) +
  labs(y = " ", 
       x = "Mean IMBD Ratings") +
  ggtitle(label = "Avatar Featured Badass Femme Fighters", 
          subtitle  = "Toph Appeared in the Most Badass Episodes") + 
  annotation_custom(azula,
                    xmin = 9,
                    xmax = 10.3,
                    ymin = 2,
                    ymax = 4.5) + 
  annotation_custom(katara,
                    xmin = 7,
                    xmax = 8,
                    ymin = 6,
                    ymax = 8) 
  # annotation_custom(mai,
  #                   xmin =  8.3,
  #                   xmax = 8.7,
  #                   ymin = 4,
  #                   ymax = 6) 
  # annotation_custom(tylee,
  #                   xmin = 9.7,
  #                   xmax = 10.1,
  #                   ymin = 7.2,
  #                   ymax = 4.98) +
  # annotation_custom(toph,
  #                   xmin = 7.9,
  #                   xmax = 8.3,
  #                   ymin = .99,
  #                   ymax = 2.7) 
  # annotation_custom(suki,
  #                   xmin = 8.7,
  #                   xmax = 9.2,
  #                   ymin = 3.2,
  #                   ymax = 6)
  # 


