library(tidyverse)
library(ggtext)
library(mdthemes)
library(RColorBrewer)
library(cowplot)

options(scipen=999)

## fonts 
library(showtext)
## Add the font with the corresponding font faces
font_add("Embryotic", regular = "Embryotic-ZVqJl.ttf")
font_add("Filetto", regular = "filetto_regular.ttf")
## Automatically use showtext to render plots
showtext_auto()


## import data

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')

sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv') %>% 
  filter(artist == "Beyoncé")

charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv') %>% 
  filter(artist == "Beyoncé")

### mutate data 
sales2 = sales %>% 
  mutate(title2 = title,
         sales2 = 5500000) %>% 
  add_row(title2 = "1", sales = 0) %>% 
  add_row(title2 = "2", sales = 0) %>% 
  filter(country == "US" |
         is.na(country))


## racetrack plot 
sales2 %>% 
  ggplot() + 
  geom_bar(aes(x = title, y = sales2), 
           fill = "lightgray", stat = "identity", width = 1,
           alpha= .8) +
  geom_bar(aes(x = title2, y = sales,
               fill = title), 
           width = 1, stat="identity") + 
  coord_polar(theta = "y") +
  ylim(c(0, 5500000)) + 
  geom_text(data = sales2, hjust = .7, size = 3, angle = 270,
            color = "white", family = "mono", fontface = "bold",
            aes(x = title2, y = 1500000, label = title)) +
  theme_minimal() +
  labs(title = "Beyoncé in the US",
       subtitle = "Sales out of 5.5 million") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.subtitle = element_text(color = "white",
                                     family = "mono",
                                     size = 15), 
        plot.title = element_text(color = "white",
                                  family = "mono",
                                  face = "bold",
                                  size = 20),
        plot.background = element_rect(fill = "black"))


## mutate data 
sales3 = sales %>% 
  group_by(country) %>% 
  mutate(
         sales2 = 6000000,
         title2 = fct_reorder(title, sales)) %>% 
  filter(country == "US" |
           country == "UK" |
           is.na(country)) %>% 
  mutate(title2 = as.numeric(title2),
         sales3 = paste(format(round(sales / 1e6, 1), trim = TRUE), "M"))


number_of_bar <- nrow(sales3)
angle <- 90 - 360 * (sales3$title2) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
sales3$hjust <- ifelse( angle < -14, 1, 0)
sales3$angle <- ifelse(angle < -90, angle+180, angle)


## polar plot
plot = sales3 %>% 
ggplot(aes(x=factor(country), 
           y=sales)) +  
  ## gray circle
  geom_col(aes(x = country, 
               y = sales2), 
           fill = "gray",
           alpha = 0.5,
           width = 1.0004) + 
  ## fill colors 
  geom_col(aes(fill=title),
           position = "dodge",
           width = 0.5) +
  ylim(c(-1500000, 6000000)) + 
  geom_hline(yintercept = -150000, color = "gray90", size = 10) + 
  coord_polar() +
  scale_fill_manual(values = c("#CCFFCC", "#CCDDDD", "#CCAAFF", "#FFAAEE", "#FFDDDD", "#FFFFCC")) + 
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.margin = unit(rep(0,8), "pt") ,
    plot.background = element_rect(fill = "black"),
    panel.background = element_blank(),
    strip.background = element_blank(),
    
  )


plot


### ANNOTATIONS :P
canvas = ggdraw(plot) +
  draw_label("Beyoncé in the US and UK", x = 0.31, y = 0.70, hjust = 0, vjust = 0,
             fontfamily = "Embryotic", fontface = "bold", size = 40,
             color = "white") +
  draw_label("4", x = 0.38, y = 0.35, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#CCFFCC") + 
  draw_label("B'Day", x = 0.38, y = 0.325, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#CCDDDD") + 
  draw_label("Beyoncé", x = 0.38, y = 0.3, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#CCAAFF") + 
  draw_label("Dangerously in Love", x = 0.38, y = 0.275, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#FFAAEE")+ 
  draw_label("I Am... Sasha Fierce", x = 0.38, y = 0.25, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#FFDDDD") + 
  draw_label("Lemonade", x = 0.38, y = 0.225, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#FFFFCC")+
  draw_label(paste(sales3$sales3[sales3$title=="4" & sales3$country == "US"], ", ",sales3$sales3[sales3$title=="4" & sales3$country == "UK"], sep = ""), x = 0.55, y = 0.35, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#CCFFCC")+
  draw_label(paste(sales3$sales3[sales3$title=="B'Day" & sales3$country == "US"], ", ",sales3$sales3[sales3$title=="B'Day" & sales3$country == "UK"], sep = ""), x = 0.55, y = 0.325, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#CCDDDD") +
  draw_label(paste(sales3$sales3[sales3$title=="Beyoncé" & sales3$country == "US"], ", ",sales3$sales3[sales3$title=="Beyoncé" & sales3$country == "UK"], sep = ""), x = 0.55, y = 0.3, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#CCAAFF") +
  draw_label(paste(sales3$sales3[sales3$title=="Dangerously in Love" & sales3$country == "US"], ", ",sales3$sales3[sales3$title=="Dangerously in Love" & sales3$country == "UK"], sep = ""), x = 0.55, y = 0.275, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#FFAAEE") + 
  draw_label(paste(sales3$sales3[sales3$title=="I Am... Sasha Fierce" & sales3$country == "US"], ", ",sales3$sales3[sales3$title=="I Am... Sasha Fierce" & sales3$country == "UK"], sep = ""), x = 0.55, y = 0.25, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#FFDDDD") + 
  draw_label(paste(sales3$sales3[sales3$title=="Lemonade" & sales3$country == "US"], ", ",sales3$sales3[sales3$title=="Lemonade" & sales3$country == "UK"], sep = ""), x = 0.55, y = 0.225, hjust = 0, vjust = 0,
             fontfamily = "Filetto", fontface = "bold", size = 20,
             color = "#FFFFCC") 

canvas + 
  geom_curve(aes(x=.55,y=.35,xend=.45,yend=.4),
                    curvature = -0.3, 
                    arrow = arrow(type="closed",length = unit(0.25,"cm")),
                    color = "gray90",
                    size = 1) +
  geom_curve(aes(x=.63,y=.37,xend=.57,yend=.58),
             curvature = 0.8, 
             arrow = arrow(type="closed",length = unit(0.25,"cm")),
             color = "gray90",
             size = 1)
