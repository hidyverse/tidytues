#### Week 32 - ENERGIES #### 
# energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
# country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')
library(tidytuesdayR)
library(tidyverse)
library(ggimage)
library(jpeg)
library(ggpubr)
library(cowplot)
library(ggridges)


tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types
country_totals = tuesdata$country_totals




### Hydro Plot 
watercurves =  data.frame(x = c(1,2,3,4,5,6,7,8,9,10,
                                11,12,13,14,15,16,17,18,19,20,
                                21,22,23,24, 25, 26, 27, 28, 29 ,30,
                                31,32,33,34),
                          y = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5 ,1.5, 
                                1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5 ,1.5, 1.5, 
                                1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5,
                                1.5, 1.5, 1.5, 1.5),
                          xend = c(1.5, 2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5, 10.5,
                                   11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5,
                                   21.5,22.5,23.5,24.5,25.5,26.5, 27.5, 28.5, 29.5, 30.5,
                                   31.5, 32.5, 33.5, 34.5), 
                          yend = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                                   -1,-1,-1,-1,-1,-1,-1,-1,-1, -1, 
                                   -1,-1,-1,-1,-1,-1,-1,-1,-1, -1,
                                   -1,-1,-1,-1))


plot_hydrodat = 
  energy_types %>% 
  dplyr::filter(type == "Hydro",
                country_name != "NA") %>% 
  # mutate(`2018` = if_else(type %in% c("Conventional thermal", "Geothermal", "Nuclear"), (`2018`* -1), `2018`)) %>% 
  group_by(country_name) %>% 
  mutate(meanx = log(mean(`2018` + `2017` + `2016`)))%>% 
  filter(meanx > 0) %>% 
  ungroup() %>% 
  mutate(country_name = as.factor(country_name),
         country_name= fct_reorder(country_name,meanx)) %>% 
  arrange(country_name, meanx)


watercurvesdat = bind_cols(watercurves, plot_hydrodat)




plot_hydro = plot_hydrodat %>% 
ggplot(aes(x= reorder(country_name,meanx), 
           y=meanx,
           color = `2018`)) +
  geom_segment( aes(x=country_name,
                    xend=country_name, 
                    y=1, 
                    yend=meanx),
                size = 5) +
  geom_curve(data = watercurvesdat, aes(x = x, 
                                     y = y, 
                                     xend = xend, 
                                     yend = yend,
                                     color = `2018`),
             curvature = -.1,
             size = 5)  + 
  theme_classic() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x =  element_text(angle = 60, 
                                hjust = .1, 
                                color = "#009FFD",
                                face = "bold",
                                size = 10),
    axis.text.y = element_text(color = "gray"),
    axis.ticks.y = element_line(color = "gray"),
    axis.title.y = element_text(color = "gray"),
    axis.line = element_blank(),
    legend.position = "bottom",
    legend.title  = element_text(color ="gray"),
    legend.text = element_text(color = "gray"),
    plot.margin = margin(0,20,0,0, unit = "pt")
  ) +
  xlab("") +
  ylab("Log of Mean 2018 Energy in GWh")+ 
  labs(color = "GWh (Gigawatt hours)") + 
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  scale_color_gradient(low = "#009FFD", high = "#2A2A72")


plot_hydro


### SOLAR PLOT 
# countries with nonzero 2018 ghz  
data_solar = 
  energy_types %>% 
  dplyr::filter(type == "Solar",
                country_name != "NA") %>% 
  group_by(country_name) %>% 
  mutate(meanx = log(mean(`2018` + `2017` + `2016`)),
         mean = mean(`2018`)) %>% 
  filter(meanx >= 0)

data_solar$idx = seq(1:nrow(data_solar))
labeldat = data_solar


number_of_bar = nrow(labeldat)
angle = 90-360 * 
  (labeldat$idx-.5) /
  number_of_bar

labeldat$hjustx = ifelse(angle < -90, 1, 0)
labeldat$anglex = ifelse(angle < -90,
                        angle + 180,
                        angle)



plot_solar = 
  ggplot(data = data_solar, aes(x = factor(idx), y = meanx, fill = `2018`)) + 
  geom_col(na.rm = T)+ 
  coord_polar(start = 0) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0,15,0,0, unit = "pt"),
    legend.position = "top", 
    legend.text = element_text(angle = 45, color = "gray"),
    legend.key.size = unit(.7, "cm"),
    legend.title = element_text(color = "gray")
    ) +  
  scale_fill_gradient(low = "yellow", high = "red") + 
  scale_color_gradient(low = "gold", high = "red") + 
  geom_text(data = labeldat, aes(label = country_name, 
                                 hjust = hjustx,
                                 color = `2018`), 
            angle = labeldat$anglex, 
            size = 3, 
            fontface = "bold", show.legend = F) +
  labs(fill = "Energy in GWh (Gigawatt hours)", color = "Energy in GWh (Gigawatt hours)")


plot_solar


## wind energy? 
## wind turbines?
data_wind = 
  energy_types %>% 
  dplyr::filter(type == "Wind",
                country_name != "NA") %>% 
  group_by(country_name) %>% 
  mutate(meanx = log(mean(`2018`)),
         mean = mean(`2018`)) %>% 
  filter(meanx >= 0) %>% 
  gather(key = "year", value = "value", 5:7)



windtext = data_wind %>% 
  group_by(year) %>% 
  mutate(mean = mean(value))



plot_wind = 
  ggplot(data = data_wind, aes(x = as.numeric(year), y = mean)) + 
  geom_segment(data = data_wind, aes(xend = as.numeric(data_wind$year), yend = 0),
               size = 10, color = "gray", lineend = "round")+ 
  coord_polar(start = 25) + 
  scale_x_discrete( breaks = c("2016", "2017", "2018")) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_text(color = "gray")
  ) + 
  annotate("text", label = c("8280.791", "9704.7", "10130.248"), x = c(2016, 2017, 2018) ,y =c( 80000, 80000, 80000), color = "gray20", angle = c(40, -80, -12),
           size = 3) +
  annotate("text", 
           x = c(2016.2, 2017.2, 2018.1),
           y = c(160000,150000,160000),
           label = c("2016", "2017", "2018"),
           size = 4) + 
  labs(x = "", y = "Energy in GWh (Gigawatt hours)")


plot_wind

#### total energies? 


totals = energy_types %>% 
  filter(country != "EL",
         !is.na(country_name),
         level != "Level 2") %>% 
  select(country_name, type, `2018`) %>% 
  pivot_wider(id_cols = country_name,
              names_from = type,
              values_from = `2018`) %>% 
  rowwise() %>% 
  mutate(total = rowSums(across(`Conventional thermal`:`Other`)))



energylines = data.frame(x = c(1,2,3,4,5,6,
                               1,2,3,4,5,6),
                         yx = c(8.5,8.5,10,10,11,11,
                                7.5,7.5,9,9,10,10),
                         xend = c(2,3,4,5,6,7,
                                  2,3,4,5,6,7),
                         yendx = c(8.5,10,11,11,12,12,
                                   7.5,9,10,10,11,11))


plot_all = energy_types %>% 
  select(country_name, typex = type, year = `2018`, level) %>% 
  filter(level != "Level 2") %>% 
  mutate(year = log(year) , typex = fct_reorder(typex, desc(year))) %>% 
  filter(year >0) 
  


all = ggplot() +
  geom_segment(data = plot_all,
              aes(x= reorder(typex, year),
                  xend=typex, 
                  y=1, 
                  yend= year),
              size =2)+
  geom_curve(data = energylines,
               aes(x = x,
                   xend = xend,
                   y = yx,
                   yend = yendx), 
               inherit.aes = F) +
  scale_y_log10() +
  theme_minimal()+
  labs(x = "", y ="Energy in GWh (Gigawatt hours)") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 10,
                             face= "bold"),
    axis.text.y = element_blank()
  )  +
  annotate("text",
           label = c("46.4", "34.1", "177.2", "260.9", "294.4", "347.3", "153.6"),
           x = c(1.1,2.1,3.1,4.1,5.1,6.1,7.1),
           y = c(5, 5, 6, 6, 6, 7, 7),
           size = 4, angle = 90)

all




##### cow plot
toprow = plot_grid(plot_solar, plot_wind, rel_widths = c(1.5, 1))
toprow = plot_grid(toprow, 
                   plot_hydro, nrow = 1)

energyplot = plot_grid(toprow, all, ncol = 1)


energyplot

