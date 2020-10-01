tuesdata <- tidytuesdayR::tt_load(2020, week = 38)

kids <- tuesdata$kids

library(readxl)
library(tidyverse)
library(rvest)

datadict = read_excel("week38/datadict.xlsx") %>% 
  select(Variable,
         variable = `Variable name`)

data = inner_join(kids, datadict)

govs = read_xlsx("week38/govs.xlsx")

govs2 = govs %>% 
  separate(`Time in Office`, sep = "-", into = c("From", "To")) %>% 
  rowwise() %>%
  do(data.frame(name = .$`Party`, year = seq(.$From, .$To, by = 1))) 


data2 = left_join(data, govs2, by = "year")

data2 %>% 
  filter(state == "Arizona",
         variable == "PK12ed") %>% 
  ggplot(aes(x = year,
             y = inf_adj_perchild)) + 
  geom_rect(aes(xmin=year,xmax=year+1,
                ymin=min(inf_adj_perchild),
                ymax=max(inf_adj_perchild), 
                fill=name)) +
  geom_line()


data2 %>% 
  filter(state == "Arizona") %>% 
  group_by(variable) %>% 
ggplot(aes(group = variable)) +
  facet_wrap(~variable) + 
  geom_rect(data = data2, aes(xmin=year,xmax=year+1,ymin=min(inf_adj_perchild),ymax=max(inf_adj_perchild), 
                fill=name)) +
  geom_line(aes(x=year,y=inf_adj_perchild)) 


library(ggbump)
library(ggimage)
library(scales)
library(showtext)
## Add the font with the corresponding font faces

font_add("Filetto", regular = "filetto_regular.ttf")
## Automatically use showtext to render plots
showtext_auto()

flags = read_csv("week38/stateflags.csv")

lowest = data2 %>% 
  filter(variable == "PK12ed") %>% 
  filter(year %in% c("2000", "2005", "2010", "2015")) %>% 
  group_by(year) %>% 
  arrange(desc(inf_adj_perchild)) %>% 
  slice_tail(n = 10) %>% 
  mutate(rank = rank(inf_adj_perchild, ties.method = "random")) %>% 
  left_join(flags, by = "state") 




lowest %>% 
  ggplot(aes(x = year, 
             y = rank)) + 
  geom_bump(aes(color = state), geom = "line", size = 3) +
  geom_text(data = lowest %>% 
              group_by(state) %>% 
              slice(1), 
            aes(label = state),
            nudge_y = .5,
            nudge_x = .43,
            size = 9,
            alpha = .5,
            family= "Filetto") + 
  geom_image(aes(image = state_flag_url), size = .05) +
 scale_color_manual(values = c("#fdd10a", "#032763","#ca8f10",
                                "#000053", "#af0025", "#03329b",
                                "#af0025", "#0b5cc4", "#0c6097",
                                 "#be0004", "#031f57")) +
  scale_y_reverse(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  xlim(c(1998.5,2015)) +
  theme_bw() + 
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 15, family = "Filetto"),
        plot.title = element_text(size = 25, family = "Filetto",
                                  lineheight = 0.5)) +
  labs(x = "", 
       y = "Rank",
       title = "Arizona is historically among the lowest\n
       spending states on K-12 education")
















             