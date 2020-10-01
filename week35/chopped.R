library(tidyverse)
library(maps)
library(datasets)
library(tidycensus)
library(mapproj)
library(showtext)
library(ggrepel)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')


## Add the font with the corresponding font faces
font_add("Capoon",
         regular = "CapoonMedi_PERSONAL.ttf")
## Automatically use showtext to render plots
showtext_auto() ## You need to do this at the beginning of a session.

skimr::skim(chopped)

info = chopped %>% 
  select(episode_name,
         contains("info")) %>%
  gather( -episode_name, key = "value",
          value = "info") %>% 
  mutate(info = gsub("  Episode.*$", "", info),
         info = gsub("  advances.*$", "", info),
         info = gsub("Episode.*$", "", info),
         info = gsub("  charity.*$", "",info),
         info = gsub("charity.*$", "", info),
         info = gsub(" †.*$", "", info),
         info = gsub("  Chopped.*$", "", info, ignore.case = T)) %>% 
  select(-value) %>% 
  dplyr::filter(!grepl('Charity', info),
                !grepl('Winner', info, ignore.case = T),
                !grepl("Worst", info),
                info != "",
                !is.na(info))

info$home = word(info$info, -1)

info$home[info$home == "Pennsylvania"] = "PA"
info$home[info$home == "Oregon"] = "OR"
info$home[info$home == "Angeles"] = "CA"
info$home[info$home == "Tennessee"] = "TN"
info$home[info$home == "York"] = "NY"

info2 = info %>% 
  dplyr::filter(nchar(home) <= 3) %>% 
  mutate(home = str_remove(home, '\\.')) %>% 
  group_by(episode_name) %>% 
  select(-info) %>% 
  pivot_wider(names_from = "episode_name",
              values_from = "home") %>% 
  pivot_longer(names(info2),names_to = "episode_name",
               values_to = "state") 



states= data.frame(state.abb,
                   state.name)
colnames(states)[colnames(states) == "state.abb"] = "home"
states$state.name = toupper(states$state.name)
colnames(states)[colnames(states) == "state.name"] = "region"

MainStates <- map_data("state")
MainStates$region = toupper(MainStates$region)
MainStates = left_join(x = MainStates,
                       y = states,
                       by= "region")


info3 = data.frame(matrix(ncol = 50,nrow = 494))
colnames(info3) = states$home
info4 = bind_cols(info2, info3)

x = 245
info5<-as.data.frame(info4)
for (x in 3:nrow(info5)) {
info5[x,colnames(info5) %in% unlist(info5$state[x])]<-1
}


info5 = info5 %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  select(-state)

census_api_key("ee40e8d753390f16624a1e1f3b26ae816fcb949f")
texas_pop <- get_acs(geography = "state", 
                     variables = "B01003_001", 
                     geometry = TRUE) 
head(texas_pop)
pop = texas_pop %>% 
  mutate(region = toupper(NAME))

cities = us.cities %>% 
  filter(capital == 2,
         country.etc != "AK",
         country.etc != "HI") %>% 
  mutate(home = country.etc) 

cities2 = MainStates %>% 
  select(region, 
         home) %>% 
  right_join(cities, by = "home")

cities3 = as.data.frame(pop) %>% 
  select(region, 
         estimate) %>% 
  full_join(cities2, by = "region") %>% 
  distinct()

ratings = chopped %>% 
  select(episode = episode_name, episode_rating)

finaldata = left_join(info5, ratings, by = "episode")

results = finaldata %>% 
  pivot_longer(cols = AL:WY,
               names_to = "home",
               values_to = "in_episode") %>% 
  group_by(home,
           in_episode) %>% 
  summarise(min = min(episode_rating, na.rm = T)) %>% 
  group_by(home) %>% 
  summarize(index = min[in_episode == 1] - min[in_episode ==0]) %>% 
  right_join(MainStates, by = "home") %>% 
  filter(home != "HI",
         home != "AK") %>% 
  left_join(pop, by = "region") %>% 
  select(-moe,
         -geometry) %>% 
  mutate(index_norm = index/estimate)


## Plot 
p1 = ggplot()  + 
  geom_polygon(data = results, 
               aes(x=long, 
                   y=lat,
                   group = group,
                   fill = index),
               color = "#f1f1f3",
               size = .08) + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    text = element_text(family = "Capoon"),
    plot.title = element_text(margin=margin(0,0,0,0)),
  ) +
  scale_fill_gradient(high = "#83060e",
                      low = "#ffaf32",
                      na.value = "#5b5a5f",
                      guide = F) + 
  labs( x = "",
        y = "",
        fill = "") +
  lims(x = c(-140, -65)) +
  annotate(geom = "text",
           x= -100, y =56,
           label = "State Pride in Chopped Episodes",
           family = "Capoon",
           size = 8) + 
  annotate(geom = "text",
           x = -100, y = 52,
           label = "Ratings were compred between episodes 
           with or without a cook from that 
           state to form a 'Pride Index'",
           family = "Capoon",
           size = 5) +
  annotate(geom = "text", 
           x = -90,
           y = 25,
           label = "Darker colors 
           mean higher 
           'Pride'",
           family = "Capoon",
           size = 3)

p1

arrows <- 
  tibble(
    x1 = c(-90),
    x2 = c(-85),
    y1 = c(26.8), 
    y2 = c(29)
  )

p1 = p1 +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 1,
    curvature = -.2
  )

cordat = results %>% 
  select(estimate, index) %>% 
  filter(!is.na(estimate),
         !is.na(index),
         home != "MN") %>% 
  distinct()

cor(cordat$index, cordat$estimate, method = "spearman")

p2 = ggplot(data = cordat,
       aes(x = log(estimate),
           y = index)) + 
  geom_smooth(method = "lm", se = F, color = "#ffaf32")+ 
  geom_text_repel(aes(label = home),
             color = "#83060e",
            size = 4,
            family = "Capoon") +
  theme(
    plot.background = element_blank(),
    panel.grid =  element_blank(),
    panel.background = element_blank(),
    axis.title = element_text(family = "Capoon")
  ) +
  labs(x = "Log Population", 
       y = "Pride Index")

p2 

summary(lm(data = cordat, index~estimate))

gg_inset_map1 = ggdraw() +
  draw_plot(p1) +
  draw_plot(p2, x = 0, y = 0.0, width = 0.5, height = 0.5)

gg_inset_map1

