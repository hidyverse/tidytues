library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2020, week = 37)

friends_info <- tuesdata$friends_info

df_friends_avg <-
  friends_info %>% 
  arrange(season, episode) %>% 
  mutate(episode_id = row_number()) %>% 
  group_by(season) %>% 
  mutate(
    avg = mean(imdb_rating),
    episode_mod = episode_id + (9 * season),
    mid = mean(episode_mod)
  ) %>% 
  ungroup() %>% 
  mutate(season = factor(season))

df_lines <-
  df_friends_avg %>% 
  group_by(season) %>% 
  summarize(
    start_x = min(episode_mod) - 5,
    end_x = max(episode_mod) + 5,
    y = unique(avg)
  ) %>% 
  pivot_longer(
    cols = c(start_x, end_x),
    names_to = "type",
    values_to = "x"
  ) %>% 
  mutate(
    x_group = if_else(type == "start_x", x + .1, x - .1),
    x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
    x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
  )


p <- df_friends_avg %>% 
  ggplot(aes(episode_mod, imdb_rating)) +
  geom_hline(data = tibble(y = 7:10),
             aes(yintercept = y),
             color = "grey82",
             size = .5) +
  geom_segment(aes(xend = episode_mod,
                   yend = avg, 
                   color = season)) +
  geom_line(data = df_lines,
            aes(x, y),
            color = "grey40") +
  geom_line(data = df_lines,
            aes(x_group, y),
            color = rep(c("#058DD9", ""))
            size = 2.5) +
  geom_point(aes(
                 color = season))
 
p
 