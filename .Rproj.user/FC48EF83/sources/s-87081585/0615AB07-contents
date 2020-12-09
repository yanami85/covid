library(tidyverse)
library(gganimate)
library(lubridate)
library(gifski)
library(transformr)
library(scales)
library(magrittr)

Covid_19 <-  read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

Covid_19_top <-
  Covid_19 %>% 
  mutate("date" = ymd(date)) %>% 
  filter(
    date == today()-2,
    location != "World"
  ) %>% 
  slice_max(order_by = total_cases, n = 5)

Covid_19_top$location

Covid_19 %>%    
  select(location, date, total_cases) %>% 
  drop_na() %>% 
  filter(
    location == Covid_19_top$location
  )  %>% #locationのうちの上位5カ国だけにする
  ggplot(aes(date, total_cases, color = location)) +
  scale_x_date(date_breaks = "1 month")+ # 1ヶ月ごとに目盛りを入れる
  geom_line()


Covid_animation <- 
  Covid_19 %>%
  select(location, date, total_cases) %>% 
  drop_na() %>% 
  filter(
    location == Covid_19_top$location
  )  %>% #locationのうちの上位5カ国だけにする
  mutate(
    location = as_factor(location) %>% fct_relevel(Covid_19_top$location)
    ) %>% #locationをfactor型に変え、多い順に並べ替え
  ggplot(aes(date, total_cases, size = total_cases, group = location)) +
  geom_line(aes(color = location), size = 1.5) +
  geom_point(aes(size = total_cases)) +
  scale_x_date(date_breaks = "1 month") +
  labs(x = "Date", y = "Total cases") + # label名の変更
  geom_text(aes(color = location, label = paste0(location, ": ", total_cases)), hjust = -0.3, size = 5) + # pointの横に感染者数を明記
  scale_y_continuous(label = comma, limits = c(0, NA)) + # y軸のlabelを3桁ごとにコンマを打つように変更
  transition_reveal(date) 
  animate(Covid_animation, duration = 20, fps = 30, width = 960, height = 480)  
  anim_save("output.gif")
