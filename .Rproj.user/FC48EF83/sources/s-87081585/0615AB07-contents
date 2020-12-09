library(tidyverse)
library(gganimate)
library(lubridate)
library(gifski)
library(transformr)
library(scales)
library(magrittr)

scientific_10 <- function(x) {
  index_zero <- which(x == 0) 
  label <- scientific_format()(x)
  label <- str_replace(label, "e", " %*% 10^")
  label <- str_replace(label, "\\^\\+", "\\^")
  label[index_zero] <- "0"
  parse(text=label)
}

my_theme <- 
  theme_classic() + # 装飾を全部なくす
  theme(
    legend.title = element_text(colour = "black", size = 12, face = "bold"), # 凡例タイトルの文字サイズ
    legend.text = element_text(colour = "black", size = 10.5), #凡例テキストのサイズ
    panel.border = element_blank(), 
    axis.ticks = element_line(colour = "black", size = 0.5) , # 軸の色&太さ
    axis.ticks.length = unit(2, "mm"), # 軸目盛の長さ
    axis.title = element_text(colour = "black", size = 12, face = "bold"),
    axis.text = element_text(colour = "black", size = 10, face = "bold"),     #軸のフォントの色
    axis.line = element_line(colour = "black", size = 1, lineend = "square") ,   #軸の太さ(size = 1とか指定)と色の指定
    title = element_text(colour = "black", size = rel(1.5), face = "bold"), 
    strip.text.x = element_text(size = 10, face = "bold"),
    strip.background = element_blank() 
    
  )

Covid_19 <-  read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

Covid_19_top <-
  df %>% 
  mutate("date" = ymd(date)) %>% 
  filter(
    date == "2020-12-02",
    location != "World"
  ) %>% 
  slice_max(order_by = total_cases, n = 5)

Covid_19_top$location

Covid_19 %>%    
  select(location, date, total_cases) %>% 
  drop_na() %>% 
  filter(
    location == df_top$location
  )  %>% #locationのうちの上位5カ国だけにする
  ggplot(aes(date, total_cases, color = location)) +
  scale_x_date(date_breaks = "1 month")+ # 1ヶ月ごとに目盛りを入れる
  geom_line()


Covid_animation <- Covid_19 %>%
  select(location, date, total_cases) %>% 
  drop_na() %>% 
  filter(
    location == df_top$location
  )  %>% #locationのうちの上位5カ国だけにする
  mutate(
    location = as_factor(location) %>% fct_relevel(df_top$location)
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
