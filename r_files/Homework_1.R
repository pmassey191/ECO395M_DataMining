library(mosaic)
library(tidyverse)
library(here)
library(ggthemes)
#Number 2
billboard = read.csv('../data/billboard.csv')

top10 <- billboard %>% 
  group_by(performer, song) %>% 
  summarise(
    count = n()
  ) %>% 
  arrange(desc(count)) %>% 
  head(10)
top10


unique_songs <- billboard %>% 
  filter(year != 1958) %>% 
  filter(year != 2021) %>% 
  group_by(year) %>% 
  distinct(song_id) %>% 
  summarise(
    count = n()
  )
ggplot(unique_songs)+
  geom_line(aes(x = year, y = count))

ten_week_hit <- billboard %>% 
  group_by(performer, song) %>% 
  summarise(
    count = n()
  ) %>% 
  arrange(desc(count)) %>% 
  filter(count >= 10) %>% 
  group_by(performer) %>% 
  summarise(count= n()) %>% 
  filter(count >=30)

ggplot(ten_week_hit, aes(fct_rev(fct_reorder(performer,count)),count))+
  geom_bar(stat = "identity")+
  coord_flip()

#Number 3
olympics_top20 = read.csv('../data/olympics_top20.csv')

top_height <-olympics_top20 %>% 
  group_by(sport) %>% 
  summarise(
    '95th_height' = quantile(height,probs = .95)  
  )

height_variation <- olympics_top20 %>% 
  group_by(event) %>% 
  summarise(
    sd = sd(height)
  ) %>% 
  arrange(desc(sd))

swimmers <- olympics_top20 %>% 
  filter(sport == 'Swimming') %>% 
  group_by(year, sex) %>% 
  summarise(
    avg_age = mean(age)
  )
ggplot(data = swimmers, aes(x = year, y = avg_age,color = sex))+
  geom_point()+
  geom_line()+
  theme_fivethirtyeight()
