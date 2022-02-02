library(mosaic)
library(tidyverse)

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
  


