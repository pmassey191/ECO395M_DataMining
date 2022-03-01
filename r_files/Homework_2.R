library(mosaic)
library(tidyverse)
library(here)
library(ggthemes)
library(lubridate)
library(rsample)
library(caret)
library(modelr)
library(parallel)
library(foreach)

#number 1
capmetro_UT = read.csv('data/capmetro_UT.csv')

glimpse(capmetro_UT)
head(capmetro_UT)

capmetro_UT_avg_boarding = mutate(capmetro_UT,
                     day_of_week = factor(day_of_week,
                                          levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun")),
                     month = factor(month,
                                    levels=c("Sep", "Oct","Nov")),
                     date = ymd_hms(capmetro_UT$timestamp)
                     )) %>% 
  group_by(month,day_of_week,hour(date)) %>% 
  summarise(
    avg_boarding = mean(boarding)
  ) %>% 
  rename(hour = 'hour(date)')

ggplot(data = capmetro_UT_avg_boarding, aes(x = hour, y = avg_boarding,color = month))+
  geom_line()+
  facet_wrap(~day_of_week)

ggplot(data = capmetro_UT, aes(x = temperature, y = boarding, color = weekend))+
  geom_point()+
  facet_wrap(~hour(ymd_hms(timestamp)))

#number 2

german_credit = read.csv('data/german_credit.csv')


model <- glm(Default ~ duration + amount + installment + age + history + purpose + foreign, 
             data = german_credit, family = 'binomial')
summary(model)

#number 3

hotels_dev = read.csv('data/hotels_dev.csv')
hotels_val = read.csv('data/hotels_val.csv')

hotels_split <- initial_split(hotels_dev,.8)
hotels_train <- training(hotels_split)
hotels_test <- testing(hotels_split)

model_small <- lm(children ~ market_segment + adults + customer_type + is_repeated_guest, data = hotels_train)
model_large <- lm(children ~ . - arrival_date, data = hotels_train)



