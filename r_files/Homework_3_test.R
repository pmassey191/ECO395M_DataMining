library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample) 
library(randomForest)
library(lubridate)
library(modelr)
library(caret)
library(gamlr)
library(glmnet)

greenbuildings <- read.csv("data/greenbuildings.csv") %>% 
  mutate(revenue = Rent*(leasing_rate/100),
         green_certified = ifelse(LEED == 1 | Energystar == 1,1,0),
         utility_cost = net*Gas_Costs + net*Electricity_Costs) %>%
  select(-empl_gr)

greenbuildings_split <- initial_split(greenbuildings,.8)
greenbuildings_test <- testing(greenbuildings_split)
greenbuildings_train <- training(greenbuildings_split)

lm_base = lm(revenue ~ size + age + renovated + class_a + class_b + green_rating + utility_cost + total_dd_07
             + total_dd_07*utility_cost, data = greenbuildings_train)

rmse(lm_base,greenbuildings_test)

# 
# gbx = model.matrix(revenue ~ (.-1)^2, data=greenbuildings_train) # do -1 to drop intercept!
# gby = greenbuildings_train$revenue
# nrow(gbx)
# gblasso = gamlr(gbx, gby, family = "poisson")
# plot(gblasso)
# coef(gblasso)
# 
# test = glmnet(gbx,gby)
# plot(test)

gb_tree = rpart(revenue ~ size + age + renovated + class_a + class_b + green_rating + utility_cost,
                  data=greenbuildings_train, control = rpart.control(cp = 0.001,minsplit = 100))
rpart.plot(gb_tree, digits=-5, type=4, extra=1)

gb_forest = randomForest(revenue ~ size + age + renovated + class_a + class_b + green_rating + utility_cost + total_dd_07,
                           data=greenbuildings_train, importance = TRUE)
plot(gb_forest)

modelr::rmse(gb_tree, greenbuildings_test)
modelr::rmse(gb_forest, greenbuildings_test) 
