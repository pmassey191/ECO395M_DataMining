library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample) 
library(randomForest)
library(modelr)
library(caret)
library(gbm)





greenbuildings <- read.csv("data/greenbuildings.csv") %>% 
  mutate(revenue = Rent*(leasing_rate/100),
         utility_cost = net*Gas_Costs + net*Electricity_Costs) %>%
  select(-empl_gr)

greenbuildings_split <- initial_split(greenbuildings,.8)
greenbuildings_test <- testing(greenbuildings_split)
greenbuildings_train <- training(greenbuildings_split)

lm_base = lm(revenue ~ . - CS_PropertyID - cluster-leasing_rate-Rent-LEED-Energystar, data = greenbuildings_train)
plot(lm_base)

rmse(lm_base,greenbuildings_test)

prune_1se = function(my_tree) {
  out = as.data.frame(my_tree$cptable)
  thresh = min(out$xerror + out$xstd)
  cp_opt = max(out$CP[out$xerror <= thresh])
  prune(my_tree, cp=cp_opt)
}

gb_tree = rpart(revenue ~ . - CS_PropertyID - cluster-leasing_rate-Rent-LEED-Energystar,
                  data=greenbuildings_train, control = rpart.control(cp = 0.0001,minsplit = 30))
rpart.plot(gb_tree, digits=-5, type=4, extra=1)

gb_tree_prune = prune_1se(gb_tree)
rpart.plot(gb_tree_prune, digits=-5, type=4, extra=1)

gb_forest = randomForest(revenue ~ . - CS_PropertyID - cluster-leasing_rate-Rent-LEED-Energystar,
                           data=greenbuildings_train, importance = TRUE)

boost1 = gbm(revenue ~ . - CS_PropertyID - cluster-leasing_rate-Rent-LEED-Energystar,
             data=greenbuildings_train,
             interaction.depth=7, n.trees=1000, shrinkage=.6)
gbm.perf(boost1)

plot(gb_forest)
plot(gb_tree_prune)

modelr::rmse(gb_tree_prune, greenbuildings_test)
modelr::rmse(gb_tree, greenbuildings_test)
modelr::rmse(gb_forest, greenbuildings_test) 
modelr::rmse(boost1, greenbuildings_test)

varImpPlot(gb_forest, main = "Variable Importance Plot")

partialPlot(gb_forest, greenbuildings_test, 'green_rating', las=1)

partialPlot(gb_forest, greenbuildings_test, 'age', las=1)


