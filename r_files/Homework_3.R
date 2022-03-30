library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample) 
library(foreach)


dengue = read.csv("Data/dengue.csv")

dengue_split = initial_split(dengue, .8)
dengue_train = training(dengue_split)
dengue_test = testing(dengue_split)

N = nrow(dengue_train)
K = 10
fold_id = rep_len(1:K, N)  # repeats 1:K over and over again
fold_id = sample(fold_id, replace=FALSE) # permute the order randomly

maxM = 10
err_save = matrix(0, nrow=K, ncol=maxM)

for(i in 1:K) {
  train_set = which(fold_id != i)
  y_test = dengue_train$total_cases[-train_set]
  for(m in 1:maxM) {
    this_model =  rpart(total_cases~season + precipitation_amt + avg_temp_k + specific_humidity+dew_point_temp_k, data=dengue_train[train_set,],
                        control = rpart.control(cp = 0.002, minsplit=30))
    yhat_test = predict(this_model, newdata=dengue_train[-train_set,])
    err_save[i, m] = mean((y_test - yhat_test)^2)
  }
}

err_save

rpart.plot(this_model, digits=-5, type=4, extra=1)

plot(1:maxM, sqrt(colMeans(err_save)))

K_folds = 5

# Pipeline 1:
# create specific fold IDs for each row
# the default behavior of sample actually gives a permutation
test = dengue_train %>%
  mutate(fold_id = rep(1:K_folds, length=nrow(dengue_train)) %>% sample)

head(test)

# now loop over folds
rmse_cv = foreach(fold = 1:K_folds, .combine='c') %do% {
  this_model =  rpart(total_cases~season + precipitation_amt + avg_temp_k + specific_humidity+dew_point_temp_k, 
                      data=filter(test, fold_id != fold), control = rpart.control(cp = 0.002, minsplit=30))
  modelr::rmse(this_model, data=filter(test, fold_id == fold))
}
rmse_cv
