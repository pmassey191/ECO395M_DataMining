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
library(gamlr)
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
                     ) %>% 
  group_by(month,day_of_week,hour(date)) %>% 
  summarise(
    avg_boarding = mean(boarding)
  ) %>% 
  rename(hour = 'hour(date)')

ggplot(data = capmetro_UT_avg_boarding, aes(x = hour, y = avg_boarding,color = month))+
  geom_line()+
  facet_wrap(~day_of_week)+
  theme_minimal()


ggplot(data = capmetro_UT, aes(x = temperature, y = boarding, color = weekend))+
  geom_point()+
  facet_wrap(~hour(ymd_hms(timestamp)))
#Number 2



#number 3

german_credit = read.csv('data/german_credit.csv') 

german_credit_split = initial_split(german_credit,.8)
german_credit_training = training(german_credit_split)
german_credit_testing = testing(german_credit_split)

prob_default <- german_credit %>% 
  group_by(history) %>% 
  summarise(
    prob = mean(Default),
    n=n()
  )

ggplot(data = prob_default, aes(x = history, y = prob))+
         geom_col()

model <- glm(Default ~ duration + amount + installment + age + history + purpose + foreign, 
             data = german_credit_training, family = 'binomial')
summary(model)

sum(german_credit$Default)


#number 4

hotels_dev = read.csv('data/hotels_dev.csv')
hotels_val = read.csv('data/hotels_val.csv')

hotels_dev = hotels_dev %>% mutate_if(is.character,as.factor) %>% 
  mutate(arrival_date = ymd(arrival_date))

hotels_split <- initial_split(hotels_dev,.8)
hotels_train <- training(hotels_split)
hotels_test <- testing(hotels_split)

model_small <- lm(children ~ market_segment + adults + customer_type + 
                    is_repeated_guest, data = hotels_train)
model_large <- lm(children ~ . - arrival_date, data = hotels_train)

# model_good <- lm(children ~ (. - arrival_date + month(arrival_date))^2
#               ,data = hotels_train)


hotel_x_train = model.matrix(children ~ (.-1 - arrival_date+month(arrival_date))^2, data=hotels_train) # do -1 to drop intercept!
hotel_y_train = hotels_train$children

# Note: there's also a "sparse.model.matrix"
# here our matrix isn't sparse.
# but sparse.model.matrix is a good way of doing things if you have factors.

# fit a single lasso
hotel_lasso = gamlr(hotel_x_train, hotel_y_train, family="binomial", type = 'response')
plot(hotel_lasso) # the path plot!

hotel_x_test = model.matrix(children ~ (.-1 - arrival_date+month(arrival_date))^2, data=hotels_test)
hotel_y_test = hotels_test$children

y_hat_lasso <- predict(hotel_lasso, newdata = hotel_x_test, type = 'response') %>% as.matrix() %>% as.data.frame()


rmse <- sqrt(sum((y_hat_lasso - hotel_y_test)^2)/nrow(hotels_test))
# AIC selected coef
# note: AICc = AIC with small-sample correction.  See ?AICc
AICc(hotel_lasso)  # the AIC values for all values of lambda
plot(hotel_lasso$lambda, AICc(hotel_lasso))
plot(log(hotel_lasso$lambda), AICc(hotel_lasso))
# the coefficients at the AIC-optimizing value
# note the sparsity
hotel_beta = coef(hotel_lasso) 

# optimal lambda
log(hotel_lasso$lambda[which.min(AICc(hotel_lasso))])
sum(hotel_beta!=0) # chooses 30 (+intercept) @ log(lambda) = -4.5
hotel_beta
hotel_cvl = cv.gamlr(hotel_x, hotel_y, nfold=10, family="binomial", verb=TRUE)
plot(sccvl, bty="n")
hotelb.min = coef(hotel_cvl, select="min")
log(hotel_cvl$lambda.min)
sum(hotelb.min!=0) 
scb.1se = coef(sccvl)
log(sccvl$lambda.1se)
sum(scb.1se!=0) ## usually selects all zeros (just the intercept)
plot(sccvl, bty ="n", ylim=c(0, 1))
lines(log(sclasso$lambda),AICc(sclasso)/n, col="green", lwd=2)
legend("top", fill=c("blue","green"),
       legend=c("CV","AICc"), bty="n")

colnames(hotels_dev)



rmse(model_small,hotels_test)
rmse(model_large,hotels_test)


phat_test_model_small = predict(model_small, hotels_test, type='response')
phat_test_model_large = predict(model_large, hotels_test, type='response')
phat_test_model_good = predict(hotel_lasso, newdata = hotel_x_test, type='response')
thresh_grid = seq(1, 0, by=-0.005)
roc_curve_hotel = foreach(thresh = thresh_grid, .combine='rbind') %do% {
  yhat_test_model_small = ifelse(phat_test_model_small >= thresh, 1, 0)
  yhat_test_model_large = ifelse(phat_test_model_large >= thresh, 1, 0)
  yhat_test_model_good = ifelse(phat_test_model_good >= thresh, 1, 0)
  confusion_out_model_small = as.matrix(confusionMatrix(data = as.factor(yhat_test_model_small),reference = as.factor(as.matrix(hotels_test$children))))
  confusion_out_model_large = as.matrix(confusionMatrix(data = as.factor(yhat_test_model_large),reference = as.factor(as.matrix(hotels_test$children))))
  confusion_out_model_good =  as.matrix(confusionMatrix(data = as.factor(yhat_test_model_good),reference = as.factor(as.matrix(hotels_test$children))))
  out_model_small = data.frame(model = "Small Linear Model",
                       TPR = confusion_out_model_small[2,2]/sum(hotels_test$children==1),
                       FPR = confusion_out_model_small[2,1]/sum(hotels_test$children==0))
  out_model_large = data.frame(model = "Large Linear Model",
                               TPR = confusion_out_model_large[2,2]/sum(hotels_test$children==1),
                               FPR = confusion_out_model_large[2,1]/sum(hotels_test$children==0))
  out_model_good = data.frame(model = "Lasso Linear Model",
                              TPR = confusion_out_model_good[2,2]/sum(hotels_test$children==1),
                              FPR = confusion_out_model_good[2,1]/sum(hotels_test$children==0))
  rbind(out_model_small, out_model_large,out_model_good)
} %>% as.data.frame()


# roc_curve_hotel_test = foreach(thresh = thresh_grid, .combine='rbind') %do% {
#   yhat_test_model_small = ifelse(phat_test_model_small >= thresh, 1, 0)
#   yhat_test_model_large = ifelse(phat_test_model_large >= thresh, 1, 0)
#   yhat_test_model_good = ifelse(phat_test_model_good >= thresh, 1, 0)
#   confusion_out_model_small = table(y = hotels_test$children, yhat = yhat_test_model_small)
#   confusion_out_model_large = table(y = hotels_test$children, yhat = yhat_test_model_large)
#   confusion_out_model_good = table(y = hotels_test$children, yhat = yhat_test_model_good)
#   out_model_small = data.frame(model = "model_small",
#                                TPR = ifelse(sum(yhat_test_model_small) ==0, 0, confusion_out_model_small[2,2]/sum(hotels_test$children==1)),
#                                FPR = ifelse(sum(yhat_test_model_small) == 0, 0,confusion_out_model_small[1,2]/sum(hotels_test$children==0)))
#   out_model_large = data.frame(model = "model_large",
#                                TPR = ifelse(sum(yhat_test_model_large) ==0, 0, confusion_out_model_large[2,2]/sum(hotels_test$children==1)),
#                                FPR = ifelse(sum(yhat_test_model_large) == 0, 0,confusion_out_model_large[1,2]/sum(hotels_test$children==0)))
#   out_model_good = data.frame(model = "model_good",
#                               TPR = if(sum(yhat_test_model_good)==nrow(hotels_test)){
#                                 confusion_out_model_good[2,1]/sum(hotels_test$children==1)
#                               }else if(sum(yhat_test_model_good) == 0){
#                                 0
#                               }else{
#                                 confusion_out_model_good[2,2]/sum(hotels_test$children==1)
#                               },
#                               FPR = if(sum(yhat_test_model_good)==nrow(hotels_test)){
#                                 0
#                               }else if(sum(yhat_test_model_good) == 0){
#                                 confusion_out_model_good[1,1]/sum(hotels_test$children==0)
#                               }else{
#                                 confusion_out_model_good[1,2]/sum(hotels_test$children==0)
#                               })
#   rbind(out_model_small, out_model_large,out_model_good)
# } %>% as.data.frame()



ggplot(roc_curve_hotel) + 
  geom_line(aes(x=FPR, y=TPR, color=model)) + 
  labs(title="ROC Curves") +
  theme_minimal()

hotels_val = hotels_val %>% mutate(fold_id = rep(1:K_folds, length=nrow(hotels_val)) %>% sample) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(arrival_date = ymd(arrival_date))

hotels_val = hotels_val %>%  
  mutate_if(is.character,as.factor) %>% 
  mutate(arrival_date = ymd(arrival_date))



set.seed(1234)
  
K_folds = 20
hotels_val_x = model.matrix(children ~ (.-1 - arrival_date+month(arrival_date))^2, data=hotels_val)
hotels_val = hotels_val %>%
  mutate(fold_id = rep(1:K_folds, length=nrow(hotels_val)) %>% sample)

predict(hotel_lasso, newdata = hotels_val_x, type = 'response')

pred = foreach(fold = 1:K_folds, .combine='cbind') %do% {
  # model_good_folds = lm(children ~(.-arrival_date + month(arrival_date))^2,
  #                 data=filter(hotels_val, fold_id != fold))
  # predict(hotel_lasso, newdata=as.matrix(filter(as.data.frame(hotels_val_x), fold_id != fold)),type = 'response')
  predict(hotel_lasso, newdata=hotels_val_x,type = 'response')
} %>% 
  colSums()

actual_number <- hotels_val %>% 
  group_by(fold_id) %>% 
  summarise(
    sum = sum(children)
  )

pred
actual_number

