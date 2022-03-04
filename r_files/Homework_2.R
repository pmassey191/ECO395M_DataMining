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

hotels_split <- initial_split(hotels_dev,.8)
hotels_train <- training(hotels_split)
hotels_test <- testing(hotels_split)

model_small <- lm(children ~ market_segment + adults + customer_type + 
                    is_repeated_guest, data = hotels_train)
model_large <- lm(children ~ . - arrival_date, data = hotels_train)

model_good <- lm(children ~ . - arrival_date + month(arrival_date)+
                   month(arrival_date)*hotel, data = hotels_train)

hotel_x = model.matrix(children ~ (.-1 - arrival_date+month(arrival_date))^2, data=hotels_dev) # do -1 to drop intercept!
hotel_y = hotels_dev$children

# Note: there's also a "sparse.model.matrix"
# here our matrix isn't sparse.
# but sparse.model.matrix is a good way of doing things if you have factors.

# fit a single lasso
hotel_lasso = gamlr(hotel_x, hotel_y, family="binomial")
plot(hotel_lasso) # the path plot!



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
# Now without the AIC approximation:
# cross validated lasso (`verb` just prints progress)
# this takes a little longer, but still so fast compared to stepwise
hotel_cvl = cv.gamlr(hotel_x, hotel_y, nfold=10, family="binomial", verb=TRUE)

# plot the out-of-sample deviance as a function of log lambda
# Q: what are the bars associated with each dot? 
plot(sccvl, bty="n")

## CV min deviance selection
hotelb.min = coef(hotel_cvl, select="min")
log(hotel_cvl$lambda.min)
sum(hotelb.min!=0) # note: this is random!  because of the CV randomness

## CV 1se selection (the default)
scb.1se = coef(sccvl)
log(sccvl$lambda.1se)
sum(scb.1se!=0) ## usually selects all zeros (just the intercept)

## comparing AICc and the CV error
# note that AIC is a pretty good estimate of out-of-sample deviance
# for values of lambda near the optimum
# outside that range: much worse  
plot(sccvl, bty ="n", ylim=c(0, 1))
lines(log(sclasso$lambda),AICc(sclasso)/n, col="green", lwd=2)
legend("top", fill=c("blue","green"),
       legend=c("CV","AICc"), bty="n")

colnames(hotels_dev)



rmse(model_small,hotels_test)
rmse(model_large,hotels_test)
rmse(model_good,hotels_test)

AICc(model_small)
AICc(model_large)
