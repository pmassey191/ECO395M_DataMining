library(tidyverse)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)
library(igraph)

groceries_raw <- read.csv("Data/groceries.txt")

groceries <- groceries_raw %>% rownames_to_column(var = "customer") %>% 
  mutate(customer = as.factor(customer))

groceries <- pivot_longer(groceries,cols = !customer, 
                               names_to = "category",values_to = "grocery")
groceries = split(x=groceries_longer$grocery, f=as.factor(groceries_longer$customer))

groceries= lapply(groceries, unique)

groc_carts = as(groceries, "transactions")
summary(groc_carts)

grocrules = apriori(groc_carts, 
                     parameter=list(support=.01, confidence=.1, maxlen=2))
inspect(grocrules)
plot(grocrules)

plot(grocrules, measure = c("support", "lift"), shading = "confidence")

sub1 = subset(grocrules, subset=confidence > 0.01 & support > 0.005)

plot(sub1, method='graph')
