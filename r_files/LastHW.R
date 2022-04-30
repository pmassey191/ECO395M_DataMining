library(tidyverse)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)
library(igraph)

groceries_raw <- read.csv("Data/groceries.csv",header = FALSE)

groceries <- groceries_raw %>% separate(V1, 
                        into =c("1","2","3","4","5","6","7","8",
                                "9","10","11","12","13","14","15","16",
                                "17","18","19","20","21","22","23","24","25",
                                "26","27","28","29","30","31","32"),
                                sep = ",")

groceries <- groceries %>% rownames_to_column(var = "customer") %>% 
  mutate(customer = as.factor(customer))

groceries <- pivot_longer(groceries,cols = !customer, 
                               names_to = "category",values_to = "grocery") %>% 
  filter(grocery != "")


groceries = split(x=groceries$grocery, f=as.factor(groceries$customer))

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

sub1 = subset(grocrules, subset=confidence > 0.25 & support > 0.005)
saveAsGraph(sub1, file = "grocrules.graphml")
inspect(sub1)

write(sub1,file = "grocrules.csv")

