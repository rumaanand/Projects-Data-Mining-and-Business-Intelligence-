library(tidyverse)
setwd("/Users/ecm/teach/data/book2")
# read in the transaction file
ord = read.csv("orders.csv")
dim(ord)
head(ord)
# the date of the offer was 11/25/2014, so t is time since action
ord$t = as.numeric(as.Date("2014/11/25") - as.Date(ord$orddate, "%d%b%Y"))/365.25
summary(ord$t)
hist(ord$t)

#read in the customer file with one row per customer
customer = read.csv("customer.csv")
names(customer)
head(customer)
table(customer$train)

# rollup order file to create RFM variables
rfm = ord %>%
  group_by(id) %>%
  summarise(tof=max(t), r = min(t), fitem=n(), ford=n_distinct(ordnum), m=sum(price*qty)) 
head(rfm)
summary(rfm)
dim(rfm)
table(ord$category)

# this shows you how you can roll up order file counting purchases by category
cats = sort(unique(ord$category))  # list of all unique categories
cats
rfm2 = ord %>%
  group_by(id, category) %>%
  summarise(f=n()) %>%
  spread(category,f, fill=0)  %>%
  setNames(c("id", paste("f", cats, sep="")))
head(rfm2)
summary(rfm2)

# this joins the customer, RFM and RFM by category tables
all = left_join(customer, rfm, by="id") %>%
  left_join(rfm2, by="id")
summary(all)
names(all)
# Note that the dependent variable is in col 3 and the predictors 
# are in columns 4-38.

# this sets up the regresison model
train = (all$train==1)  # create logical train variable
table(train)
# This command logs all of the predictor variables. You may want to try
# other transformations, or not transform some variables at at all.
for(i in 4:38) all[[i]] = log(all[[i]]+1)
summary(all)

fit = lm(logtarg ~ ., all[train, -c(1,2)])  # -c(1,2) drops columns 1 and 2
yhat = predict(fit, all[!train,])
length(yhat) # matches number of test cases.
# the sampans.csv file is what you upload to Kaggle. File format: id,predicted logtarg
write.csv(data.frame(id=all$id[!train], logtarg=yhat), "sampans.csv", row.names=F)
