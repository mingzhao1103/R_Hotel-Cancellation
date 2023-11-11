
########## Hotel Booking Cancellations ##########

library(tidyverse)
library(dplyr)
library(ggplot2)

setwd("/Users/mingzhao/Desktop/250")

data <- read.csv("hotel_bookings.csv")


dim(data) # 119390, 32
head(data)
str(data)

sum(is.na(data))
sapply(data, function(x) sum(is.na(x)))

summary(data)

# remove 2 variables due to a large number of missing values
df <- select(data, -c(agent, company)) 

# remove 3 variables due to date
df <- select(df, -c(reservation_status_date, arrival_date_day_of_month, arrival_date_week_number))

# remove 2 variables due to overlapping with other variables (is_canceled and distribution_channel)
df <- select(df, -c(reservation_status, distribution_channel))

# indicate missing values
df$na <- 0
##df$na[df$country=="NULL"] <- 1
df$na[is.na(df$children)] <- 1

# replace missing values with mode
##which.max(table(df$country))
##df$country[df$country=="NULL"] <- "PRT"

table(df$children)
df$children[is.na(df$children)] <- 0

# recode variables
df$international <- 1
df$international[df$country=="PRT"] <- 0

df$resort_hotel <- 1
df$resort_hotel[df$hotel=="City Hotel"] <- 0

df$market_segment[df$market_segment=="Undefined"|df$market_segment=="Aviation"] <- "Others"

# generate new variables
df$total_stay = df$stays_in_week_nights + df$stays_in_weekend_nights
df$guests = df$adults + df$children + df$babies

df$different_room_assigned = 0
df$different_room_assigned[df$reserved_room_type!=df$assigned_room_type] <- 1

# remove 9 variables due to generated new variables
df <- select(df, -c(country, hotel,
                    stays_in_week_nights, stays_in_weekend_nights, adults, children, babies,
                    reserved_room_type, assigned_room_type))

# change data types
int = list("is_canceled", "different_room_assigned", "na", "is_repeated_guest", "international", "resort_hotel")
for (var in int){
  df[[var]] <- as.integer(df[[var]])
}

num = list("lead_time", "previous_cancellations", "previous_bookings_not_canceled", "booking_changes", 
           "days_in_waiting_list", "adr", "required_car_parking_spaces", "total_of_special_requests", 
           "total_stay", "guests")
for (var in num){
  df[[var]] <- as.numeric(df[[var]])
}

# rename outcome
df <- df %>% 
  rename(outcome = is_canceled)

# rename treatment
df <- df %>% 
  rename(treatment = different_room_assigned)

###########################################################################

# Descriptive Statistics

table(df$international, df$outcome)
prop.table(table(df$international, df$outcome), margin=2) # col-sum
prop.table(table(df$international, df$treatment), margin=2) #col-sum

prop.table(table(df$international, df$outcome), margin=1) # row-sum
prop.table(table(df$international, df$treatment), margin=1) #row-sum

table(df$arrival_date_year, df$outcome)
prop.table(table(df$arrival_date_year, df$outcome), margin=2) # col-sum
prop.table(table(df$arrival_date_year, df$treatment), margin=2) #col-sum

prop.table(table(df$arrival_date_year, df$outcome), margin=1) # row-sum
prop.table(table(df$arrival_date_year, df$treatment), margin=1) #row-sum

df_2016 <- subset(df, arrival_date_year==2016)
table(df_2016$international, df_2016$outcome)
table(df_2016$international, df_2016$treatment)

df <- subset(df_2016, international==0)
df <- subset(df, select=-c(international, arrival_date_year))

table(df$outcome)
prop.table(table(df$outcome))
table(df$treatment)
prop.table(table(df$treatment))
table(df$treatment, df$outcome)
prop.table(table(df$treatment, df$outcome), margin=2) #col-sum
prop.table(table(df$treatment, df$outcome), margin=1) # row-sum

# summarize data
names_list <- names(df)
chr_list <- names_list[sapply(names_list, function(var) is.character(df[[var]]))]
int_list <- names_list[sapply(names_list, function(var) is.integer(df[[var]]))]
num_list <- names_list[sapply(names_list, function(var) is.numeric(df[[var]]))]

for(var in chr_list){
  print("______________")
  print(var)
  cnt <- table(df[[var]])
  len <- length(unique(df[[var]]))
  print(cnt)
  print(len)
}

for(var in int_list){
  print("______________")
  print(var)
  cnt <- table(df[[var]])
  print(cnt)
}

for(var in num_list){
  print("______________")
  print(var)
  cnt <- summary(df[[var]])
  print(cnt)
}

# data types
## arrival_date_month: category 12
## meal: category 5
## market_segment: category 7
## deposit_type: category 3
## customer_type: category 4

## is_canceled: binary
## different_room_assigned: binary
## is_repeated_guest: binary
## resort_hotel: binary

## lead_time: numeric
## previous_cancellations: numeric
## previous_bookings_not_canceled: numeric
## booking_changes: numeric
## days_in_waiting_list: numeric
## adr: numeric
## required_car_parking_spaces: numeric
## total_of_special_requests: numeric
## total_stay: numeric
## guests: numeric

df <- subset(df, select=-c(na))
dim(df) # 22321, 19
sum(is.na(df))

###########################################################################

# Preliminary Inference about Treatment Effect

### In a randomized experiment, the randomization tends to balance the covariates between the treated and control groups.

### In an observational study, the raw comparison between the treatment and control group will be biased 
### if the treated and control groups differ prior to treatment in ways that matter for the outcomes under study.


# Fisher's exact test
attach(df)

hotel_data = matrix(c(sum(outcome[treatment==1]),sum(outcome[treatment==0]),
                      sum((1-outcome)[treatment==1]),sum((1-outcome)[treatment==0])), ncol=2,
                    dimnames=list(treatment=c("Different Room","Control"),
                                  cancelled=c("Yes","No")))
hotel_data

## H0: (no effect)
## H1: (different room assigned increases booking cancelled)
fisher.test(hotel_data, alternative="greater")

## H0: (no effect)
## H1: (effect)
fisher.test(hotel_data)

# Logistic regression
## under no unmeasured confounders

logit1 = glm(outcome ~ treatment, data=df, family=binomial)
summary(logit1)

logit2 = glm(outcome ~ ., data=df, family=binomial)
summary(logit2)

#################

# Raw comparison of treated and control Groups

covariates = list("resort_hotel", "is_repeated_guest",  "previous_cancellations", "previous_bookings_not_canceled",  
                  "lead_time",  "total_stay",  "guests", "booking_changes",  "days_in_waiting_list",  "adr", 
                  "required_car_parking_spaces", "total_of_special_requests", "arrival_date_month_February",  
                  "arrival_date_month_March", "arrival_date_month_April",  "arrival_date_month_May", 
                  "arrival_date_month_June",  "arrival_date_month_July", "arrival_date_month_August",  
                  "arrival_date_month_September", "arrival_date_month_October",  "arrival_date_month_November", 
                  "arrival_date_month_December",  "meal_Undefined",  "meal_SC",  "meal_HB",  "meal_FB", 
                  "market_segment_Corporate",  "market_segment_Direct",  "market_segment_Groups", 
                  "market_segment_Offline.TA.TO",  "market_segment_Online.TA", "market_segment_Others",  
                  "deposit_type_Non.Refund",  "deposit_type_Refundable", "customer_type_Group",  
                  "customer_type_Transient",  "customer_type_Transient.Party")

for(var in covariates){
  print("______________")
  print(var)
  means <- round(aggregate(df[[var]], list(df$treatment), FUN=mean), 3)
  print(means)
}


for(var in covariates){
  print("______________")
  print(var)
  p <- t.test(df[[var]][df$treatment==1], df[[var]][df$treatment==0])$p.val
  print(round(p, 4)<0.01)
}

for(var in covariates){
  print("______________")
  print(var)
  p <- t.test(df[[var]][df$treatment==1], df[[var]][df$treatment==0])$p.val
  print(round(p, 3))
}

#################

# Multivariate matching

## Multivariate matching methods attempt to produce matched pairs or sets that balance observed covariates, 
## so that, in aggregate, the distributions of observed covariates are similar in treated and matched control groups


## Distance for matching
### robust Mahalanobis distance with a propensity score caliper

data <- df
data <- select(data, -c(outcome))
logit3 = glm(treatment ~ ., data=data, family=binomial)
summary(logit3)

library(fastDummies)
df <- dummy_cols(df)

names_list <- names(df)
names_list[sapply(names_list, function(var) is.character(df[[var]]))]

df <- select(df, -c(arrival_date_month, meal, market_segment, deposit_type, customer_type))

dim(df) # 22321, 45
str(df)

df <- df %>% 
  rename( market_segment_Offline.TA.TO = `market_segment_Offline TA/TO`, 
          market_segment_Online.TA = `market_segment_Online TA`,
          deposit_type_Non.Refund = `deposit_type_Non Refund`,
          customer_type_Transient.Party = `customer_type_Transient-Party`)

datatemp <- df

propscore.model = glm(treatment ~ resort_hotel + is_repeated_guest + previous_cancellations
                      + previous_bookings_not_canceled + lead_time + total_stay + guests
                      + booking_changes + days_in_waiting_list + adr
                      + required_car_parking_spaces + total_of_special_requests
                      + arrival_date_month_February + arrival_date_month_March
                      + arrival_date_month_April + arrival_date_month_May
                      + arrival_date_month_June + arrival_date_month_July
                      + arrival_date_month_August + arrival_date_month_September
                      + arrival_date_month_October + arrival_date_month_November
                      + arrival_date_month_December + meal_Undefined + meal_SC + meal_HB + meal_FB
                      + market_segment_Corporate + market_segment_Direct + market_segment_Groups
                      + market_segment_Offline.TA.TO + market_segment_Online.TA
                      + market_segment_Others + deposit_type_Non.Refund + deposit_type_Refundable 
                      + customer_type_Group + customer_type_Transient + customer_type_Transient.Party, 
                      family=binomial, x=TRUE, y=TRUE, data=datatemp)

datatemp$treated <- propscore.model$y
datatemp$treatment <- datatemp$treated

library(caret)
dmy = dummyVars(propscore.model$formula, data=datatemp)
Xmat = data.frame(predict(dmy, newdata=datatemp))

Xmatmahal = Xmat
treated = datatemp$treated
datatemp$logit.ps = predict(propscore.model)

### use Hansen (2009)'s rule for removing subjects who lack overlap
logit.propscore = predict(propscore.model)
pooled.sd.logit.propscore = sqrt(var(logit.propscore[datatemp$treatment==1])/2+
                                 var(logit.propscore[datatemp$treatment==0])/2)
min.treated.logit.propscore = min(logit.propscore[datatemp$treatment==1])
max.control.logit.propscore = max(logit.propscore[datatemp$treatment==0])

### how many treated and control subjects lack overlap by Hansen's criterion
no.treated.lack.overlap = sum(logit.propscore[datatemp$treatment==1] >
                          (max.control.logit.propscore + .5*pooled.sd.logit.propscore))
no.control.lack.overlap = sum(logit.propscore[datatemp$treatment==0] <
                          (min.treated.logit.propscore - .5*pooled.sd.logit.propscore))

### if there are subjects who lack overlap, remove them
datatemp.original = datatemp
datatemp.full = datatemp
Xmat.original = Xmat
Xmat.full = Xmat

if(no.treated.lack.overlap+no.control.lack.overlap > 0){
  which.remove = which(
    (logit.propscore > (max.control.logit.propscore + .5*pooled.sd.logit.propscore))|
    (logit.propscore < (min.treated.logit.propscore - .5*pooled.sd.logit.propscore)))
  datatemp = datatemp[-which.remove,]
  datatemp.full = rbind(datatemp, datatemp.original[which.remove,])
  Xmat = Xmat[-which.remove,]
  Xmat.full = rbind(Xmat,Xmat.original[which.remove,])
  Xmatmahal = Xmatmahal[-which.remove,]
}

### for the purposes of balance checking later, in datatemp.full, append the removed rows of datatemp to the end of datatemp

### make the rownames in datatemp be 1:number of rows
rownames(datatemp) = seq(1, nrow(datatemp), 1)

#### devtools::install_github("ruoqiyu/DiPs")
library(DiPs)

### rank based Mahalanobis distance
distmat = maha_dense(datatemp$treated, Xmatmahal, matrix=TRUE)

### add caliper
distmat = addcaliper(distmat, datatemp$treated, datatemp$logit.ps, rg=0.5,
                     stdev=TRUE, penalty=1000, constant=FALSE)

### label the rows and columns of the distance matrix by the rownames in datatemp
rownames(distmat) = rownames(datatemp)[as.numeric(rownames(distmat))]
colnames(distmat) = rownames(datatemp)[as.numeric(colnames(distmat))]

#################

# Optimal matching (1 to 1)

result = match(datatemp$treatment, distmat, datatemp, ncontrol=1)
matcheddata = result$data
treated.y = matcheddata$outcome[matcheddata$treatment==1]
control.y = matcheddata$outcome[matcheddata$treatment==0]

# Examining Balance
## Standardized difference and Love Plot

### Extract Xmat for the matched data
Xmat.matched = Xmat[as.numeric(rownames(matcheddata)),]

### Calculate standardized differences
### Which non-categorical variables can be missing

missing.mat = matrix(rep(0, ncol(Xmat.full)*nrow(Xmat.full)),
                     ncol = ncol(Xmat.full))
missing.mat.matched = matrix(rep(0, ncol(Xmat.matched) * nrow(Xmat.matched)),
                             ncol=ncol(Xmat.matched))

### Put in NAs for all X variables which are missing and for which mean value has been imputed
Xmat.without.missing = Xmat.full
Xmat.without.missing.matched = Xmat.matched
for(i in 1:ncol(Xmat.full)){
  Xmat.without.missing[missing.mat[,i]==1, i] = NA
  Xmat.without.missing.matched[missing.mat.matched[,i]==1, i] = NA
}

balance_tb = check(Xmat.without.missing, Xmat.without.missing.matched,
                   datatemp.full$treated, matcheddata$treated)

round(balance_tb, 3)

### Plot
abs.stand.diff.before=abs(balance_tb[,5])
abs.stand.diff.after=abs(balance_tb[,4])
covariates = rownames(balance_tb)
plot.dataframe = data.frame(abs.stand.diff=c(abs.stand.diff.before, abs.stand.diff.after),
                            covariates=rep(covariates,2),
                            type=c(rep("Before", length(covariates)),
                            rep("After", length(covariates))))
plot.dataframe$covariates <- fct_rev(factor(plot.dataframe$covariates, levels = 
                                         c("resort_hotel", "is_repeated_guest",  "previous_cancellations", "previous_bookings_not_canceled",  
                                           "lead_time",  "total_stay",  "guests", "booking_changes",  "days_in_waiting_list",  "adr", 
                                           "required_car_parking_spaces", "total_of_special_requests", "arrival_date_month_February",  
                                           "arrival_date_month_March", "arrival_date_month_April",  "arrival_date_month_May", 
                                           "arrival_date_month_June",  "arrival_date_month_July", "arrival_date_month_August",  
                                           "arrival_date_month_September", "arrival_date_month_October",  "arrival_date_month_November", 
                                           "arrival_date_month_December",  "meal_Undefined",  "meal_SC",  "meal_HB",  "meal_FB", 
                                           "market_segment_Corporate",  "market_segment_Direct",  "market_segment_Groups", 
                                           "market_segment_Offline.TA.TO",  "market_segment_Online.TA", "market_segment_Others",  
                                           "deposit_type_Non.Refund",  "deposit_type_Refundable", "customer_type_Group",  
                                           "customer_type_Transient",  "customer_type_Transient.Party")))
ggplot(plot.dataframe, aes(x=abs.stand.diff, y=covariates)) +
       geom_point(size=5, aes(shape=factor(type))) +
       scale_shape_manual(values=c(4,1)) +
       geom_vline(xintercept=c(.1,.2), lty=2)


# Improving Match

## add asymmetric caliper for booking_changes
distmat_1 = addcaliper(distmat, datatemp$treated, datatemp$booking_changes,
                       rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                       constant=FALSE)

result_1 = match(datatemp$treatment, distmat_1, datatemp, ncontrol=1)
matcheddata_1 = result_1$data

Xmat.matched = Xmat[as.numeric(rownames(matcheddata_1)),]
missing.mat.matched = matrix(rep(0, ncol(Xmat.matched)*nrow(Xmat.matched)),
                             ncol=ncol(Xmat.matched))
Xmat.without.missing.matched = Xmat.matched
for(i in 1:ncol(Xmat.full)){
  Xmat.without.missing.matched[missing.mat.matched[,i]==1, i] = NA
}

balance_tb_1 = check(Xmat.without.missing, Xmat.without.missing.matched,
                   datatemp.full$treated, matcheddata_1$treated)

round(balance_tb_1[,4:5], 3)

## Plot
abs.stand.diff.before=abs(balance_tb_1[,5])
abs.stand.diff.after=abs(balance_tb_1[,4])
covariates = rownames(balance_tb_1)
plot.dataframe = data.frame(abs.stand.diff=c(abs.stand.diff.before, abs.stand.diff.after),
                            covariates=rep(covariates,2),
                            type=c(rep("Before", length(covariates)),
                                   rep("After", length(covariates))))
plot.dataframe$covariates <- fct_rev(factor(plot.dataframe$covariates, levels = 
                                              c("resort_hotel", "is_repeated_guest",  "previous_cancellations", "previous_bookings_not_canceled",  
                                                "lead_time",  "total_stay",  "guests", "booking_changes",  "days_in_waiting_list",  "adr", 
                                                "required_car_parking_spaces", "total_of_special_requests", "arrival_date_month_February",  
                                                "arrival_date_month_March", "arrival_date_month_April",  "arrival_date_month_May", 
                                                "arrival_date_month_June",  "arrival_date_month_July", "arrival_date_month_August",  
                                                "arrival_date_month_September", "arrival_date_month_October",  "arrival_date_month_November", 
                                                "arrival_date_month_December",  "meal_Undefined",  "meal_SC",  "meal_HB",  "meal_FB", 
                                                "market_segment_Corporate",  "market_segment_Direct",  "market_segment_Groups", 
                                                "market_segment_Offline.TA.TO",  "market_segment_Online.TA", "market_segment_Others",  
                                                "deposit_type_Non.Refund",  "deposit_type_Refundable", "customer_type_Group",  
                                                "customer_type_Transient",  "customer_type_Transient.Party")))
ggplot(plot.dataframe, aes(x=abs.stand.diff, y=covariates)) +
  geom_point(size=5, aes(shape=factor(type))) +
  scale_shape_manual(values=c(4,1)) +
  geom_vline(xintercept=c(.1,.2), lty=2)


treated.y = matcheddata_1$outcome[matcheddata_1$treatment==1]
control.y = matcheddata_1$outcome[matcheddata_1$treatment==0]

#################

# Optimal matching (1 to 2)

result2 = match(datatemp$treatment, distmat, datatemp, ncontrol=2)
matcheddata2 = result2$data
treated.y2 = matcheddata2$outcome[matcheddata2$treatment==1]
control.y2 = matcheddata2$outcome[matcheddata2$treatment==0]

# Examining Balance

Xmat.matched = Xmat[as.numeric(rownames(matcheddata2)),]

missing.mat = matrix(rep(0, ncol(Xmat.full)*nrow(Xmat.full)),
                     ncol=ncol(Xmat.full))
missing.mat.matched = matrix(rep(0, ncol(Xmat.matched)*nrow(Xmat.matched)),
                             ncol=ncol(Xmat.matched))

Xmat.without.missing = Xmat.full
Xmat.without.missing.matched = Xmat.matched
for(i in 1:ncol(Xmat.full)){
  Xmat.without.missing[missing.mat[,i]==1, i] = NA
  Xmat.without.missing.matched[missing.mat.matched[,i]==1, i] = NA
}

balance_tb2 = check(Xmat.without.missing, Xmat.without.missing.matched,
                   datatemp.full$treated, matcheddata2$treated)

round(balance_tb2, 3)[,4:5]

### Plot
abs.stand.diff.before=abs(balance_tb2[,5])
abs.stand.diff.after=abs(balance_tb2[,4])
covariates = rownames(balance_tb2)
plot.dataframe = data.frame(abs.stand.diff=c(abs.stand.diff.before, abs.stand.diff.after),
                            covariates=rep(covariates,2),
                            type=c(rep("Before", length(covariates)),
                                   rep("After", length(covariates))))
plot.dataframe$covariates <- fct_rev(factor(plot.dataframe$covariates, levels = 
                                              c("resort_hotel", "is_repeated_guest",  "previous_cancellations", "previous_bookings_not_canceled",  
                                                "lead_time",  "total_stay",  "guests", "booking_changes",  "days_in_waiting_list",  "adr", 
                                                "required_car_parking_spaces", "total_of_special_requests", "arrival_date_month_February",  
                                                "arrival_date_month_March", "arrival_date_month_April",  "arrival_date_month_May", 
                                                "arrival_date_month_June",  "arrival_date_month_July", "arrival_date_month_August",  
                                                "arrival_date_month_September", "arrival_date_month_October",  "arrival_date_month_November", 
                                                "arrival_date_month_December",  "meal_Undefined",  "meal_SC",  "meal_HB",  "meal_FB", 
                                                "market_segment_Corporate",  "market_segment_Direct",  "market_segment_Groups", 
                                                "market_segment_Offline.TA.TO",  "market_segment_Online.TA", "market_segment_Others",  
                                                "deposit_type_Non.Refund",  "deposit_type_Refundable", "customer_type_Group",  
                                                "customer_type_Transient",  "customer_type_Transient.Party")))
ggplot(plot.dataframe, aes(x=abs.stand.diff, y=covariates)) +
  geom_point(size=5, aes(shape=factor(type))) +
  scale_shape_manual(values=c(4,1)) +
  geom_vline(xintercept=c(.1,.2), lty=2)


# Improving Match

distmat2_1 = addcaliper(distmat, datatemp$treated, datatemp$adr,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

distmat2_2 = addcaliper(distmat2_1, datatemp$treated, datatemp$resort_hotel,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

distmat2_3 = addcaliper(distmat2_2, datatemp$treated, datatemp$is_repeated_guest,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

distmat2_4 = addcaliper(distmat2_3, datatemp$treated, datatemp$total_stay,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

distmat2_5 = addcaliper(distmat2_4, datatemp$treated, datatemp$market_segment_Corporate,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

result2_5 = match(datatemp$treatment, distmat2_5, datatemp, ncontrol=2)
matcheddata2_5 = result2_5$data


Xmat.matched = Xmat[as.numeric(rownames(matcheddata2_5)),]
missing.mat.matched = matrix(rep(0, ncol(Xmat.matched)*nrow(Xmat.matched)),
                             ncol=ncol(Xmat.matched))
Xmat.without.missing.matched = Xmat.matched
for(i in 1:ncol(Xmat.full)){
  Xmat.without.missing.matched[missing.mat.matched[,i]==1, i] = NA
}

balance_tb2_5 = check(Xmat.without.missing, Xmat.without.missing.matched,
                      datatemp.full$treated, matcheddata2_5$treated)

round(balance_tb2_5[,4:5], 3)

## Plot
abs.stand.diff.before=abs(balance_tb2_5[,5])
abs.stand.diff.after=abs(balance_tb2_5[,4])
covariates = rownames(balance_tb2_5)
plot.dataframe = data.frame(abs.stand.diff=c(abs.stand.diff.before, abs.stand.diff.after),
                            covariates=rep(covariates,2),
                            type=c(rep("Before", length(covariates)),
                                   rep("After", length(covariates))))
plot.dataframe$covariates <- fct_rev(factor(plot.dataframe$covariates, levels = 
                                              c("resort_hotel", "is_repeated_guest",  "previous_cancellations", "previous_bookings_not_canceled",  
                                                "lead_time",  "total_stay",  "guests", "booking_changes",  "days_in_waiting_list",  "adr", 
                                                "required_car_parking_spaces", "total_of_special_requests", "arrival_date_month_February",  
                                                "arrival_date_month_March", "arrival_date_month_April",  "arrival_date_month_May", 
                                                "arrival_date_month_June",  "arrival_date_month_July", "arrival_date_month_August",  
                                                "arrival_date_month_September", "arrival_date_month_October",  "arrival_date_month_November", 
                                                "arrival_date_month_December",  "meal_Undefined",  "meal_SC",  "meal_HB",  "meal_FB", 
                                                "market_segment_Corporate",  "market_segment_Direct",  "market_segment_Groups", 
                                                "market_segment_Offline.TA.TO",  "market_segment_Online.TA", "market_segment_Others",  
                                                "deposit_type_Non.Refund",  "deposit_type_Refundable", "customer_type_Group",  
                                                "customer_type_Transient",  "customer_type_Transient.Party")))
ggplot(plot.dataframe, aes(x=abs.stand.diff, y=covariates)) +
  geom_point(size=5, aes(shape=factor(type))) +
  scale_shape_manual(values=c(4,1)) +
  geom_vline(xintercept=c(.1,.2), lty=2)

treated.y2 = matcheddata2_5$outcome[matcheddata2_5$treatment==1]
control.y2 = matcheddata2_5$outcome[matcheddata2_5$treatment==0]

#################

# Optimal matching (1 to 3)

result3 = match(datatemp$treatment, distmat, datatemp, ncontrol=3)
matcheddata3 = result3$data
treated.y3 = matcheddata3$outcome[matcheddata3$treatment==1]
control.y3 = matcheddata3$outcome[matcheddata3$treatment==0]

# Examining Balance
## Standardized difference and Love Plot

### Extract Xmat for the matched data
Xmat.matched = Xmat[as.numeric(rownames(matcheddata3)),]

# Calculate standardized differences
# Which non-categorical variables can be missing

missing.mat = matrix(rep(0, ncol(Xmat.full)*nrow(Xmat.full)),
                     ncol=ncol(Xmat.full))
missing.mat.matched = matrix(rep(0, ncol(Xmat.matched)*nrow(Xmat.matched)),
                             ncol=ncol(Xmat.matched))

# Put in NAs for all X variables which are missing and for which mean value has been imputed
Xmat.without.missing = Xmat.full
Xmat.without.missing.matched = Xmat.matched
for(i in 1:ncol(Xmat.full)){
  Xmat.without.missing[missing.mat[,i]==1, i] = NA
  Xmat.without.missing.matched[missing.mat.matched[,i]==1, i] = NA
}

balance_tb3 = check(Xmat.without.missing, Xmat.without.missing.matched,
                    datatemp.full$treated, matcheddata3$treated)

round(balance_tb3, 3)[,4:5]

## Plot
abs.stand.diff.before=abs(balance_tb3[,5])
abs.stand.diff.after=abs(balance_tb3[,4])
covariates = rownames(balance_tb3)
plot.dataframe = data.frame(abs.stand.diff=c(abs.stand.diff.before, abs.stand.diff.after),
                            covariates=rep(covariates,2),
                            type=c(rep("Before", length(covariates)),
                                   rep("After", length(covariates))))
ggplot(plot.dataframe, aes(x=abs.stand.diff, y=covariates)) +
  geom_point(size=5, aes(shape=factor(type))) +
  scale_shape_manual(values=c(4,1)) +
  geom_vline(xintercept=c(.1,.2), lty=2)


# Improving Match

# add asymmetric caliper for market_segment
distmat3_1 = addcaliper(distmat, datatemp$treated, datatemp$resort_hotel,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

distmat3_2 = addcaliper(distmat3_1, datatemp$treated, datatemp$is_repeated_guest,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

distmat3_3 = addcaliper(distmat3_2, datatemp$treated, datatemp$adr,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

distmat3_4 = addcaliper(distmat3_3, datatemp$treated, datatemp$total_stay,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

distmat3_5 = addcaliper(distmat3_4, datatemp$treated, datatemp$lead_time,
                        rg=c(-0.4, 0.6), stdev=TRUE, penalty=1000,
                        constant=FALSE)

result3_5 = match(datatemp$treatment, distmat3_5, datatemp, ncontrol=3)
matcheddata3_5 = result3_5$data

Xmat.matched = Xmat[as.numeric(rownames(matcheddata3_5)),]
missing.mat.matched = matrix(rep(0, ncol(Xmat.matched)*nrow(Xmat.matched)),
                             ncol=ncol(Xmat.matched))
Xmat.without.missing.matched = Xmat.matched
for(i in 1:ncol(Xmat.full)){
  Xmat.without.missing.matched[missing.mat.matched[,i]==1, i] = NA
}

balance_tb3_5 = check(Xmat.without.missing, Xmat.without.missing.matched,
                      datatemp.full$treated, matcheddata3_5$treated)

round(balance_tb3_5[,4:5], 3)

treated.y3 = matcheddata3_5$outcome[matcheddata3_5$treatment==1]
control.y3 = matcheddata3_5$outcome[matcheddata3_5$treatment==0]

#################

# Inference about Treatment after 1:1 Matching

hotel_data_1v1 = matrix(c(sum(matcheddata_1$outcome[matcheddata_1$treatment==1]),sum(matcheddata_1$outcome[matcheddata_1$treatment==0]),
                      sum((1-matcheddata_1$outcome)[matcheddata_1$treatment==1]),sum((1-matcheddata_1$outcome)[matcheddata_1$treatment==0])), ncol=2,
                    dimnames=list(treatment=c("Different Room","Control"),
                                  cancelled=c("Yes","No")))
hotel_data_1v1

## H0: (no effect)
## H1: (different room assigned increases booking cancelled)
fisher.test(hotel_data_1v1, alternative="greater")

# Logistic regression

logit1v1 = glm(outcome ~ treatment, data=matcheddata_1, family=binomial)
summary(logit1v1)

logit1v1_2 = glm(outcome ~ treated + resort_hotel + is_repeated_guest + previous_cancellations
                 + previous_bookings_not_canceled + lead_time + total_stay + guests
                 + booking_changes + days_in_waiting_list + adr
                 + required_car_parking_spaces + total_of_special_requests
                 + arrival_date_month_February + arrival_date_month_March
                 + arrival_date_month_April + arrival_date_month_May
                 + arrival_date_month_June + arrival_date_month_July
                 + arrival_date_month_August + arrival_date_month_September
                 + arrival_date_month_October + arrival_date_month_November
                 + arrival_date_month_December + meal_Undefined + meal_SC + meal_HB + meal_FB
                 + market_segment_Corporate + market_segment_Direct + market_segment_Groups
                 + market_segment_Offline.TA.TO + market_segment_Online.TA
                 + market_segment_Others + deposit_type_Non.Refund + deposit_type_Refundable 
                 + customer_type_Group + customer_type_Transient + customer_type_Transient.Party,
                 data=matcheddata_1, family=binomial)


summary(logit1v1_2)

#################

# Inference about Treatment after 1:2 Matching

hotel_data_1v2 = matrix(c(sum(matcheddata2_5$outcome[matcheddata2_5$treatment==1]),sum(matcheddata2_5$outcome[matcheddata2_5$treatment==0]),
                          sum((1-matcheddata2_5$outcome)[matcheddata2_5$treatment==1]),sum((1-matcheddata2_5$outcome)[matcheddata2_5$treatment==0])), ncol=2,
                        dimnames=list(treatment=c("Different Room","Control"),
                                      cancelled=c("Yes","No")))
hotel_data_1v2

## H0: (no effect)
## H1: (different room assigned increases booking cancelled)
fisher.test(hotel_data_1v2, alternative="greater")

# Logistic regression

logit1v2 = glm(outcome ~ treatment, data=matcheddata2_5, family=binomial)
summary(logit1v2)

logit1v2_2 = glm(outcome ~ ., data=matcheddata2_5, family=binomial)
summary(logit1v2_2)

##################

# Sensivitity Analysis for McNemar's Test Statistic

str(matcheddata_1)

table(matcheddata_1$outcome)
prop.table(table(matcheddata_1$outcome))
table(matcheddata_1$treatment)
prop.table(table(matcheddata_1$treatment))
table(matcheddata_1$treatment, matcheddata_1$outcome)
prop.table(table(matcheddata_1$treatment, matcheddata_1$outcome), margin=2) #col-sum
prop.table(table(matcheddata_1$treatment, matcheddata_1$outcome), margin=1) # row-sum


table(matcheddata2_5$mset)

sa1 <- data.frame(matcheddata_1 %>%
                    group_by(mset) %>%
                    summarize(count1 = sum(outcome == 1)))

sa1$count1[sa1$count1==0] <- -1
sum(sa1[sa1$count1==-1,]$count1)
sum(sa1[sa1$count1==1,]$count1) #738
sum(sa1[sa1$count1==2,]$count1) #256/2=128

sa0 <- data.frame(matcheddata_1 %>%
                    group_by(mset) %>%
                    summarize(count0 = sum(outcome == 0)))

sa11 <- data.frame(matcheddata_1 %>%
                     group_by(mset) %>%
                     summarize(count11 = sum(outcome == 1 & treatment == 1)))

sa10 <- data.frame(matcheddata_1 %>%
                     group_by(mset) %>%
                     summarize(count10 = sum(outcome == 1 & treatment == 0)))

sa01 <- data.frame(matcheddata_1 %>%
                     group_by(mset) %>%
                     summarize(count01 = sum(outcome == 0 & treatment == 1)))

sa00 <- data.frame(matcheddata_1 %>%
                     group_by(mset) %>%
                     summarize(count00 = sum(outcome == 0 & treatment == 0)))


sa_inner = merge(x=sa1, y=sa11, by="mset")
sa_inner = merge(x=sa_inner, y=sa10, by="mset")
sa_inner = merge(x=sa_inner, y=sa0, by="mset")
sa_inner = merge(x=sa_inner, y=sa01, by="mset")
sa_inner = merge(x=sa_inner, y=sa00, by="mset")

sum(sa_inner[sa_inner$count11==1 & sa_inner$count1!=2,]$count11) #167
sum(sa_inner[sa_inner$count10==1 & sa_inner$count1!=2,]$count10) #571

## Let D be the number of discordant pairs = 738
## Tobs be the number of discordant pairs in which treated unit has a 1 for outcome =167

## and Gamma be the sensitivity parameter exp(gamma)
sens.analysis.mcnemar=function(D,Tobs,Gamma){
  p.positive=Gamma/(1+Gamma);
  p.negative=1/(1+Gamma);
  lowerbound=1-pbinom(Tobs-1,D,p.negative);
  upperbound=1-pbinom(Tobs-1,D,p.positive);
  list(lowerbound=lowerbound,upperbound=upperbound)
}
D=738;Tobs=167
sen.tb=matrix(nrow=6,ncol=2)
for (g in 1:6) sen.tb[g,]=unlist(sens.analysis.mcnemar(D,Tobs,g))
round(sen.tb, 4)







