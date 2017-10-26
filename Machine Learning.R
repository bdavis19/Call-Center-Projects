
#########################################################################################################
# Logistic Regression
#     This is an attempt predict whether the call center will have a high or low call volume day.
#########################################################################################################
library(data.table)
library(tidyverse)
library(lubridate)

# Load Data
primaryApps <- fread("apps_primary_refined.csv")

# Check missing values
sapply(primaryApps, function(x) sum(is.na(x)))
sapply(primaryApps, function(x) length(unique(x)))
library(Amelia)
missmap(primaryApps, main="Missing values vs observed")

# Get date into some workable formats
primaryApps$statTimestamp <- ymd_hms(primaryApps$statTimestamp)
primaryApps <- primaryApps %>% mutate(Date = as.Date(statTimestamp), statTimestamp = NULL, ApplicationName = NULL, ApplicationID = NULL)
primaryApps <- aggregate(. ~ Date, primaryApps, FUN='sum')

# Average calls per day
avgCallsPerDay <- sum(primaryApps$CallsOffered) / length(unique(paste(month(primaryApps$Date), day(primaryApps$Date), sep = "/")))

# Create new df so that each observation is a daily total
primaryApps <- primaryApps %>% group_by(Date) %>% summarise(Sum = sum(CallsOffered))

# Add classification variable
primaryApps <- primaryApps %>% mutate(Volume = ifelse(Sum > avgCallsPerDay, 1, 0))

# Fit the model
set.seed(1979)
split <- sample.split(primaryApps$Volume, SplitRatio = 0.7)
train <- subset(primaryApps, split == TRUE)
test <- subset(primaryApps, split == FALSE)
primaryApps.mod <- glm(Volume ~ mday(Date) + month(Date) + weekdays(Date), family=binomial, data=train)
summary(primaryApps.mod)
anova(primaryApps.mod, test="Chisq")

# Assessing Predictability
primaryApps.pred <- predict(primaryApps.mod, newdata=test, type='response')
primaryApps.pred <- ifelse(primaryApps.pred > 0.5, 1, 0)
misClasificError <- mean(primaryApps.pred != test$Volume)
print(paste('Accuracy', 1-misClasificError))

library(ROCR)
p <- predict(primaryApps.mod, newdata=test, type='response')
pr <- prediction(p, test$Volume)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc
primaryApps.sse <- sum((primaryApps.pred - test$Volume)^2)
primaryApps.sse
table(test$Volume, primaryApps.pred)

###############################################################################################
# Tree Regression
###############################################################################################
library(caTools)
library(rpart)
library(rpart.plot)

set.seed(1979)

# Get the data into test and training sets
split <- sample.split(primaryApps$Volume, SplitRatio = 0.7)
primaryApps.train <- subset(primaryApps, split == TRUE)
primaryApps.test <- subset(primaryApps, split == FALSE)
primaryApps.tree <- rpart(Volume ~ month(Date) + mday(Date) + weekdays(Date), data = primaryApps.train)
prp(primaryApps.tree)
primaryApps.tree.pred <- predict(primaryApps.tree, newdata = primaryApps.test)
primaryApps.tree.sse <- sum((primaryApps.tree.pred - primaryApps.test$Volume)^2)
primaryApps.tree.sse

# Random forest appears to be a better model in this case.