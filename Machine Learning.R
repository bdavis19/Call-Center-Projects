
#########################################################################################################
# Logistic Regression
#     This is an attempt predict whether the call center will have a high or low call volume day.
#########################################################################################################
library(data.table)
library(tidyverse)
# Load Data
apps_primary <- fread("apps_primary_refined.csv")

# Check missing values
sapply(apps_primary, function(x) sum(is.na(x)))
sapply(apps_primary, function(x) length(unique(x)))
library(Amelia)
missmap(apps_primary, main="Missing values vs observed")

# Get data into df by day
apps_primary_days <- apps_primary
apps_primary_days <- apps_primary_days %>% mutate(Date=as.Date(statTimestamp), statTimestamp = NULL, ApplicationID=NULL, ApplicationName=NULL)
apps_primary_days <- aggregate(. ~ Date, apps_primary_days, FUN='sum')

# Find average number of calls and add a category for Below Average and Above Average
avg_calls <- apps_primary_days %>% select(CallsOffered) %>% sum()
avg_calls <- avg_calls/nrow(apps_primary_days)
apps_primary_days <- apps_primary_days %>% mutate(Volume=ifelse(CallsOffered < avg_calls, 1, 0))
apps_primary_days$Volume <- apps_primary_days$Volume %>% as.factor()

# Fit the model
train <- apps_primary_days[1:132,]
test <- apps_primary_days[133:176,]

model <- glm(Volume ~ mday(Date) + month(Date) + weekdays(Date), family=binomial, data=train)
summary(model)
anova(model, test="Chisq")

# Assessing Predictability
fitted <- predict(model, newdata=train, type='response')
fitted <- ifelse(fitted > 0.5, 1, 0)
misClasificError <- mean(fitted != train$Volume)
print(paste('Accuracy', 1-misClasificError))

library(ROCR)
p <- predict(model, newdata=train, type='response')
pr <- prediction(p, train$Volume)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc
