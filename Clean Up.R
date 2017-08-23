# Environment
library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)

# Load Data
apps <- fread("applications_sanitized.csv")
skills <- fread("skillsets_sanitized.csv")
agents_ptd <- fread("ptdAgents_sanitized.csv", na.strings = c("", "NA"))
answered <- fread("answered_sanitized.csv", na.strings = c("","NA"))

# Missing Values (the data was clean to begin with)
grep("^$", apps)
sum(is.na(apps))
sum(is.na(skills))
grep("^$", skills)
grep("^$", agents_ptd)
sum(is.na(agents_ptd))
grep("^$", answered)
sum(is.na(answered))

# Filter for applications and skillsets to use for analysis
apps_primary <- apps[ApplicationID %in% c(10032,10110,10104,10117,10038,10025, 10021, 10033, 10120, 10037, 10115)]
skills_primary <- skills[SkillsetID %in% c(10054,10178,10188,10189,10190,10058,10037, 10055,10029,10168,10180)]
setDT(answered)
answered_primary <- answered[ApplicationID %in% c(10032,10110,10104,10117,10038,10025, 10021, 10033, 10120, 10037, 10115) | SkillsetID %in% c(10054,10178,10188,10189,10190,10058,10037, 10055,10029,10168,10180)]

# Remove NA, remove spares, make column names better, remove training room records, remove names and columns
# from agents_ptd
agents_ptd <- agents_ptd[rowSums(is.na(agents_ptd)) != ncol(agents_ptd),]
agents_ptd <- agents_ptd %>% filter(!(is.na(Ext)), Employee.Name..Last.name..First.name. != "spare")
colnames(agents_ptd)[colnames(agents_ptd) == "Ext"] <- "AgentID"
colnames(agents_ptd)[colnames(agents_ptd) == "Network.login"] <- "Username"
agents_ptd <- agents_ptd %>% filter(Employee.Name..Last.name..First.name. != "training room")
agents_ptd <- agents_ptd %>% separate(Employee.Name..Last.name..First.name., c("FirstName", "LastName"), sep = ",", remove = TRUE)
agents_ptd <- agents_ptd %>% mutate(FirstName = NULL, LastName = NULL, email = NULL, Permission = NULL, Monitoring = NULL, Recording = NULL, X = NULL, X.1 = NULL)

# Remove NA, remove colums
answered_primary <- answered_primary %>% mutate(CCMID = NULL, ProviderContactID = NULL, Originator = NULL, RoutePoint = NULL, ApplicationStartStamp = NULL, LastTreatmentID = NULL, LastTreatmentStamp = NULL, LastTreatmentTime = NULL, SkillsetQueuedStamp = NULL, InitialDisposition = NULL, ServiceStamp = NULL, NumberOfTimesOnHold = NULL, NumberOfTimesRTQ = NULL, FinalDisposition = NULL, FinalDispositionStamp = NULL, NextSegmentID = NULL, ContactOriginatedStamp = NULL, DisconnectSource = NULL, AgentName = NULL, SupervisorName = NULL, SupervisorID = NULL)
answered_primary <- na.omit(answered_primary, c("AgentID"))


# We will be putting the timestamps into the correct format for use.
setDT(apps_primary)
apps_primary$statTimestamp <- ymd_hms(apps_primary$statTimestamp)
setDT(skills_primary)
skills_primary$statTimestamp <- ymd_hms(skills_primary$statTimestamp)
setDT(answered_primary)
answered_primary$OriginatedStamp <- ymd_hms(answered_primary$OriginatedStamp)
apps$statTimestamp <- ymd_hms(apps$statTimestamp)

# We'll need to change the chr variables to numerics
apps_primary[ , 2:25 :=lapply(.SD, as.numeric), .SDcols = 2:25]
skills_primary[ , 2:26 :=lapply(.SD, as.numeric), .SDcols = 2:26]
answered_primary <- answered_primary[,c(6, 1:5, 7:11)]


# Remove 2015 data as it's erroneously in the data set
apps_primary <- apps_primary[year(statTimestamp) != 2015]
skills_primary <- skills_primary[year(statTimestamp) != 2015]

# Remove December data as it's just data from a few days.
apps_primary <- apps_primary[month(statTimestamp) != 12]
skills_primary <- skills_primary[month(statTimestamp) != 12]

# Create new files for those that needed clean up.
write.csv(apps_primary, "apps_primary_refined.csv", row.names = FALSE)
write.csv(skills_primary, "skills_primary_refined.csv", row.names = FALSE)
write.csv(agents_ptd, "agents_ptd_refined.csv", row.names = FALSE)
write.csv(answered_primary, "answered_primary_refined.csv", row.names = FALSE)

# BEGIN EXPLORATORY ANALYSIS
# Call Volume
# Add factors
apps_primary$ApplicationName <- apps_primary$ApplicationName %>% as.factor()

# Plot monthly view of calls offered, abandoned, answered
ggplot(apps_primary, aes(x = month(statTimestamp), y = CallsOffered, fill = ApplicationName)) + 
  geom_col()
ggplot(apps_primary, aes(x = month(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col()
ggplot(skills_primary, aes(x = month(statTimestamp), y = CallsAnswered, fill = SkillsetName)) + 
  geom_col()

# week
ggplot(apps_primary, aes(x = week(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_col()
ggplot(apps_primary, aes(x = week(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col()
ggplot(skills_primary, aes(x = week(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_col()

# day of month
ggplot(apps_primary, aes(x = mday(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_col()
ggplot(apps_primary, aes(x = mday(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col()
ggplot(skills_primary, aes(x = mday(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_col()

# day of week
ggplot(apps_primary, aes(x = wday(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_col()
ggplot(apps_primary, aes(x = wday(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col()
ggplot(skills_primary, aes(x = wday(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_col()

# Time of day
xscale <- scale_x_continuous(breaks = c(0:23),labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
ggplot(apps_primary, aes(x = hour(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_col() +
  xscale
ggplot(apps_primary, aes(x = hour(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col() +
  xscale
ggplot(skills_primary, aes(x = hour(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_col() +
  xscale

# Wait Time by month, week day, week, day of month, hour
apps_primary %>% group_by(month(statTimestamp), ApplicationName) %>% rename(month = "month(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col()
apps_primary %>% group_by(wday(statTimestamp), ApplicationName) %>% rename(month = "wday(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col()
apps_primary %>% group_by(week(statTimestamp), ApplicationName) %>% rename(month = "week(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col()
apps_primary %>% group_by(mday(statTimestamp), ApplicationName) %>% rename(month = "mday(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col()
apps_primary %>% group_by(hour(statTimestamp), ApplicationName) %>% rename(month = "hour(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col() +
  xscale

# Talk Time by month, week day, week, day of month, hour
apps_primary %>% group_by(month(statTimestamp), ApplicationName) %>% rename(month = "month(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
  ggplot(aes(x = month, y = mean_TalkTime, fill = ApplicationName)) +
  geom_col()
apps_primary %>% group_by(wday(statTimestamp), ApplicationName) %>% rename(month = "wday(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
  ggplot(aes(x = month, y = mean_TalkTime, fill = ApplicationName)) +
  geom_col()
apps_primary %>% group_by(week(statTimestamp), ApplicationName) %>% rename(month = "week(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
  ggplot(aes(x = month, y = mean_TalkTime, fill = ApplicationName)) +
  geom_col()
apps_primary %>% group_by(mday(statTimestamp), ApplicationName) %>% rename(month = "mday(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
  ggplot(aes(x = month, y = mean_TalkTime, fill = ApplicationName)) +
  geom_col()
apps_primary %>% group_by(hour(statTimestamp), ApplicationName) %>% rename(month = "hour(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
  ggplot(aes(x = month, y = mean_TalkTime, fill = ApplicationName)) +
  geom_col() +
  xscale

#####################
# Implement Erlang-C
#####################
#
# Calculate agents needed:
#   If the service level  is specified and you want to calculate the number of agents needed, then you must do a bit
#     of trial and error. You have to find the number of agents that will just achieve the service level you want.
#     A good starting point is the traffic intensity, rounded up to next integer. Then increase the number of agents
#     until the required service level is met.

# Calls for 8am - 6pm, get to calls per second
# Specify Arrival Rate A: 360 calls in 15 min/900 seconds
##############################
# Need Unanswered, 2024
##############################
apps_primary_total <- apps_primary %>% filter(hour(statTimestamp) >= 8 & hour(statTimestamp) <= 17) %>% select(CallsOffered) %>% sum()

# Join calls answered, determine ptd calls from 8am-6pm
answered_primary_ptd <- inner_join(answered_primary, agents_ptd, by = "AgentID")
num_calls_ptd <- answered_primary_ptd %>% filter(hour(OriginatedStamp) >= 8 & hour(OriginatedStamp) <= 17) %>% nrow()

# Remove ptd calls
total_calls <- apps_primary_total - num_calls_ptd

# Find pn calls, remove them
#####################
# DTA & Route to PN
#####################
num_calls_pn <- apps %>% filter(ApplicationID == 10111, hour(statTimestamp) >=8 & hour(statTimestamp) <= 17) %>% select(CallsOffered) %>% sum()
total_calls <- total_calls - num_calls_pn

# Arrival rate
calls_per_hour <- total_calls / (30 * 10)
calls_per_quarter <- calls_per_hour / 4
arrival_rate <- calls_per_quarter / 900

# Talk Time (includes hold time) + Not Ready Time
##########################
# Need Agent Performance
##########################
# Specify duration Ts
talkTime <- skills_primary %>% filter(hour(statTimestamp) >=8 & hour(statTimestamp) <= 17 & month(statTimestamp) == 6) %>% select(TalkTime) %>% sum()
callsAnswered <- skills_primary %>% filter(hour(statTimestamp) >=8 & hour(statTimestamp) <= 17 & month(statTimestamp) == 6) %>% select(CallsAnswered) %>% sum()
postTime <- skills_primary %>% filter(hour(statTimestamp) >=8 & hour(statTimestamp) <= 17 & month(statTimestamp) == 6) %>% select(PostCallProcessingTime) %>% sum()
duration <- (talkTime + postTime)/callsAnswered

# Specify number of agents m: num agents
numAgents <- 24

# Calculate traffic intensity u: u = A * Ts
trafficIntensity <- arrivalRate * duration

# Calculate agent occupancy p: p = u / m
agentOccupancy <- trafficIntensity / numAgents

# Erlang-C Ec(m, u) = (u^m/m!) / (u^m/m! + (1 - p) m-1{k=0 u^k/k!)
numerator <- trafficIntensity^numAgents/factorial(numAgents)
sigma <- 0
for (i in 0:(numAgents-1)){
  sigma <- sigma + trafficIntensity^i / factorial(i)
}
denominator <- numerator + ((1 - agentOccupancy) * sigma)
erlangC <- numerator/denominator

# Calculate probability of waiting Ec(m,u): Ec(m, u) * 100
waitProb <- erlangC * 100

# Calculate average speed of answer Tw: Tw = (Ec(m, u) * T) / (m * (1 - p))
avgSpeedAnswer <- (erlangC * duration) / (numAgents * (1 - agentOccupancy))

# Calculate Seravice level:
#   t = target answer time
#   W(t) = Prob(waiting time <= t): = 1 * Ec(m, u) * e^(-(m-u) * t/Ts)
targetAnswerTime <- 60
exponent <- -(numAgents - trafficIntensity) * (targetAnswerTime / duration)
serviceLevel <- 1 - erlangC * exp(exponent)