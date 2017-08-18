# Environment
library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)

# Load Data
# PREFIX: m - master list
mapps <- fread("applications_sanitized.csv")
mskills <- fread("skillsets_sanitized.csv")
mcdns <- fread("cdns_sanitized.csv")
apps <- fread("aggint_application.csv")
skills <- fread("aggint_skillset.csv")

# Missing Values (the data was clean to begin with)
grep("^$", mapps)
sum(is.na(mapps))
grep("^$", mskills)
sum(is.na(mskills))
grep("^$", mcdns)
sum(is.na(mcdns))
grep("^$", apps)
sum(is.na(apps))
sum(is.na(skills))
grep("^$", skills)

# We will be putting the timestamps into the correct format for use.
apps$statTimestamp <- ymd_hms(apps$statTimestamp)
skills$statTimestamp <- ymd_hms(skills$statTimestamp)

# We'll need to change the chr variables to numerics
apps[ , 2:25 :=lapply(.SD, as.numeric), .SDcols = 2:25]
skills[ , 2:26 :=lapply(.SD, as.numeric), .SDcols = 2:26]

# Remove 2015 data as it's erroneously in the data set
apps <- apps[year(statTimestamp) != 2015]
skills <- skills[year(statTimestamp) != 2015]

# Remove December data as it's just data from a few days.
apps <- apps[month(statTimestamp) != 12]
apps <- merge(apps, mapps, by="ApplicationID")
apps %>% setnames("Name", "ApplicationName")
skills <- skills[month(statTimestamp) != 12]
skills <- merge(skills, mapps, by="ApplicationID")
skills %>% setnames("Name", "ApplicationName")
skills <- merge(skills, mskills, by="SkillsetID")
skills %>% setnames("Name", "SkillsetName")

# Create new files for those that needed clean up.
write.csv(apps, "aggint_application_refined.csv", row.names = FALSE)
write.csv(skills, "agg_int_skillset_refined.csv", row.names = FALSE)

# BEGIN EXPLORATORY ANALYSIS
# Call Volume
# Focus on the major application entry points: 10025, 10021, 10139, 10120, 10037, 10115
primaryApps <- apps[ApplicationID %in% c(10032,10110,10104,10117,10038,10025, 10021, 10033, 10120, 10037, 10115)]
primarySkills <- skills[SkillsetID %in% c(10054,10178,10188,10189,10190,10058,10037, 10055,10029,10168,10180)]

# Add factors
primaryApps$ApplicationName <- primaryApps$ApplicationName %>% as.factor()

# Plot monthly view of calls offered, abandoned, answered
ggplot(primaryApps, aes(x = month(statTimestamp), y = CallsOffered, fill = ApplicationName)) + 
  geom_col()
ggplot(primaryApps, aes(x = month(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col()
ggplot(primarySkills, aes(x = month(statTimestamp), y = CallsAnswered, fill = SkillsetName)) + 
  geom_col()

# week
ggplot(primaryApps, aes(x = week(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_col()
ggplot(primaryApps, aes(x = week(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col()
ggplot(primarySkills, aes(x = week(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_col()

# day of month
ggplot(primaryApps, aes(x = mday(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_col()
ggplot(primaryApps, aes(x = mday(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col()
ggplot(primarySkills, aes(x = mday(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_col()

# day of week
ggplot(primaryApps, aes(x = wday(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_col()
ggplot(primaryApps, aes(x = wday(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col()
ggplot(primarySkills, aes(x = wday(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_col()

# Time of day
xscale <- scale_x_continuous(breaks = c(0:23),labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
ggplot(primaryApps, aes(x = hour(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_col() +
  xscale
ggplot(primaryApps, aes(x = hour(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_col() +
  xscale
ggplot(primarySkills, aes(x = hour(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_col() +
  xscale

# Wait Time by month, week day, week, day of month, hour
primaryApps %>% group_by(month(statTimestamp), ApplicationName) %>% rename(month = "month(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col()
primaryApps %>% group_by(wday(statTimestamp), ApplicationName) %>% rename(month = "wday(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col()
primaryApps %>% group_by(week(statTimestamp), ApplicationName) %>% rename(month = "week(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col()
primaryApps %>% group_by(mday(statTimestamp), ApplicationName) %>% rename(month = "mday(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col()
primaryApps %>% group_by(hour(statTimestamp), ApplicationName) %>% rename(month = "hour(statTimestamp)") %>% summarise(mean_WaitTime = mean(WaitTime)) %>% 
  ggplot(aes(x = month, y = mean_WaitTime, fill = ApplicationName)) +
  geom_col() +
  xscale

# Talk Time by month, week day, week, day of month, hour
primaryApps %>% group_by(month(statTimestamp), ApplicationName) %>% rename(month = "month(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
  ggplot(aes(x = month, y = mean_TalkTime, fill = ApplicationName)) +
  geom_col()
primaryApps %>% group_by(wday(statTimestamp), ApplicationName) %>% rename(month = "wday(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
  ggplot(aes(x = month, y = mean_TalkTime, fill = ApplicationName)) +
  geom_col()
primaryApps %>% group_by(week(statTimestamp), ApplicationName) %>% rename(month = "week(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
  ggplot(aes(x = month, y = mean_TalkTime, fill = ApplicationName)) +
  geom_col()
primaryApps %>% group_by(mday(statTimestamp), ApplicationName) %>% rename(month = "mday(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
  ggplot(aes(x = month, y = mean_TalkTime, fill = ApplicationName)) +
  geom_col()
primaryApps %>% group_by(hour(statTimestamp), ApplicationName) %>% rename(month = "hour(statTimestamp)") %>% summarise(mean_TalkTime = mean(TalkTime)) %>% 
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
callsWorkHours <- primaryApps %>% filter(hour(statTimestamp) >= 8 & hour(statTimestamp) <= 17 & month(statTimestamp) == 6) %>% select(CallsOffered) %>% sum()
arrivalPerHour <- callsWorkHours / (30 * 10)
arrivalRate <- arrivalPerHour / 4
arrivalRate <- arrivalRate / 900

# Talk Time + wrap up + hold
# Specify duration Ts: AHT
talkTime <- primarySkills %>% filter(hour(statTimestamp) >=8 & hour(statTimestamp) <= 17 & month(statTimestamp) == 6) %>% select(TalkTime) %>% sum()
callsAnswered <- primarySkills %>% filter(hour(statTimestamp) >=8 & hour(statTimestamp) <= 17 & month(statTimestamp) == 6) %>% select(CallsAnswered) %>% sum()
postTime <- primarySkills %>% filter(hour(statTimestamp) >=8 & hour(statTimestamp) <= 17 & month(statTimestamp) == 6) %>% select(PostCallProcessingTime) %>% sum()
duration <- (talkTime + postTime)/callsAnswered

# Specify number of agents m: num agents
numAgents <- 38

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
targetAnswerTime <- 45
exponent <- -(numAgents - trafficIntensity) * (targetAnswerTime / duration)
serviceLevel <- 1 - erlangC * exp(exponent)
