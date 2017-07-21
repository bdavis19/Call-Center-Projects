# Environment
library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)

# Call Volume
# Focus on the major application entry points: 10025, 10021, 10139, 10120, 10037, 10115
primaryApps <- apps[ApplicationID %in% c(10025, 10021, 10139, 10120, 10037, 10115)]
primarySkills <- skills[SkillsetID %in% c(10058,10037, 10055,10029,10168,10180)]


# Plot monthly view of calls offered, abandoned, answered
ggplot(primaryApps, aes(x = month(statTimestamp), y = CallsOffered, fill = ApplicationName)) + 
  geom_bar(stat="identity")
ggplot(primaryApps, aes(x = month(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_bar(stat="identity")
ggplot(primarySkills, aes(x = month(statTimestamp), y = CallsAnswered, fill = SkillsetName)) + 
  geom_bar(stat="identity")

# week
ggplot(primaryApps, aes(x = week(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_bar(stat="identity")
ggplot(primaryApps, aes(x = week(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_bar(stat="identity")
ggplot(primarySkills, aes(x = week(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_bar(stat="identity")

# day of month
ggplot(primaryApps, aes(x = mday(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_bar(stat="identity")
ggplot(primaryApps, aes(x = mday(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_bar(stat="identity")
ggplot(primarySkills, aes(x = mday(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_bar(stat="identity")

# day of week
ggplot(primaryApps, aes(x = wday(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_bar(stat="identity")
ggplot(primaryApps, aes(x = wday(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_bar(stat="identity")
ggplot(primarySkills, aes(x = wday(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_bar(stat="identity")

# Time of day
xscale <- scale_x_continuous(breaks = c(0:23),labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
ggplot(primaryApps, aes(x = hour(statTimestamp), y = CallsOffered, fill = ApplicationName)) +
  geom_bar(stat="identity") +
  xscale
ggplot(primaryApps, aes(x = hour(statTimestamp), y = CallsAbandoned, fill = ApplicationName)) +
  geom_bar(stat="identity") +
  xscale
ggplot(primarySkills, aes(x = hour(statTimestamp), y = CallsAnswered, fill = SkillsetName)) +
  geom_bar(stat="identity") +
  xscale

# Wait Time
p <- ggplot(primaryApps, aes(x = month(statTimestamp), y = WaitTime, fill = ApplicationName)) +
  stat_summary(fun.y = "mean", geom = "bar")
ggplotly(p)
p <- ggplot(primaryApps, aes(x = wday(statTimestamp), y = WaitTime, fill = ApplicationName)) +
  stat_summary(fun.y = "mean", geom = "bar")
ggplotly(p)
p <- ggplot(primaryApps, aes(x = mday(statTimestamp), y = WaitTime, fill = ApplicationName)) +
  stat_summary(fun.y = "mean", geom = "bar")
ggplotly(p)
p <- ggplot(primaryApps, aes(x = hour(statTimestamp), y = WaitTime, fill = ApplicationName)) +
  stat_summary(fun.y = "mean", geom = "bar")
ggplotly(p)