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
aperf <-fread("agent_perf_sanitized.csv", na.strings = c("","NA", "\\N"))

# Filter for applications and skillsets to use for analysis
apps_primary <- apps[ApplicationName %in% c("xfer_from_bcs_script", "bilingual", "p_2_xfer_script", "p_csr_xfer_2_script", "ivr_transfer_script", "xfer_from_new_agent", "disconnect_script", "dbb_script", "hs_script", "install_upgrades_script", "main_csr_billing_script", "troubleshooting_script", "unanswered_pn_calls_script", "xfer_from_pn_script")]
skills_primary <- skills[SkillsetName %in% c("bcs_xfer", "bilingual", "xfer_from_ivr", "xfer_from_pn", "unanswered_pn_calls", "xfer_from_new_agent", "p_csr_xfer_2_brc", "p_2_xfer", "disconnect", "dbb", "hs", "main_csr_billing", "troubleshooting", "install_upgrades")]
setDT(answered)
answered_primary <- answered[ApplicationName %in% c("xfer_from_bcs_script", "bilingual", "p_2_xfer_script", "p_csr_xfer_2_script", "ivr_transfer_script", "xfer_from_new_agent", "disconnect_script", "dbb_script", "hs_script", "install_upgrades_script", "main_csr_billing_script", "troubleshooting_script", "unanswered_pn_calls_script", "xfer_from_pn_script") | SkillsetName %in% c("bcs_xfer", "bilingual", "xfer_from_ivr", "xfer_from_pn", "unanswered_pn_calls", "xfer_from_new_agent", "p_csr_xfer_2_brc", "p_2_xfer", "disconnect", "dbb", "hs", "main_csr_billing", "troubleshooting", "install_upgrades")]

# Remove NA, remove spares, make column names better, remove training room records, remove names and columns
# that aren't needed from agents_ptd
agents_ptd <- agents_ptd[rowSums(is.na(agents_ptd)) != ncol(agents_ptd),]
agents_ptd <- agents_ptd %>% filter(!(is.na(Ext)))
colnames(agents_ptd)[colnames(agents_ptd) == "Ext"] <- "AgentID"

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
aperf$statTimestamp <- ymd_hms(aperf$statTimestamp)

# We'll need to change some chr variables to numerics
apps_primary[ , 2:25 :=lapply(.SD, as.numeric), .SDcols = 2:25]
skills_primary[ , 2:26 :=lapply(.SD, as.numeric), .SDcols = 2:26]
answered_primary <- answered_primary[,c(6, 1:5, 7:11)]

# Remove 2015 data as it's erroneously in the data set
apps_primary <- apps_primary[year(statTimestamp) != 2015]
skills_primary <- skills_primary[year(statTimestamp) != 2015]

# Remove December data as it's just data from a few days.
apps_primary <- apps_primary[month(statTimestamp) != 12]
skills_primary <- skills_primary[month(statTimestamp) != 12]

# Only keep data for the least amount I have between all sets to work in the same time frame/totals
# The agent performance only goes back to 2/26. The newest common date between sets is 8/20
apps_primary <- apps_primary %>% filter(statTimestamp >= "2017-02-26" & statTimestamp <= "2017-08-20")
aperf <- aperf %>% filter(statTimestamp >= "2017-02-26" & statTimestamp <= "2017-08-20")
skills_primary <- skills_primary %>% filter(statTimestamp >= "2017-02-26" & statTimestamp <= "2017-08-20")
answered_primary <- answered_primary %>% filter(OriginatedStamp >= "2017-02-26" & OriginatedStamp <= "2017-08-20" & HandlingTime > 0)


# Create new files for those that needed clean up.
write.csv(apps_primary, "apps_primary_refined.csv", row.names = FALSE)
write.csv(skills_primary, "skills_primary_refined.csv", row.names = FALSE)
write.csv(agents_ptd, "agents_ptd_refined.csv", row.names = FALSE)
write.csv(answered_primary, "answered_primary_refined.csv", row.names = FALSE)
write.csv(aperf, "agent_perf_refined.csv", row.names = FALSE)

#########################################################################################################
# BEGIN EXPLORATORY ANALYSIS PLOTS
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
#########################################################################################################

#########################################################################################################
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
# Specify Arrival Rate A: X calls in 15 min/900 seconds
apps_primary_total <- apps_primary %>% filter(hour(statTimestamp) >= 8 & hour(statTimestamp) <= 17) %>% select(CallsOffered) %>% sum()

# Join calls answered, determine ptd calls from 8am-6pm
answered_primary_ptd <- inner_join(answered_primary, agents_ptd, by = "AgentID")
num_calls_ptd <- answered_primary_ptd %>% filter(hour(OriginatedStamp) >= 8 & hour(OriginatedStamp) <= 17) %>% nrow()

# Remove ptd calls from total offered. They are not 100% available for calls as they aren't dedicated employees
total_calls <- apps_primary_total - num_calls_ptd

# Find pn calls, remove them - they are a vendor. With no insight to their queue/agents/etc it's best to remove them 
# and only deal with calls the client is servicing
num_calls_pn <- apps %>% filter(ApplicationName == "route_to_pn", hour(statTimestamp) >=8 & hour(statTimestamp) <= 17) %>% select(CallsOffered) %>% sum()
total_calls <- total_calls - num_calls_pn

# Arrival rate
total_days <- sum(days_in_month(c(3,4,5,6,7)))
calls_per_hour <- total_calls / (total_days * 10)
calls_per_quarter <- calls_per_hour / 4
arrival_rate <- calls_per_quarter / 900

# Talk Time (includes hold time) + Not Ready Time, remove ptd talk time since those calls are discarded
# Specify duration Ts
talk_time_ptd <- answered_primary_ptd %>% select(HandlingTime) %>% sum()
talk_time <- skills_primary %>% filter(hour(statTimestamp) >=8 & hour(statTimestamp) <= 17) %>% select(TalkTime) %>% sum()
total_talk_time <- talk_time - talk_time_ptd
not_ready <- aperf %>% filter(hour(statTimestamp) >= 8 & hour(statTimestamp) <= 17) %>% select(NotReadyTime) %>% sum()
total_talk_time <- talk_time + not_ready

# Find calls answered, and remove ptd calls since those are discarded
calls_answered <- skills_primary %>% filter(hour(statTimestamp) >=8 & hour(statTimestamp) <= 17) %>% select(CallsAnswered) %>% sum()
calls_answered <- calls_answered - num_calls_ptd

# Calculate the duraction of calls
duration <- total_talk_time/calls_answered

# Specify number of agents m: num agents
num_agents <- 37

# Calculate traffic intensity u: u = A * Ts
traffic_intensity <- arrival_rate * duration

# Calculate agent occupancy p: p = u / m
agent_occupancy <- traffic_intensity / num_agents

# Erlang-C Ec(m, u) = (u^m/m!) / (u^m/m! + (1 - p) m-1{k=0 u^k/k!)
numerator <- traffic_intensity^num_agents/factorial(num_agents)
sigma <- 0
for (i in 0:(num_agents-1)){
  sigma <- sigma + traffic_intensity^i / factorial(i)
}
denominator <- numerator + ((1 - agent_occupancy) * sigma)
erlangC <- numerator/denominator

# Calculate probability of waiting Ec(m,u): Ec(m, u) * 100
wait_prob <- erlangC * 100

# Calculate average speed of answer Tw: Tw = (Ec(m, u) * T) / (m * (1 - p))
avg_speed_answer <- (erlangC * duration) / (num_agents * (1 - agent_occupancy))

# Calculate Seravice level:
#   t = target answer time
#   W(t) = Prob(waiting time <= t): = 1 * Ec(m, u) * e^(-(m-u) * t/Ts)
target_answer_time <- 45
exponent <- -(num_agents - traffic_intensity) * (target_answer_time / duration)
service_level <- 1 - erlangC * exp(exponent)
#########################################################################################################




#########################################################################################################
# Linear Regression
#     I don't believe this offers any valuable insight, and that there are data points I do not have
#     access to in order to have this be valuable. Or perhaps, linear regression isn't the way to go.
#########################################################################################################
calls1 <- lm(CallsOffered ~ ApplicationID + lubridate::wday(statTimestamp) + lubridate::hour(statTimestamp) + lubridate::mday(statTimestamp), apps_primary)
summary(calls1)
