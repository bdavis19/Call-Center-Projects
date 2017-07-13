# Environment
library(tidyverse)
library(lubridate)
library(data.table)

# Call Volume
# Focus on the major application entry points: 10025, 10021, 10139, 10120, 10037, 10115
primaryApps <- apps %>% filter(ApplicationID == 10025 | ApplicationID == 10021 | ApplicationID == 10139 | ApplicationID == 10120 | ApplicationID == 10037 | ApplicationID == 10115)
ggplot(primaryApps, aes(x = month(statTimestamp), y = CallsOffered, fill = Name)) + 
  geom_bar(stat="identity")

primaryApps %>% filter(month(statTimestamp) == 1) %>% select(CallsOffered) %>% tally()