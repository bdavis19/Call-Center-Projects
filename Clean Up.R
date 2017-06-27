# Environment
library(tidyverse)

# Load Data
# PREFIX: m - master list
mapps <- read.csv("applications_sanitized.csv", TRUE, ",")
mskills <- read.csv("skillsets_sanitized.csv", TRUE, ",")
mcdns <- read.csv("cdns_sanitized.csv", TRUE, ",")
apps <- read.csv("aggint_application.csv", TRUE, ",")
skills <- read.csv("aggint_skillset.csv", TRUE, ",")

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

# We need to separate the timestamp fields so that they are in a date and time field
apps <- apps %>% separate(statTimestamp, c("Date", "Timestamp"), sep = "T")
skills <- skills %>% separate(statTimestamp, c("Date", "Timestamp"), sep = "T")

# Create new files for those that needed clean up.
write.table(apps, "aggint_application_refined.csv", row.names = FALSE)
write.table(skills, "agg_int_skillset_refined.csv", row.names = FALSE)
