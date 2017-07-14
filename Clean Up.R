# Environment
library(tidyverse)
library(lubridate)
library(data.table)

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
write.table(apps, "aggint_application_refined.csv", row.names = FALSE)
write.table(skills, "agg_int_skillset_refined.csv", row.names = FALSE)
