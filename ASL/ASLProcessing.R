######## corruption networks - ASL - RHEM data processing ########

library(lubridate)
library(dplyr)
library(stringr)
library(readxl)

setwd("C:/Users/tomas/OneDrive - The University of Manchester/práce/postdoc
      /corruption_DPAs/rel event paper")

# loading the attribute data
ASLAtt <- read_excel("ASL_relEvents.xlsx", sheet = 2, col_names = TRUE) 

# nr of actors 
numASL <- length(ASLAtt$actor)

# create a DF with "ASL" events
dummyASL <- data.frame(ASLAtt$actor, ASLAtt$actor,
                        rep("2011-05-01 00:00:00", numASL),
                        rep("asl", numASL),
                        ASLAtt$ASL)
# add variable names
colnames(dummyASL) <- c("source", "target", "date", "type", "weight")

# reate a DF with "add.actor" events 
dummyAct <- data.frame(ASLAtt$actor, ASLAtt$actor,
                       rep("2004-06-10 00:00:00", numASL),
                       rep("add.actor", numASL),
                       rep(1, numASL))
# add variable names
colnames(dummyAct) <- c("source", "target", "date", "type", "weight")

# bind them together
initEvents <- rbind(dummyAct, dummyASL)

# loading the rel events data
ASLEv <- read_excel("ASL_relEvents.xlsx", sheet = 1, col_names = TRUE) 

# merging day and time into date and selecting only relevant columns
ASLEv <- ASLEv %>% 
  mutate(day = ymd(day),
         time2 = str_extract(time, pattern = "\\d+:\\d+:\\d+"),
         date = ymd_hms(paste(day, time2))) %>% 
  select(source, target, date, type)

# converting the date to character to preserve the format
ASLEv$date <- as.character(ASLEv$date)

# giving the same weight to all events
ASLEv <- ASLEv %>% 
  mutate(weight = 1)

# merge initial events and relational events
ASL <- rbind(initEvents, ASLEv)

# generating event IDs based on the same time
ASL <- ASL %>%
  mutate(date_index = group_indices(., date) - 1) %>% 
  arrange(date_index)

# export to .csv
write.csv(ASL, "ASLRHEM.csv")
