######## corruption networks - Airbus - RHEM data processing ########

library(lubridate)
library(dplyr)
library(stringr)
library(readxl)

setwd("C:/Users/tomas/OneDrive - The University of Manchester/práce/postdoc
      /corruption_DPAs/rel event paper")

# loading the attribute data (all = 2; nonIso = 3)
ABAtt <- read_excel("Airbus_relEvents.xlsx", sheet = 2, col_names = TRUE) 

# nr of actors 
numAB <- length(ABAtt$actor)

# create a DF with "AB" events
dummyAB <- data.frame(ABAtt$actor, ABAtt$actor,
                       rep("0", numAB), # order only, not precise time
                       rep("AB", numAB),
                       ABAtt$Airbus)
# add variable names
colnames(dummyAB) <- c("source", "target", "order", "type", "weight")

# reate a DF with "add.actor" events 
dummyAct <- data.frame(ABAtt$actor, ABAtt$actor,
                       rep("0", numAB),
                       rep("add.actor", numAB),
                       rep(1, numAB))
# add variable names
colnames(dummyAct) <- c("source", "target", "order", "type", "weight")

# bind them together
initEvents <- rbind(dummyAct, dummyAB)

# loading the rel events data
ABEv <- read_excel("Airbus_relEvents.xlsx", sheet = 1, col_names = TRUE) 

# 
ABEv <- ABEv %>% 
  select(source, target, order, what) %>% 
  rename(type = what)

# giving the same weight to all events
ABEv <- ABEv %>% 
  mutate(weight = 1)

# merge initial events and relational events
AB <- rbind(initEvents, ABEv)

# export to .csv
write.csv(AB, "AirbusRHEMAll.csv")
