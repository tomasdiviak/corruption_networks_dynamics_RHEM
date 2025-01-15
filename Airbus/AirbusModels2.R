library(survival)

#set the working directory
setwd("C:/Users/tomas/OneDrive - The University of Manchester/práce/postdoc/
      corruption_DPAs/rel event paper")

# read the output table of eventnet (observation that also explains the sender)
events.random.sender <- read.table("AirbusRHEMAll_EMAILS_RANDOM_SENDER.txt", 
                                   header = TRUE, sep = "\t")

head(events.random.sender)

## distribution of event sizes (number of receivers)
summary(events.random.sender$num.receivers[events.random.sender$IS_OBSERVED == 1])

sum(events.random.sender$IS_OBSERVED == 1)

## distributions of statistics over all observations
summary(events.random.sender[,c(13:ncol(events.random.sender))])
## distributions of statistics over the events
summary(events.random.sender[events.random.sender$IS_OBSERVED == 1,c(13:ncol(events.random.sender))])
## distributions of statistics over the non-events
summary(events.random.sender[events.random.sender$IS_OBSERVED == 0,c(13:ncol(events.random.sender))])

# scale and standardize statistics of network effects, but not those dependent on actor-level attributes
# (network effects are consecutive at the end of the list of variables, starting with "repetition")
first.network.var <- which(names(events.random.sender) == "repetition")
## alternative: use square-root (see the JRSSA paper)
events.random.sender[,c(first.network.var:ncol(events.random.sender))] <- 
  sqrt(events.random.sender[,c(first.network.var:ncol(events.random.sender))])

#events.random.sender[,c(first.network.var:ncol(events.random.sender))] <- 
#  log1p(events.random.sender[,c(first.network.var:ncol(events.random.sender))])
#events.random.sender[,c(first.network.var:ncol(events.random.sender))] <- 
#  scale(events.random.sender[,c(first.network.var:ncol(events.random.sender))])

RHEM_Airbus1 <- coxph(Surv(time = rep(1,nrow(events.random.sender)), 
                            event = events.random.sender$IS_OBSERVED) ~ 
                         receiver.avg.AB #+ sender.AB
                       + sender.receiver.abs.diff.AB # + receiver.pair.abs.diff.AB = 0
                       + strata(EVENT_INTERVAL) # tells the coxph() function which events and non-events belong together
                       , data = events.random.sender
                      , control = coxph.control(iter.max = 40) # increase number of iterations
)
summary(RHEM_Airbus1)

RHEM_Airbus2 <- coxph(Surv(time = rep(1,nrow(events.random.sender)), 
                            event = events.random.sender$IS_OBSERVED) ~ 
                         receiver.avg.AB #+ sender.AB
                       + sender.receiver.abs.diff.AB # + receiver.pair.abs.diff.AB
                       + repetition + undirected.repetition
                       + r.sub.rep.1 # + r.sub.rep.2 
                       + s.r.sub.rep.1 #+ s.r.sub.rep.2 
                       + receiver.outdeg + sender.indeg + reciprocation 
                       + strata(EVENT_INTERVAL) # tells the coxph() function which events and non-events belong together
                       , data = events.random.sender
                      , control = coxph.control(iter.max = 40) # increase number of iterations
)
summary(RHEM_Airbus2) # issues w/ s.r.subrep.1 & reciprocation --> Seem to work now

RHEM_Airbus2.1 <- coxph(Surv(time = rep(1,nrow(events.random.sender)), 
                           event = events.random.sender$IS_OBSERVED) ~ 
                        receiver.avg.AB #+ sender.AB
                      + sender.receiver.abs.diff.AB # + receiver.pair.abs.diff.AB
                      + repetition + undirected.repetition
                      + r.sub.rep.1 # + r.sub.rep.2 
                      #+ s.r.sub.rep.1 #+ s.r.sub.rep.2 
                      + receiver.outdeg + sender.indeg #+ reciprocation 
                      + strata(EVENT_INTERVAL) # tells the coxph() function which events and non-events belong together
                      , data = events.random.sender
                      , control = coxph.control(iter.max = 40) # increase number of iterations
)
summary(RHEM_Airbus2.1)

RHEM_Airbus3 <- coxph(Surv(time = rep(1,nrow(events.random.sender)), 
                            event = events.random.sender$IS_OBSERVED) ~ 
                         receiver.avg.AB #+ sender.AB
                       + sender.receiver.abs.diff.AB #+ receiver.pair.abs.diff.AB
                       + repetition + undirected.repetition
                       + r.sub.rep.1 #+ r.sub.rep.2 
                       + s.r.sub.rep.1 
                      #+ s.r.sub.rep.2 
                       + receiver.outdeg + sender.indeg 
                       + reciprocation # receiver outdegree is generalized reciprocation
                       #+ past.interaction.among.current.receivers # past event from A to B; now C sends an event to {A,B}
                       #+ current.interaction.among.past.receivers # past event from C to {A,B}; now A sends an event to B
                       + transitive.closure + cyclic.closure
                       #+ shared.receiver + shared.sender # not significant
                       + strata(EVENT_INTERVAL) # tells the coxph() function which events and non-events belong together
                       , data = events.random.sender
                       , control = coxph.control(iter.max = 40) # increase number of iterations
)
summary(RHEM_Airbus3)
