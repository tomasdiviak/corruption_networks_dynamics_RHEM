library(survival)

#set the working directory (set back to your directory)
#setwd("C:/Users/tomas/OneDrive - The University of Manchester/práce/postdoc/
#      corruption_DPAs/rel event paper")

#setwd("c:/juergen/projects/elyzer/hyper.events/tomas_diviak/data_for_submission/")

# read the output table of eventnet (observation that also explains the sender)
events.random.sender <- read.table("AirbusRHEMAll_EMAILS_RANDOM_SENDER.txt", 
                                 header = TRUE, sep = "\t")

names(events.random.sender)

## distributions of statistics over all observations
summary(events.random.sender[,c(13:ncol(events.random.sender))])
## distributions of statistics over the events
summary(events.random.sender[events.random.sender$IS_OBSERVED == 1,c(13:ncol(events.random.sender))])
## distributions of statistics over the non-events
summary(events.random.sender[events.random.sender$IS_OBSERVED == 0,c(13:ncol(events.random.sender))])

# (network effects are consecutive at the end of the list of variables, starting with "repetition")
first.network.var <- which(names(events.random.sender) == "repetition")
## scale by square-root (see the JRSSA paper)
events.random.sender[,c(first.network.var:ncol(events.random.sender))] <- 
  sqrt(events.random.sender[,c(first.network.var:ncol(events.random.sender))])

# fit model used in the manuscript
RHEM_Airbus <- coxph(Surv(time = rep(1,nrow(events.random.sender)), 
                            event = events.random.sender$IS_OBSERVED) ~ 
                         receiver.avg.AB 
                       + sender.receiver.abs.diff.AB
                       + repetition + undirected.repetition
                       + r.sub.rep.1
                       + s.r.sub.rep.1 
                       + receiver.outdeg + sender.indeg + reciprocation # receiver outdegree is generalized reciprocation
                       + transitive.closure + cyclic.closure
                       #+ shared.receiver + shared.sender # not significant
                       + strata(EVENT_INTERVAL) # tells the coxph() function which events and non-events belong together
                       , data = events.random.sender
                       , control = coxph.control(iter.max = 40) # increase number of iterations
)
summary(RHEM_Airbus)

## vector of coefficients
coefs <- RHEM_Airbus$coefficients

# number of bootstrap samples
n.sim <- 100

# build matrix with columns labeled by effects and n.sim+1 rows
# first row filled with published coefficients
# remaining rows filled with coefficients fitted to re-sampled events
coef.mat <- matrix(0, nrow = n.sim+1, ncol = length(coefs), 
                   dimnames = list(c(1:(n.sim+1)), names(coefs)))
# fill first row
coef.mat[1,] <- coefs

# resample and fit models to resampled data

for(i in c(2:(n.sim+1))){
  print(paste("iteration:", i))
  sampled.events <- events.random.sender[sample.int(nrow(events.random.sender), replace = TRUE),]
  sampled.model <- coxph(Surv(time = rep(1,nrow(sampled.events)), 
                             event = sampled.events$IS_OBSERVED) ~ 
                           receiver.avg.AB 
                         + sender.receiver.abs.diff.AB
                         + repetition + undirected.repetition
                         + r.sub.rep.1
                         + s.r.sub.rep.1 
                         + receiver.outdeg + sender.indeg + reciprocation # receiver outdegree is generalized reciprocation
                         + transitive.closure + cyclic.closure
                         #+ shared.receiver + shared.sender # not significant
                        + strata(EVENT_INTERVAL) # tells the coxph() function which events and non-events belong together
                        , data = sampled.events
                        , control = coxph.control(iter.max = 40) # increase number of iterations
  )
  # store coefficients in the respective row
  coef.mat[i,] <- sampled.model$coefficients
}

# write matrix to file
write.csv(coef.mat, row.names = F, file = "bootstrap_coefs_airbus.csv")

# once you have that file you can reload it without redoing the sampling
coef.mat <- read.csv("bootstrap_coefs_airbus.csv")

# compare actual coefficients with distribution over resampled ones
coef.mat[1,] # actual coefficients

apply(coef.mat[c(2:n.sim),], 2, summary)

get.95.ci <- function(x) {
  return(quantile(x, probs = c(0.025, 0.975)))
}
get.90.ci <- function(x) {
  return(quantile(x, probs = c(0.05, 0.95)))
}

apply(coef.mat[c(2:n.sim),], 2, get.95.ci)

# it turns out that there are some problems with the significant findings (except undirecte repetition)

# be a bit more relaxed (one-sided test):
apply(coef.mat[c(2:n.sim),], 2, get.90.ci)

# still some problems; namely for those effects:

# repetition (reported to be negative in the manuscript but value at 95% quantile is positive)
# cyclic closure (reported to be negative but value at 95% quantile is positive)

# I don't think it's a big problem --> have to discuss (and admit) it

# display their distribution
hist(coef.mat[c(2:n.sim),"repetition"], breaks = 100)
hist(coef.mat[c(2:n.sim),"cyclic.closure"], breaks = 100)

# in both cases there is a clear tendency - but fails to reject null hypothesis
