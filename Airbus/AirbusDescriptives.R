######## Airbus network descriptives - relational events & network ########

setwd("C:/Users/tomas/OneDrive - The University of 
      Manchester/práce/postdoc/corruption_DPAs/rel event paper")

#### network descriptives
# loading data
library(openxlsx)

AB <- read.xlsx("Airbus_relEvents.xlsx", sheet = 1, colNames = TRUE)

# creating an edgelist with weights
library(tidyverse)

ABel <- AB %>% 
  select(source, target) %>% 
  count(source, target) %>% 
  rename(weight = n)

# importing to igraph
library(igraph)

ABg <- graph_from_data_frame(ABel, directed = TRUE, vertices = NULL)
gorder(ABg)
ABnodes <- V(ABg) # not necessary

# importing attributes
att <- read.xlsx("Airbus_relEvents.xlsx", sheet = 2, colNames = TRUE)
allNodes <- att$actor

allN <- unique(c(ABel$source, ABel$target))

# finding isolated actors (not included in the edgelist, but in the attributes)
iso <- setdiff(allNodes, allN)

# reducing the attribute list only to the connected nodes
attConn <- att[!(att$actor %in% iso), ]
dim(attConn) # works, but beware the original indexing is kept

# add the isolates to the graph
ABgAll <- ABg + iso

# producing adjacency matrices
ABmB <- as_adjacency_matrix(ABg, sparse = FALSE) # binary
ABmW <- as_adjacency_matrix(ABg, sparse = FALSE, attr = "weight") # weighted

ABmBAll <- as_adjacency_matrix(ABgAll, sparse = FALSE) # binary
ABmWAll <- as_adjacency_matrix(ABgAll, sparse = FALSE, attr = "weight") # weighted

write.csv(ABmW, "Airbus_matW.csv")
write.xlsx(as.data.frame(ABmW), "Airbus_matW.xlsx")

write.csv(ABmWAll, "Airbus_matWAll.csv")
write.xlsx(as.data.frame(ABmWAll), "Airbus_matWAll.xlsx")

# average degree & SDs - weighted network (consistent with igraph)
mean(rowSums(ABmW))
sd(rowSums(ABmW))
sd(colSums(ABmW))

mean(rowSums(ABmWAll)) # sensitivity analysis w/ added isolates
sd(rowSums(ABmWAll))
sd(colSums(ABmWAll))

# average degree & SDs - binary network
mean(rowSums(ABmB))
sd(rowSums(ABmB))
sd(colSums(ABmB))

mean(rowSums(ABmBAll)) # sensitivity analysis w/ added isolates
sd(rowSums(ABmBAll))
sd(colSums(ABmBAll))

# network visualisation with ggraph
library(ggraph)
library(tidygraph)

ABgA <- graph_from_data_frame(ABel, directed = TRUE, vertices = attConn)

ABgATbl <- as_tbl_graph(ABgA)

ABgATbl %>%
  mutate(centrality = centrality_degree()) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link(alpha = 0.25, aes(width = weight)) + 
  scale_edge_width(range = c(1, 7)) + 
  geom_node_point(aes(size = centrality, colour = organisation)) + 
# geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position = "none") 
# + labs(title = 'Airbus network')

# analysis with xUCINET (core-periphery) - unreliable; export matrix and use UCINET
library(xUCINET)

xAB <- xCreateProject(NetworkName = "ABweight", NETFILE1 = ABmW, 
                       FileType = "Robject", Mode = c("actors"), 
                       References = NULL)

xCorePeriphery(xAB$ABweight)

#### descriptives over time
# converting character dates to date format
library(lubridate)
library(readxl) # much better for reading in dates than openxlsx

ABevents <- read_xlsx("Airbus_relEvents.xlsx")

# conversion to dates and renaming
ABdates <- as.data.frame(ymd(ABevents$day))

ABdates <- ABdates %>% 
  rename(dates = 'ymd(ABevents$day)')

# visualising events over time
library(ggplot2)

# creating a timeline with weeks and filling empty weeks
ABweek <- ABdates %>% 
  count(week = floor_date(dates, "week")) %>% 
  # add missing weeks
  complete(week = seq.Date(min(week), max(week), by = "week")) %>% 
  replace_na(list(n = 0))

# visualising it with a lineplot
ggplot(aes(week, n), data = ABweek) +
  geom_line() + 
  theme_bw() + 
  labs(x = "Weeks", y = "Events weighted by the number of interactions")

# descriptive statistics
mean(ABweek$n)
sd(ABweek$n)

# creating a timeline with months and filling empty months
ABmnth <- ABdates %>% 
  count(month = floor_date(dates, "month")) %>% 
  complete(month = seq.Date(min(month), max(month), by = "month")) %>% 
  replace_na(list(n = 0)) 

# visualising it with a lineplot
ggplot(aes(month, n), data = ABmnth) +
  geom_line() + 
  theme_bw() + 
  labs(x = "Months", y = "Events weighted by the number of interactions")

# descriptive statistics
mean(ABmnth$n)
sd(ABmnth$n)

# time series plot or control
ts.plot(ABmnth$n)

# change point analysis
library(changepoint)

# binary segmentation
m.binseg <- cpt.mean(ABmnth$n, method = "BinSeg", Q = 7)
plot(m.binseg, type = "l", xlab = "Index", cpt.width = 4)
cpts(m.binseg) # 5

# PELT method
m.pelt <- cpt.mean(ABmnth$n, method = "PELT")
plot(m.pelt, type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)
cpts(m.pelt) # 4
