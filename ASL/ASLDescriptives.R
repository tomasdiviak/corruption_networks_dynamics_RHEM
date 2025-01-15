######## ASL network descriptives - relational events & network ########

setwd("C:/Users/tomas/OneDrive - The University of 
      Manchester/práce/postdoc/corruption_DPAs/rel event paper")

#### network descriptives
# loading data
library(openxlsx)

ASL <- read.xlsx("ASL_relEvents.xlsx", sheet = 1, colNames = TRUE)

# creating an edgelist with weights
library(tidyverse)

ASLel <- ASL %>% 
  select(source, target) %>% 
  count(source, target) %>% 
  rename(weight = n)

# importing to igraph
library(igraph)

ASLg <- graph_from_data_frame(ASLel, directed = TRUE, vertices = NULL)

# producing adjacency matrices
ASLmB <- as_adjacency_matrix(ASLg, sparse = FALSE) # binary
ASLmW <- as_adjacency_matrix(ASLg, sparse = FALSE, attr = "weight") # weighted

write.csv(ASLmW, "ASL_matW.csv")
write.xlsx(as.data.frame(ASLmW), "ASL_matW.xlsx")

# average degree & SDs - weighted network (consistent with igraph)
mean(rowSums(ASLmW))
sd(rowSums(ASLmW))
sd(colSums(ASLmW))

# average degree & SDs - binary network
mean(rowSums(ASLmB))
sd(rowSums(ASLmB))
sd(colSums(ASLmB))

# network visualisation with ggraph
library(ggraph)
library(tidygraph)

ASLAtt <- read.xlsx("ASL_relEvents.xlsx", sheet = 2, colNames = TRUE)

ASLgA <- graph_from_data_frame(ASLel, directed = TRUE, vertices = ASLAtt)

ASLgATbl <- as_tbl_graph(ASLgA)

ASLgATbl %>%
  mutate(centrality = centrality_degree()) %>% 
  ggraph(layout = 'circle') + 
  geom_edge_link(alpha = 0.25, aes(width = weight)) + 
  scale_edge_width(range = c(1, 7)) + 
  geom_node_point(aes(size = centrality, colour = ASL)) + 
  #geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position = "none") 
# + labs(title = 'Airbus network')

# analysis with xUCINET (core-periphery)
library(xUCINET)

xASL <- xCreateProject(NetworkName = "ASLweight", NETFILE1 = ASLmW, 
                       FileType = "Robject", Mode = c("actors"), 
                       References = NULL)

xCorePeriphery(xASL$ASLweight)

#### descriptives over time
# converting character dates to date format
library(lubridate)

# removing the first five rows that do NOT contain temporal information
ASLr <- ASL[-(1:5),]

# conversion to dates and renaming
ASLdates <- as.data.frame(dmy(ASLr$day2))

ASLdates <- ASLdates %>% 
  rename(dates = 'dmy(ASLr$day2)')

# visualising events over time
library(ggplot2)

# creating a timeline with weeks and filling empty weeks
ASLweek <- ASLdates %>% 
  count(week = floor_date(dates, "week")) %>% 
  # add missing weeks
  complete(week = seq.Date(min(week), max(week), by = "week")) %>% 
  replace_na(list(n = 0))

# visualising it with a lineplot
ggplot(aes(week, n), data = ASLweek) +
  geom_line() + 
  theme_bw() + 
  labs(x = "Weeks", y = "Events weighted by the number of interactions")

# descriptive statistics
mean(ASLweek$n)
sd(ASLweek$n)

# creating a timeline with months and filling empty months
ASLmnth <- ASLdates %>% 
  count(month = floor_date(dates, "month")) %>% 
  complete(month = seq.Date(min(month), max(month), by = "month")) %>% 
  replace_na(list(n = 0)) 

# visualising it with a lineplot
ggplot(aes(month, n), data = ASLmnth) +
  geom_line() + 
  theme_bw() + 
  labs(x = "Months", y = "Events weighted by the number of interactions")

# descriptive statistics
mean(ASLmnth$n)
sd(ASLmnth$n)

# time series plot or control
ts.plot(ASLmnth$n)

# change point analysis
library(changepoint)

# binary segmentation
m.binseg <- cpt.mean(ASLmnth$n, method = "BinSeg", Q = 7)
plot(m.binseg, type = "l", xlab = "Index", cpt.width = 4)
cpts(m.binseg) # 3

# PELT method
m.pelt <- cpt.mean(ASLmnth$n, method = "PELT")
plot(m.pelt, type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)
cpts(m.pelt) # 3
