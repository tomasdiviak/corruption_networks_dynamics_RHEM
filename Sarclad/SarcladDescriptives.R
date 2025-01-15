######## Sarclad network descriptives - relational events & network ########

setwd("C:/Users/tomas/OneDrive - The University of 
      Manchester/práce/postdoc/corruption_DPAs/rel event paper")

#### network descriptives
# loading data
library(readxl)
library(tidyverse)

Sarclad <- read_excel("Sarclad_relEvents.xlsx", sheet = 1, 
                      col_types = c("text", "text", "text", "text", "date", "date", "text"),
                      col_names = TRUE) %>% 
  mutate(day = lubridate::ymd(day))

# creating an edgelist with weights
Sarcladel <- Sarclad %>% 
  select(source, target) %>% 
  count(source, target) %>% 
  rename(weight = n)

# importing to igraph
library(igraph)

Sarcladg <- graph_from_data_frame(Sarcladel, directed = TRUE, vertices = NULL)

# producing adjacency matrices
SarcladmB <- as_adjacency_matrix(Sarcladg, sparse = FALSE) # binary
SarcladmW <- as_adjacency_matrix(Sarcladg, sparse = FALSE, attr = "weight") # weighted

write.csv(SarcladmW, "Sarclad_matW.csv")
write.xlsx(as.data.frame(SarcladmW), "Sarclad_matW.xlsx")

# average degree & SDs - weighted network (consistent with igraph)
mean(rowSums(SarcladmW))
sd(rowSums(SarcladmW))
sd(colSums(SarcladmW))

# average degree & SDs - binary network
mean(rowSums(SarcladmB))
sd(rowSums(SarcladmB))
sd(colSums(SarcladmB))

# network visualisation with ggraph
library(ggraph)
library(tidygraph)

SarcAtt <- read.xlsx("Sarclad_relEvents.xlsx", sheet = 2, colNames = TRUE)

SarcladgA <- graph_from_data_frame(Sarcladel, directed = TRUE, vertices = SarcAtt)

SarcladgATbl <- as_tbl_graph(SarcladgA)

SarcladgATbl %>%
  mutate(centrality = centrality_degree()) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link(alpha = 0.25, aes(width = weight)) + 
  scale_edge_width(range = c(1, 7)) + 
  geom_node_point(aes(size = centrality, colour = Sarclad)) + 
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position = "none") 
# + labs(title = 'Sarclad network')

# analysis with xUCINET (core-periphery)
library(xUCINET)

xSarclad <- xCreateProject(NetworkName = "Sarcladweight", NETFILE1 = SarcladmW, 
                       FileType = "Robject", Mode = c("actors"), 
                       References = NULL)

xCorePeriphery(xSarclad$Sarcladweight)

#### descriptives over time
# converting character dates to date format
library(lubridate)

# conversion to dates and renaming
Sarcladdates <- select(Sarclad, day)

# visualising events over time
library(ggplot2)

# creating a timeline with weeks and filling empty weeks
Sarcladweek <- Sarcladdates %>% 
  count(week = floor_date(day, "week")) %>% 
  # add missing weeks
  complete(week = seq.Date(min(week), max(week), by = "week")) %>% 
  replace_na(list(n = 0))

# visualising it with a lineplot
ggplot(aes(week, n), data = Sarcladweek) +
  geom_line() + 
  theme_bw() + 
  labs(x = "Weeks", y = "Events weighted by the number of interactions")

# descriptive statistics
mean(Sarcladweek$n)
sd(Sarcladweek$n)

# creating a timeline with months and filling empty months
Sarcladmnth <- Sarcladdates %>% 
  count(month = floor_date(day, "month")) %>% 
  complete(month = seq.Date(min(month), max(month), by = "month")) %>% 
  replace_na(list(n = 0)) 

# visualising it with a lineplot
ggplot(aes(month, n), data = Sarcladmnth) +
  geom_line() + 
  theme_bw() + 
  labs(x = "Months", y = "Events weighted by the number of interactions")

# descriptive statistics
mean(Sarcladmnth$n)
sd(Sarcladmnth$n)

# time series plot or control
ts.plot(Sarcladmnth$n)

# change point analysis
library(changepoint)

# binary segmentation
m.binseg <- cpt.mean(Sarcladmnth$n, method = "BinSeg", Q = 20)
plot(m.binseg, type = "l", xlab = "Index", cpt.width = 4)
cpts(m.binseg) # 15

# PELT method
m.pelt <- cpt.mean(Sarcladmnth$n, method = "PELT")
plot(m.pelt, type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)
cpts(m.pelt) # 24
