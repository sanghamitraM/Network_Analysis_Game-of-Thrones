#Author-Sanghamitra Muhuri
rm(list = ls())

library(readr)     
library(tidyverse) 
library(tidygraph) 
library(ggraph)  
library(seriation)
library(influenceR)
library(igraph)

#loading data
path <- "C:/data/"
files <- list.files(path = path, full.names = TRUE)
files

#reading the character interactions
cooc_all_edges <- read_csv(files[1])

#subsetting the data to the 100 characters with the most interactions across all books and gathering Source and Target data before summing up the weights
main_ch <- cooc_all_edges %>%
  select(-Type) %>%
  gather(x, name, Source:Target) %>%
  group_by(name) %>%
  summarise(sum_weight = sum(weight)) %>%
  ungroup()

# main_ch_l <- main_ch %>%
#   arrange(desc(sum_weight)) %>%
#   top_n(100, sum_weight)
# main_ch_l

cooc_all_f <- cooc_all_edges %>% filter(Source %in% main_ch$name & Target %in% main_ch$name)

#create object
cooc_all_f_graph <- as_tbl_graph(cooc_all_f, directed = FALSE) %>% #converting the edge table into a tbl_graph object structure
  mutate(n_rank_trv = node_rank_traveller(), #node rank
         neighbors = centrality_degree(), #centrality
         group = group_infomap(), #grouping and clustering
         center = node_is_center(), 
         dist_to_center = node_distance_to(node_is_center()), #node pairs
         keyplayer = node_is_keyplayer(k = 10)) %>% #identifying the top 10 key players in the network
  activate(edges) %>% 
  filter(!edge_is_multiple()) %>% #removing multiple edges
  mutate(centrality_e = centrality_edge_betweenness()) #edge betweenness

#converting the active node back to a tibble
cooc_all_f_graph %>%
  activate(nodes) %>% # %N>%
  as_tibble()

cooc_all_f_graph %>%
  activate(edges) %>% # %E>%
  as_tibble()

#calculating local clustering coefficient 
transitivity(cooc_all_f_graph, type = "average")
#calculating average shortest path length
mean(shortest.paths(cooc_all_f_graph))

#community detection
set.seed(345)
comm1 <- spinglass.community(cooc_all_f_graph, weights=E(cooc_all_f_graph)$weight)
plot(comm1,cooc_all_f_graph, vertex.label= NA, vertex.size=2)
sizes(comm1)

print(comm1[1])
print(comm1[2])
print(comm1[3])
print(comm1[4])
print(comm1[5])
print(comm1[6])
print(comm1[7])
print(comm1[8])
print(comm1[9])
print(comm1[10])
print(comm1[11])
print(comm1[12])
print(comm1[13])
print(comm1[14])
print(comm1[15])
print(comm1[16])
print(comm1[17])

#do we need this part? 
#identifying weak ties
edge.betweenness.estimate(cooc_all_f_graph, e=E(cooc_all_f_graph), directed = TRUE,weights = NULL)
set.seed(1234)
#plotting with ggraph
layout <- create_layout(cooc_all_f_graph,layout = "fr")

#Katy's work
#Removing edges based on betweenness
#Top 10 edges with highest betweenness
E(cooc_all_f_graph)[order((edge.betweenness(cooc_all_f_graph)), decreasing = T) [1:10]]

#delete the edge with highest betweenness
cooc_all_f_graph_1 <- delete_edges(cooc_all_f_graph,(order((edge.betweenness(cooc_all_f_graph)), decreasing = T) [1]))
E(cooc_all_f_graph_1)[order((edge.betweenness(cooc_all_f_graph_1)), decreasing = T) [1:10]]

#calculating local clustering coefficient 
transitivity(cooc_all_f_graph_1, type = "average")
#calculating average shortest path length
mean(shortest.paths(cooc_all_f_graph_1))

E(cooc_all_f_graph_1)[order((edge.betweenness(cooc_all_f_graph_1)), decreasing = T) [1:10]]

#delete next highest edge
cooc_all_f_graph_2 <- delete_edges(cooc_all_f_graph_1,(order((edge.betweenness(cooc_all_f_graph_1)), decreasing = T) [1]))
E(cooc_all_f_graph_2)[order((edge.betweenness(cooc_all_f_graph_2)), decreasing = T) [1:10]]

#calculating local clustering coefficient 
transitivity(cooc_all_f_graph_2, type = "average")
#calculating average shortest path length
mean(shortest.paths(cooc_all_f_graph_2))

#delete next highest edge
cooc_all_f_graph_3 <- delete_edges(cooc_all_f_graph_2,(order((edge.betweenness(cooc_all_f_graph_2)), decreasing = T) [1]))
E(cooc_all_f_graph_3)[order((edge.betweenness(cooc_all_f_graph_3)), decreasing = T) [1:10]]

#calculating local clustering coefficient 
transitivity(cooc_all_f_graph_3, type = "average")
#calculating average shortest path length
mean(shortest.paths(cooc_all_f_graph_3))

#the graph is still connected, and the average shortest path has not decreased much
is.connected(cooc_all_f_graph_3)

#even removing the top 10 edges with the highest betweeness does not change the graph much
cooc_all_f_graph_4 <- delete_edges(cooc_all_f_graph,(order((edge.betweenness(cooc_all_f_graph)), decreasing = T) [1:10]))
transitivity(cooc_all_f_graph_4, type = "average")
mean(shortest.paths(cooc_all_f_graph_4))
is.connected(cooc_all_f_graph_4)


#list the top 10 weakest ties
E(cooc_all_f_graph)[order(weight, decreasing = F) [1:10]]



##graph the group (define) of the networks and the centrality

#layout 1
ggraph(layout) + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link(aes(width = weight/2), alpha = 0.2) + 
  geom_node_point(aes(color = factor(group)), size = 2) +
  geom_node_text(aes(label = name), size = 2, repel = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_graph() +
  theme(legend.position="none")
  labs(title = "A Song of Ice and Fire character network",
       subtitle = "Nodes are colored by group")

#layout 2
cols <- RColorBrewer::brewer.pal(3, "Set1")
ggraph(layout) + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(center), size = dist_to_center)) +
  geom_node_text(aes(label = name), size = 0, repel = TRUE) +
  scale_colour_manual(values = c(cols[2], cols[1])) +
  theme_graph() +
  labs(title = "A Song of Ice and Fire character network",
       subtitle = "Nodes are colored by centeredness")

#layout2
ggraph(layout) + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(center), size = dist_to_center)) +
  geom_node_text(aes(label = name), size = 8, repel = TRUE) +
  scale_colour_manual(values = c(cols[2], cols[1])) +
  theme_graph() +
  labs(title = "A Song of Ice and Fire character network",
       subtitle = "Nodes are colored by centeredness")

summary(cooc_all_edges$weight)
#796 nodes and 2823 edges, undirected

set.seed(105)
#Generating random graph
g2 <- erdos.renyi.game(796, 2823, type = "gnm",directed = FALSE)
#Assiging weights to graph
E(g2)$weight <- cooc_all_edges$weight
#plotting graph
plot(g2, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Random Network: G(N,L) model")

# Average local clustering coefficient
mean(transitivity(g2, type = "local"), na.rm = TRUE)
#0.01061919
mean(transitivity(cooc_all_f_graph, type = "local"), na.rm = TRUE)

#Average shortest path length
mean(shortest.paths(cooc_all_f_graph))
mean(shortest.paths(g2))

#Closeness Centrality
table1 <- as_tbl_graph(cooc_all_f_graph, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(neighbors = centrality_degree()) %>%
  arrange(-neighbors)
View(table1)
