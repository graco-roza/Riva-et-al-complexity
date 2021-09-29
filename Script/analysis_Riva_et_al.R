####################################
# Review paper on ecological complexity
####################################

## ------------------------------------------------------------------------
# 'R script to reproduce the analyses'
## ------------------------------------------------------------------------

## Authors: Stefano Mammola & Caio Graco Roza
## Last update: 29 Sep 2021, Helsinki, Finland
## Software: R (v. R 4.1.0) and R studio (v. 1.4.1103)

###############################################################

# clean the workspace -----------------------------------------------------

rm(list = ls())

# Loading useful packages -------------------------------------------------

library("bibliometrix")
library("dplyr")
library("ggplot2")
library("ggraph")
library("igraph")
library("network")
library("tidyverse")
library("tidygraph")

## ------------------------------------------------------------------------
# 'Keywords analysis'
## ------------------------------------------------------------------------

# Loading the databases ---------------------------------------------------

db_graph <- read.csv("Database/text_compiled.csv") %>% column_to_rownames("WOS_ID")
ref_tab <- read.csv("Database/Table_papers.csv")

# Generate a bi-partite network -------------------------------------------

# removing controls
#db_graph <- db_graph[which(ref_tab$SEARCH_TYPE != "Control"),] 

db_graph <- droplevels(db_graph)

# transpose the matrix
db_graph <- t(db_graph)

colnames(db_graph)
rownames(db_graph)

#generate the graph
Graph_bipartite <- igraph::graph_from_incidence_matrix(db_graph, multiple = TRUE, directed = TRUE)

V(Graph_bipartite)$type
print(Graph_bipartite, e=TRUE, v=TRUE)
vcount(Graph_bipartite) ; ecount(Graph_bipartite)

# Get attribute table
Graph_tbl_bip <- tidygraph::as_tbl_graph(Graph_bipartite, directed = TRUE)  

# Unipartite network for words --------------------------------------------

Graph_unipartite_full <- igraph::bipartite_projection(Graph_bipartite)

Graph_unipartite <- Graph_unipartite_full$proj1

# Get the adjacency matrix
Graph_adj_matrix <- Graph_unipartite %>% get.adjacency(attr = "weight", sparse = FALSE)

# Adding attributes to the graph
Graph_tbl_uni <- Graph_unipartite %>% as_tbl_graph(directed = TRUE) %>% 
  activate(edges) %>% #%>% mutate(weight = 1) 
  igraph::simplify(edge.attr.comb = "sum") %>% 
  as_tbl_graph

Node_attributes <- data.frame( ID = rownames(db_graph), 
                                 N = rowSums(ifelse(db_graph > 0, 1, 0)))# number of mentions

Graph_tbl_uni <- Graph_tbl_uni %>% tidygraph::activate(nodes) %>% left_join(Node_attributes, by = c("name" = "ID"))

# Adding clustering 

# ClusterFunctions <- list(cluster_louvain, cluster_walktrap, cluster_fast_greedy, cluster_leading_eigen )
# 
# Clustering <- 
#   ClusterFunctions %>% 
#   map(~.x(Graph_tbl_uni)$membership) %>% 
#   bind_cols %>% 
#   rename(Louvain = 1, Walktrap = 2, FastGreedy = 3, LeadingEigen = 4) %>% 
#   mutate(Name = Graph_tbl_uni %>% V %>% names)
# 
# Graph_tbl_uni<- Graph_tbl_uni %>% 
#   left_join(Clustering, by = c("name" = "Name"))

# Plotting 
Layout1 <- layout_with_kk(Graph_tbl_uni) # Kamada-Kawai

Graph_tbl_uni %>% ggraph::ggraph(Layout1) +
  #geom_edge_density(fill="orange", alpha=0.9) +
  geom_edge_fan(aes(width=weight),color="gray30") +
  scale_edge_width_continuous(range=c(0,1))+
  geom_node_point(col="grey10", fill = "orange", alpha = .8, 
                  aes(size=N),
                   shape = 21) + 
  scale_fill_manual(values = c("blue", "orange", "turquoise","purple", "grey15"))+
  geom_node_text(aes(label = name), size=3, color="gray10", repel=TRUE) +
  theme_void() + theme(legend.position = "bottom",legend.direction = "vertical")+ coord_fixed()# add edges to the plot geom_node_point()

# Unipartite network for papers --------------------------------------------

Graph_unipartite2 <- Graph_unipartite_full$proj2

# Get the adjacency matrix
Graph_adj_matrix2 <- Graph_unipartite2 %>% get.adjacency(attr = "weight", sparse = FALSE)

# Adding attributes to the graph
Graph_tbl_uni2 <- Graph_unipartite2 %>% as_tbl_graph(directed = TRUE)

Node_attributes2 <- ref_tab %>% dplyr::select(SEARCH_TYPE,WOS_ID)

Graph_tbl_uni2 <- Graph_tbl_uni2 %>% tidygraph::activate(nodes) %>% left_join(Node_attributes2, by = c("name" = "WOS_ID"))

# # Adding clustering 
# 
# ClusterFunctions <- list(cluster_louvain, cluster_walktrap, cluster_fast_greedy, cluster_leading_eigen )
# 
# Clustering <- 
#   ClusterFunctions %>% 
#   map(~.x(Graph_tbl_uni)$membership) %>% 
#   bind_cols %>% 
#   rename(Louvain = 1, Walktrap = 2, FastGreedy = 3, LeadingEigen = 4) %>% 
#   mutate(Name = Graph_tbl_uni %>% V %>% names)
# 
# Graph_tbl_uni<- Graph_tbl_uni %>% 
#   left_join(Clustering, by = c("name" = "Name"))

# Plotting 
Layout2 <- layout_with_kk(Graph_tbl_uni2) # Kamada-Kawai

Graph_tbl_uni2 %>% ggraph::ggraph(Layout2) +
  #geom_edge_density(fill="orange", alpha=0.9) +
  geom_edge_fan(aes(width=weight),color="gray70") +
  scale_edge_width_continuous(range=c(0,1))+
  geom_node_point(col="grey10", alpha = .8, 
                  aes(fill = SEARCH_TYPE),
                  shape = 21) + 
  scale_fill_manual(values = c("blue", "orange", "turquoise"))+
  #geom_node_text(aes(label = name), size=3, color="gray10", repel=TRUE) +
  theme_void() + theme(legend.position = "bottom",legend.direction = "vertical")+ coord_fixed()# add edges to the plot geom_node_point()

## ------------------------------------------------------------------------
# 'Scientometric analysis'
## ------------------------------------------------------------------------

# Loading the databases ---------------------------------------------------

MATRIX_1 <- bibliometrix::convert2df("Database/Search_1.txt", dbsource = "wos", format = "plaintext")
MATRIX_2 <- bibliometrix::convert2df("Database/Search_2.txt", dbsource = "wos", format = "plaintext")

# Analysing the data ------------------------------------------------------

results <-
  bibliometrix::biblioAnalysis(MATRIX_1, sep = ";")
summary(object = results, k = 10, pause = FALSE)

S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

# Network of keywords ------------------------------------------------------

NetMatrix <-
  biblioNetwork(MATRIX_1,
                analysis = "co-occurrences",
                network = "keywords",
                sep = ";")

net1 <- networkPlot(
  NetMatrix,
  normalize = "association",
  weighted = TRUE,
  n = 30,
  Title = "Keyword Co-occurrences",
  type = "fruchterman",
  size = TRUE,
  size.cex = FALSE,
  edgesize = 5,
  labelsize = 0.7
)

netstat1 <- networkStat(NetMatrix)
summary(netstat1, k = 10)

# Co-citation -------------------------------------------------------------

NetMatrix2 <-
  biblioNetwork(MATRIX_1,
                analysis = "co-citation",
                network = "references",
                sep = ";")

# netstat2 <- networkStat(NetMatrix2)
# summary(netstat2, k=10)

# Plot the network
net2 <- networkPlot(
  NetMatrix2,
  n = 15,
  Title = "Co-Citation Network",
  type = "fruchterman",
  size = T,
  remove.multiple = FALSE,
  labelsize = 0.7,
  edgesize = 5
)

# Country network ---------------------------------------------------------

NetMatrix3 <-
  metaTagExtraction(MATRIX_1, Field = "AU_CO", sep = ";")
NetMatrix3 <-
  biblioNetwork(NetMatrix3,
                analysis = "collaboration",
                network = "countries",
                sep = ";")

net3 <-
  networkPlot(
    NetMatrix3,
    n = dim(NetMatrix3)[1],
    Title = "Country Collaboration",
    type = "circle",
    size = TRUE,
    remove.multiple = FALSE,
    labelsize = 0.7,
    cluster = "none"
  )
