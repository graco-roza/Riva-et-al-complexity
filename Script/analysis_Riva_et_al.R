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
library("ggplot2")
library("igraph")
library("network")
library("tidyverse")
library("tidygraph")

# Setting working directory -----------------------------------------------

setwd("/Users/stefanomammola/Desktop/PAPERS IN CORSO/Riva et al Review complexity/FINAL SEARCH")

## ------------------------------------------------------------------------
# 'Keywords analysis'
## ------------------------------------------------------------------------

# Loading the databases ---------------------------------------------------



# Generate a bi-partite network -------------------------------------------

Graph_bipartite <- igraph::graph_from_incidence_matrix(db_graph, directed = TRUE)

V(Graph_bipartite)$type
print(Graph_bipartite, e=TRUE, v=TRUE)
vcount(Graph_bipartite) ; ecount(Graph_bipartite)

# Get attribute table
Graph_tbl_bip <- tidygraph::as_tbl_graph(Graph_bipartite, directed = TRUE)  

# Collapse it into an unipartite 
Graph_unipartite_full <- igraph::bipartite_projection(Graph_bipartite)

Graph_unipartite <- Graph_unipartite_full$proj1

# Get the adjacency matrix
Graph_adj_matrix <- Graph_unipartite %>% get.adjacency(attr = "weight", sparse = FALSE)

# Get attribute table
Graph_tbl_uni <- Graph_unipartite %>% as_tbl_graph(directed = TRUE) %>% 
  activate(edges) %>% #%>% mutate(weight = 1) 
  igraph::simplify(edge.attr.comb = "sum") %>% 
  as_tbl_graph

## ------------------------------------------------------------------------
# 'Scientometric analysis'
## ------------------------------------------------------------------------

# Loading the databases ---------------------------------------------------

MATRIX_1 <-
  bibliometrix::convert2df("Search_1.txt", dbsource = "wos", format = "plaintext")
MATRIX_2 <-
  bibliometrix::convert2df("Search_2.txt", dbsource = "wos", format = "plaintext")

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
