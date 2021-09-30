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
library("ergm")
library("fs")
library("ggplot2")
library("ggraph")
library("igraph")
library("magrittr")
library("network")
library("SemNetCleaner")
library("tidyverse")
library("tidygraph")

# Functions ---------------------------------------------------------------

##############################################
# Function to extract node traits in a graph #
##############################################

# For a discussion:
# https://medium.com/@615162020004/social-network-analysis-with-r-centrality-measure-86d7fa273574

# Graph should be a tidygraph dataset

NodeTraitGet <- function(Graph, mode = "in", dir = TRUE){
  
  list(
    ID          = Graph %>% activate(nodes) %>% as.data.frame %>% pull(1),
    Degree      = degree(Graph, mode = mode),
    Strength    = strength(Graph, mode = mode),
    Eigenvector = Graph %>% eigen_centrality(directed = dir) %>% extract2("vector"),
    Betweenness = Graph %>% betweenness(directed = dir),
    Closeness   = Graph %>% closeness(mode = mode)
  )
}

## ------------------------------------------------------------------------
# 'Keywords analysis'
## ------------------------------------------------------------------------

# Loading the databases ---------------------------------------------------

db_graph <- read.csv("Database/text_compiled_absolute.csv") %>% column_to_rownames("WOS_ID")
ref_tab <- read.csv("Database/Table_papers.csv")

db_graph_rel <- read.csv("Database/text_compiled_relative.csv") %>% column_to_rownames("WOS_ID")
db_graph <- db_graph_rel

# Generate a bi-partite network -------------------------------------------

# removing controls
#db_graph <- db_graph[which(ref_tab$SEARCH_TYPE != "Control"),] 

# transpose the matrix
db_graph <- t(db_graph)

rownames(db_graph)
colnames(db_graph)

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
Graph_tbl_uni <- Graph_unipartite %>% as_tbl_graph(directed = TRUE)

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

(net1 <- Graph_tbl_uni %>% ggraph::ggraph(Layout1) +
  #geom_edge_density(fill="blue", alpha=0.8) +
  geom_edge_fan(aes(width=weight),color="gray80", alpha = .8) +
  scale_edge_width_continuous(range=c(0,1))+
  geom_node_point(col="grey10", fill = "orange", alpha = .8, 
                  aes(size=N),
                   shape = 21) + 
  scale_fill_manual(values = c("blue", "orange", "turquoise","purple", "grey15"))+
  geom_node_text(aes(label = name), size=3, color="gray10", repel=TRUE) +
  theme_void() + theme(legend.position = "bottom",legend.direction = "vertical")+ coord_fixed())# add edges to the plot geom_node_point()

ggsave("Figures/Network_1.pdf", plot = net1)

# What is the centrality of each word?
node_trait <- Graph_tbl_uni %>% NodeTraitGet(mode = "in", dir = FALSE) %>% bind_cols()
node_trait

# Unipartite network for papers --------------------------------------------

Graph_tbl_uni2 <- Graph_unipartite_full$proj2 %>% as_tbl_graph(directed = TRUE)

# Adding attributes to the graph
Node_attributes2 <- ref_tab %>% dplyr::select(WOS_ID,SEARCH_TYPE)

Graph_tbl_uni2 <- Graph_tbl_uni2 %>% tidygraph::activate(nodes) %>% left_join(Node_attributes2, by = c("name" = "WOS_ID"))

Graph_tbl_uni2 %>% tidygraph::activate(nodes) %>% data.frame %>% select(SEARCH_TYPE) %>%  table

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

net2 <- Graph_tbl_uni2 %>% ggraph::ggraph(Layout2) +
  #geom_edge_density(fill="orange") +
  geom_edge_fan(aes(width=weight),color="gray90", alpha = 0.9) +
  scale_edge_width_continuous(range=c(0,1)) +
  geom_node_point(col="grey10", alpha = .8, aes(fill = SEARCH_TYPE), shape = 21) + 
  scale_fill_manual(values = c("blue", "orange", "turquoise"))+
  #geom_node_text(aes(label = name), size=3, color="gray10", repel=TRUE) +
  theme_void() + 
  theme(legend.position = "bottom",legend.direction = "vertical") + 
  coord_fixed()# add edges to the plot geom_node_point()

ggsave("Figures/Network_2.pdf", plot = net2)

# fitting a model ---------------------------------------------------------

AdjMatrix <- Graph_tbl_uni2 %>% get.adjacency(sparse = FALSE) 
 
AdjMatrix %>% as.matrix %>% dim #square

ResponseNetwork <- as.network(AdjMatrix %>% as.matrix, 
                              directed = TRUE, 
                              matrix.type = "a", 
                              ignore.eval = FALSE, 
                              names.eval = "weight")  # Important! )

as.matrix(ResponseNetwork, attrname = "weight")

# Adding node-level attributes
Graph_tbl_uni2 %>% as.data.frame %>% colnames

ResponseNetwork %v% "SEARCH_TYPE" <- Graph_tbl_uni2 %>% as.data.frame %>% pull(SEARCH_TYPE)

# Fitting the model
ERGM1 <- ergm::ergm(ResponseNetwork ~ edges + nodematch("SEARCH_TYPE"), estimate = "MLE")

summary(ERGM1)

EstimateDF <- 
  ERGM1 %>% summary %>% extract2("coefficients") %>% 
  as.data.frame %>% 
  rownames_to_column("Variable")

ERGM1 %>% 
  confint %>% # Grabbing the 95% confidence intervals
  as.data.frame %>% 
  rename(Lower = 1, Upper = 2)

EstimateDF %<>% # Bind them together
  bind_cols(ERGM1 %>% confint %>% as.data.frame %>% 
              rename(Lower = 1, Upper = 2))

EstimateDF %>% head

# EstimateDF %>% ggplot(aes(Variable, Estimate)) +
#   geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.3) +
#   geom_point()

EstimateDF %>% ggplot2::ggplot(aes(Variable, Estimate)) +
  geom_hline(lty = 2, yintercept = 0) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_point() + theme_bw() +
  coord_flip()

## With abundance
AdjMatrix <- Graph_tbl_uni2 %>% get.adjacency(attr = "weight", sparse = FALSE) 
AdjMatrix %>% as.matrix %>% dim #square

ResponseNetwork <- as.network(AdjMatrix %>% as.matrix, 
                              directed = TRUE, 
                              matrix.type = "a", 
                              ignore.eval = FALSE, 
                              names.eval = "weight")  # Important! )

as.matrix(ResponseNetwork, attrname = "weight")

#Adding node-level attributes
Graph_tbl_uni2 %>% as.data.frame %>% colnames

ResponseNetwork %v% "SEARCH_TYPE" <- Graph_tbl_uni2 %>% as.data.frame %>% pull(SEARCH_TYPE)

NMCMC <- 1000

test <- ergm(ResponseNetwork ~ sum + #nonzero +  
               nodematch("SEARCH_TYPE") , control = control.ergm(
                 parallel =10, parallel.type="PSOCK",
                 MCMC.samplesize = NMCMC,
                 MCMLE.maxit = 50),
             response = "weight", reference = ~ Poisson)

mcmc.diagnostics(test)
summary(test)

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

# Singularize keywords

DE <- MATRIX_1$ID

for(i in 1: length(DE)) {
  
  DE_i <- strsplit(DE[i], ";")[[1]]
  
  if(is.na(DE_i) == TRUE){
    DE[i] <- DE_i
  } else {
    
    Names <- c()
    for(k in 1 : length(DE_i))
      Names = append(Names,SemNetCleaner::singularize(tolower(DE_i[k])))
    
    DE[i] <- paste(Names, collapse = " ;")  
  }
}
  
MATRIX_1$ID <- DE

# Replacing ecosystems manually





###

NetMatrix <-
  biblioNetwork(MATRIX_1,
                analysis = "co-occurrences",
                network = "keywords",
                sep = ";")

sort(table(colnames(NetMatrix)))
rownames(NetMatrix)

# # Singularize keywords
# 
# Names <- c()
# 
# for(m in 1:length(rownames(NetMatrix)))
#   Names = append(Names,SemNetCleaner::singularize(tolower(rownames(NetMatrix))[m]))
# 
# sort(table(Names))
# 
# NetMatrix@Dimnames[[1]] = Names
# NetMatrix@Dimnames[[2]] = Names
# 
# NetMatrix@i
# 
# ?cocMatrix
# 
# ?biblioNetwork
# 
# rownames(NetMatrix) = colnames(NetMatrix) = Names
# 
# NetMatrix2 <- data.frame(Names = as.factor(Names), as.matrix(NetMatrix))
# NetMatrix2 <- NetMatrix2 %>% dplyr::group_by(Names) %>% dplyr::summarise_all(sum) 
# 
# NetMatrix3 <- NetMatrix2 %>% dplyr::select(-ends_with(".1")) 
# dim(NetMatrix2)
# 
# NetMatrix2 <- matrix(NetMatrix2)

net3 <- networkPlot(
  NetMatrix,
  normalize = "association",
  weighted = TRUE,
  cluster = "louvain",
  remove.multiple = TRUE,
  n = 20,
  Title = "Keyword Co-occurrences",
  type = "fruchterman",
  size = TRUE,
  size.cex = FALSE,
  edgesize = 3,
  labelsize = 0.7
)

pdf("Figures/Network_3.pdf", width = 10.3, height = 6.5)
networkPlot(
  NetMatrix,
  normalize = "association",
  weighted = TRUE,
  n = 20,
  Title = "Keyword Co-occurrences",
  type = "fruchterman",
  size = TRUE,
  size.cex = FALSE,
  edgesize = 5,
  labelsize = 0.7)
dev.off()


netstat1 <- networkStat(NetMatrix)
summary(netstat1, k = 10)

# Co-citation -------------------------------------------------------------
MATRIX_1$CR


NetMatrix2 <-
  biblioNetwork(MATRIX_1,
                analysis = "co-citation",
                network = "references",
                sep = ";")

NetMatrix2@Dimnames

# netstat2 <- networkStat(NetMatrix2)
# summary(netstat2, k=10)

pdf("Figures/Network_4.pdf", width = 10.3, height = 6.5)
networkPlot(
  NetMatrix2,
  n = 15,
  Title = "Co-Citation Network",
  type = "fruchterman",
  size = T,
  remove.multiple = FALSE,
  labelsize = 0.7,
  edgesize = 5)
dev.off()

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
