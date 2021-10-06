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
library("ergm")
library("fs")
library("ggraph")
library("network")
library("SemNetCleaner")
library("tidyverse")
library("tidygraph")
library("rgeos")
library("rworldmap")

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

ref_tab   <- read.csv("Database/Table_papers.csv")
db_graph2 <- read.csv("Database/text_compiled_relative.csv") %>% column_to_rownames("WOS_ID")

# Generate a bi-partite network -------------------------------------------

# Removing control
db_graph <- db_graph2[which(ref_tab$SEARCH_TYPE == "Ecological complexity"),] 

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

# What is the centrality of each word?
node_trait <- Graph_tbl_uni %>% NodeTraitGet(mode = "in", dir = FALSE) %>% bind_cols()
Node_attributes <- Node_attributes %>% left_join(node_trait, by = c("ID")) ; rm(node_trait)

# Add to the graph
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
  scale_edge_width_continuous("Edge strength",range=c(0,1))+
  geom_node_point(col="grey10", alpha = .8, 
                  aes(size=N, fill=Strength),
                   shape = 21) + 
  scale_fill_gradient("Node strength")+
  geom_node_text(aes(label = name), size=2, color="gray10", repel = TRUE) +
  theme_void() + theme(legend.position = "left",legend.direction = "vertical")+ coord_fixed())# add edges to the plot geom_node_point()

ggsave("Figures/Network_1.pdf", plot = net1)

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

(net2 <- Graph_tbl_uni2 %>% ggraph::ggraph(Layout2) +
  #geom_edge_density(fill="orange") +
  geom_edge_fan(aes(width=weight),color="gray90", alpha = 0.95) +
  scale_edge_width_continuous("Edge strength",range=c(0,1)) +
  geom_node_point(col="grey10", alpha = .8, aes(fill = SEARCH_TYPE), shape = 21) + 
  scale_fill_manual("",values = c("blue", "orange", "turquoise"))+
  #geom_node_text(aes(label = name), size=3, color="gray10", repel=TRUE) +
  theme_void() + 
  theme(legend.position = "bottom", legend.direction = "vertical") + 
  coord_fixed())# add edges to the plot geom_node_point()

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

EstimateDF %>% ggplot2::ggplot(aes(Variable, Estimate)) +
  geom_hline(lty = 2, yintercept = 0, col = "grey30") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) +
  geom_point() + theme_classic() +
  coord_flip()

#  Testing separate networks ----------------------------------------------

db_graph_control <- db_graph2[which(ref_tab$SEARCH_TYPE == "Control"),] %>% na.omit %>% t
db_graph_EC      <- db_graph2[which(ref_tab$SEARCH_TYPE == "Ecological complexity"),] %>% na.omit %>% t
db_graph_CCS     <- db_graph2[which(ref_tab$SEARCH_TYPE == "Complex system science"),] %>% na.omit %>% t

#generate the graph
db_graph_control <- igraph::graph_from_incidence_matrix(db_graph_control, multiple = TRUE, directed = TRUE) 
db_graph_EC      <- igraph::graph_from_incidence_matrix(db_graph_EC, multiple = TRUE, directed = TRUE)
db_graph_CCS     <- igraph::graph_from_incidence_matrix(db_graph_CCS, multiple = TRUE, directed = TRUE)

# Unipartite network 
Graph_unipartite_control <- igraph::bipartite_projection(db_graph_control)$proj1 %>% as_tbl_graph(directed = TRUE)
Graph_unipartite_EC      <- igraph::bipartite_projection(db_graph_EC)$proj1 %>% as_tbl_graph(directed = TRUE)
Graph_unipartite_CCS     <- igraph::bipartite_projection(db_graph_CCS)$proj1 %>% as_tbl_graph(directed = TRUE)

# 
# 
# Node_attributes <- data.frame( ID = rownames(db_graph), 
#                                N = rowSums(ifelse(db_graph > 0, 1, 0)))# number of mentions
# 
# Graph_tbl_uni <- Graph_tbl_uni %>% tidygraph::activate(nodes) %>% left_join(Node_attributes, by = c("name" = "ID"))

# Plotting 

(net_control <- Graph_unipartite_control %>% ggraph::ggraph(layout_with_kk(Graph_unipartite_control)) +
    geom_edge_fan(aes(width=weight),color="gray80", alpha = .8) +
    scale_edge_width_continuous("Edge strength",range=c(0,1))+
    geom_node_point(col="grey10", fill = "purple", alpha = .8, shape = 21) + 
    scale_fill_manual("Edge strength",values = c("blue", "orange", "purple"))+
    geom_node_text(aes(label = name), size=2, color="gray10", repel = TRUE) +
    theme_void() + theme(legend.position = "left",legend.direction = "vertical")+ coord_fixed())# add edges to the plot geom_node_point()

(net_EC <- Graph_unipartite_EC %>% ggraph::ggraph(layout_with_kk(Graph_unipartite_EC)) +
    geom_edge_fan(aes(width=weight),color="gray80", alpha = .8) +
    scale_edge_width_continuous("Edge strength",range=c(0,1))+
    geom_node_point(col="grey10", fill = "purple", alpha = .8, shape = 21) + 
    scale_fill_manual("Edge strength",values = c("blue", "orange", "purple"))+
    geom_node_text(aes(label = name), size=2, color="gray10", repel = TRUE) +
    theme_void() + theme(legend.position = "bottom",legend.direction = "vertical")+ coord_fixed())# add edges to the plot geom_node_point()

(net_CCS <- Graph_unipartite_CCS %>% ggraph::ggraph(layout_with_kk(Graph_unipartite_CCS)) +
    geom_edge_fan(aes(width=weight),color="gray80", alpha = .8) +
    scale_edge_width_continuous("Edge strength",range=c(0,1))+
    geom_node_point(col="grey10", fill = "purple", alpha = .8, shape = 21) + 
    scale_fill_manual("Edge strength",values = c("blue", "orange", "purple"))+
    geom_node_text(aes(label = name), size=2, color="gray10", repel = TRUE) +
    theme_void() + theme(legend.position = "bottom",legend.direction = "vertical")+ coord_fixed())# add edges to the plot geom_node_point()

## ------------------------------------------------------------------------
# 'Scientometric analysis'
## ------------------------------------------------------------------------

# Loading the databases ---------------------------------------------------

MATRIX_1 <- bibliometrix::convert2df("Database/Search_1.txt", dbsource = "wos", format = "plaintext")

# Analysing the data ------------------------------------------------------

results <-
  bibliometrix::biblioAnalysis(MATRIX_1, sep = ";")
summary(object = results, k = 10, pause = FALSE)

S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

# Co-citation -------------------------------------------------------------

# Generating the matrix
NetMatrix2 <-
  biblioNetwork(MATRIX_1,
                analysis = "co-citation",
                network = "references",
                sep = ";")

NetMatrix2@Dimnames

# netstat2 <- networkStat(NetMatrix2)
# summary(netstat2, k=10)

# Plot
pdf("Figures/Network_3.pdf", width = 10.3, height = 6.5)
networkPlot(
  NetMatrix2,
  n = 20,
  Title = "Co-Citation Network",
  type = "fruchterman",
  cluster = "louvain",
  size = T,
  remove.multiple = FALSE,
  labelsize = 0.7,
  edgesize = 5)
dev.off()



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

###

NetMatrix <-
  biblioNetwork(MATRIX_1,
                analysis = "co-occurrences",
                network = "keywords",
                sep = ";")

sort(table(colnames(NetMatrix)))
rownames(NetMatrix)

net3 <- networkPlot(
  NetMatrix,
  normalize = "association",
  weighted = TRUE,
  cluster = "louvain",
  remove.multiple = TRUE,
  n = 50,
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
dev.off()

netstat1 <- networkStat(NetMatrix)
summary(netstat1, k = 10)

# Country network ---------------------------------------------------------

NetMatrix3 <-
  bibliometrix::metaTagExtraction(MATRIX_1, Field = "AU_CO", sep = ";")
NetMatrix3 <-
  bibliometrix::biblioNetwork(NetMatrix3,
                analysis = "collaboration",
                network = "countries",
                sep = ";")

chords <- NetMatrix3 %>%
  as.matrix %>%
  as.dist %>%
  as.matrix %>%
  reshape2::melt() %>%
  mutate(across(
    .cols = c(Var1, Var2),
    .fns = ~
      fct_recode(., "United States of America" = "USA")
  )) %>%
  mutate(across(
    .cols = c(Var1, Var2),
    .fns = ~
      StandardizeText::standardize.countrynames(
        input = .,
        standard = "iso",
        suggest = "auto"
      )
  )) %>%
  rename(Origin = "Var1",
         Destination = "Var2",
         strength = "value") 

centroid <- NetMatrix3 %>%
  as.matrix %>%
  diag %>% 
    data.frame() %>% 
  rownames_to_column("Country") %>% 
  mutate(across(.cols=c(Country), .fns = ~
                  fct_recode(., "United States of America" = "USA"))) %>% 
  mutate(across(.cols=Country, .fns = ~
                  StandardizeText::standardize.countrynames(input = ., standard="iso",suggest="auto"))) %>% 
  rename(N = ".") 




# get world map
wmap <- getMap(resolution="high")

# get centroids
country_sf <- gCentroid(wmap, byid=TRUE)


country_coord <- country_sf %>%
  data.frame %>%
  rownames_to_column("country") %>% 
  mutate(across(.cols=country, .fns = ~
                  StandardizeText::standardize.countrynames(input = ., standard="iso",suggest="auto"))) %>% 
           filter(country %in% chords$Origin)


# Countries of the Search
Country <- data.frame(table(db$Country_search))
colnames(Country) <- c("Country", "N_news")

# Adding the coordinate of each country
Country <- unique(dplyr::left_join(x  = Country, 
                                   y  = data.frame(Country = db$Country_search, long = db$lon3, lat = db$lat3), 
                                   by ="Country", copy = FALSE)) 

##Calculating country connections
all_pairs <- data.frame(long1 = NA, 
                        long2 = NA, 
                        lat1  = NA, 
                        lat2  = NA, 
                        lwd   = NA, 
                        countrySearch = NA, 
                        country = NA) 

for (i in 1:nlevels(db$Country_search)) {
  
  #select first country
  country_i <- db[db$Country_search == as.character(unique(db$Country_search)[i]),]
  
  #remove potential NAs
  country_i <- country_i[1:table(db$Country_search)[i],]
  
  country_i$Country_event <- droplevels(country_i$Country_event)
  
  country_i <- country_i[country_i$Country_event != as.character(unique(db$Country_search)[i]), ]
  
  country_i <- subset(country_i, !is.na(lon2) & !is.na(lat2))
  
  
  if(nrow(country_i) < 1){
    NULL
  }
  
  else {
    len <- length(table(droplevels(country_i$Country_event)))
    
    all_pairs2 <- data.frame( long1 = rep(country_i$lon3[1], len),
                              long2 = c(unique(country_i$lon2)),
                              lat1 = rep(country_i$lat3[1], len),
                              lat2 = c(unique(country_i$lat2)),
                              lwd = as.numeric(table(droplevels(country_i$Country_event))),
                              countrySearch = rep(as.character(unique(db$Country_search)[i]),len),
                              country= names(table(droplevels(country_i$Country_event)))
    ) 
    
    all_pairs  <- rbind(all_pairs,all_pairs2)
    
  }
}

all_pairs <- na.omit(all_pairs)

world<-map_data("world")

(map2 <- ggplot() +
    geom_map(map = world, data = world,
             aes(long, lat, map_id = region), 
             color = "gray50", fill = "grey70", size = 0.3) +
    
    geom_curve(aes(x = jitter(long1,0.0001), 
                   y = jitter(lat1,0.0001), 
                   xend = jitter(long2, 0.0001), 
                   yend = jitter(lat2, 0.0001),  # draw edges as arcs
                   size = lwd),
               data = all_pairs, curvature = 0.22,
               alpha = 0.2,  color = "orange") +
    
    geom_point(data = Country, 
               aes(x = long, y = lat),
               alpha = 0.7, colour = "black",fill="blue",
               size = range01(sqrt(Country$N_news))*13,
               shape = 21,stroke = 0.8)+
    
    
    scale_size_continuous("Number of connections:", breaks=c(1,5,10,15))+
    
    theme_map()+
    # theme(legend.position = "bottom",
    #       legend.text = element_text(size = 12),
    #       legend.title = element_text(size = 12),
    theme(
      axis.line=element_blank(),axis.text.x=element_blank(),
      axis.text.y=element_blank(),axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),legend.position="none",
      panel.background=element_rect(fill = "black", colour = "black"),
      panel.border=element_blank(),panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background= element_rect(fill = "black", colour = "black"))
  
)




