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

pacman::p_load(
  bibliometrix,
  ergm,
  fs,
  ggraph,
  network,
  SemNetCleaner,
  tidyverse,
  tidygraph,
  rgeos,
  rworldmap,
  igraph,
  magrittr,
  StandardizeText,
  ggmap,
  ggfx,
  qdapRegex
)

# Functions ---------------------------------------------------------------


# Function to extract node traits in a graph 
# For a discussion:
# https://medium.com/@615162020004/social-network-analysis-with-r-centrality-measure-86d7fa273574
# Graph should be a tidygraph dataset

NodeTraitGet <- function(Graph, mode = "in", dir = TRUE){
  
  list(
    ID          = Graph %>% activate(nodes) %>% as.data.frame %>% pull(1),
    Degree      = igraph::degree(Graph, mode = mode),
    Strength    = strength(Graph, mode = mode),
    Eigenvector = Graph %>% eigen_centrality(directed = dir) %>% extract2("vector"),
    Betweenness = Graph %>% betweenness(directed = dir),
    Closeness   = Graph %>% closeness(mode = mode)
  )
}

# Loading the databases ---------------------------------------------------

ref_tab   <- read_csv("Database/Table_papers.csv") #database listing papers 
db_graph_original <- read_csv("Database/text_compiled_relative.csv") #database of ngrams by paper

  
# Summary statistics  ---------------------------------------------------------------------------------------------


ref_tab %>%
  full_join(db_graph_original , by = "WOS_ID") %>%  
  hacksaw::keep_na(diversity) %>% 
  select("SEARCH_TYPE", "DOI", "WOS_ID", "EXTRACTED_BY") %>% 
  drop_na(DOI) %>% 
  data.frame

db_graph_original %>%
  left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE)) %>% as_tibble() %>% 
  select(SEARCH_TYPE) %>% table


zero_sum<- names(which(rowSums(db_graph_original %>%  column_to_rownames("WOS_ID")) == 0))
data.frame(WOS_ID = zero_sum) %>%  left_join(ref_tab) %>%  select(SEARCH_TYPE, WOS_ID)
  
# Generate networks  --------------------------------------------

network_words <- db_graph_original %>%
  left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE)) %>% as_tibble() %>% 
  filter(SEARCH_TYPE == "Ecological complexity") %>%
  select(-SEARCH_TYPE) %>% 
  pivot_longer(!c(WOS_ID)) %>% 
  pivot_wider(names_from = WOS_ID, values_from = value) %>% 
  column_to_rownames("name") %>% 
  igraph::graph_from_incidence_matrix(multiple = TRUE,
                                      directed = TRUE,
                                      weighted = NULL) %>%
  igraph::bipartite_projection() %>%  #projecting to unipartite
  pluck("proj1") %>% #selecting node type 1
  as_tbl_graph(directed = TRUE) %>%  #convert to table graph object
  tidygraph::activate(nodes) %>% #activating nodes
  left_join(
    #merging nodes with number of articles using words
    db_graph_original %>%
      left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE)) %>% 
      filter(SEARCH_TYPE == "Ecological complexity") %>%
      select(-SEARCH_TYPE) %>% 
      pivot_longer(!c(WOS_ID)) %>% 
      pivot_wider(names_from = WOS_ID, values_from = value) %>% 
      column_to_rownames("name") %>% 
      mutate(across(.fns =  ~ ifelse(. > 0 , 1, 0))) %>% #convert abundance of words into occurrence
      transmute(N =  rowSums(.))  %>% #sums of the articles using a word
      rownames_to_column("name")
  )  %>%
  left_join(
    #add the node level attributes
    db_graph_original %>%
      left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE)) %>% 
      filter(SEARCH_TYPE == "Ecological complexity") %>%
      select(-SEARCH_TYPE) %>% 
      pivot_longer(!c(WOS_ID)) %>% 
      pivot_wider(names_from = WOS_ID, values_from = value) %>% 
      column_to_rownames("name") %>% 
      igraph::graph_from_incidence_matrix(multiple = TRUE,
                                          directed = TRUE,
                                          weighted = NULL) %>%
    igraph::bipartite_projection() %>% #projecting to unipartite
      pluck("proj1") %>%  #selecting node type 1
      as_tbl_graph(directed = TRUE) %>%  #convert to table graph object
      NodeTraitGet(mode = "in", dir = FALSE)  %>% #calculate node level attributes using our custom function
      bind_cols()  #collapse columns
    ,
    by = c("name" = "ID")
  )  %>%
  tidygraph::activate(edges) %>%
  mutate(weight = weight / max(weight)) %>%
  tidygraph::activate(nodes) %>%
  mutate(Strength = Strength / max(Strength))



network_papers <- db_graph_original %>%
  left_join(ref_tab %>% select(WOS_ID,SEARCH_TYPE)) %>%
  filter(SEARCH_TYPE != "Complex system science") %>% 
  select(-"SEARCH_TYPE") %>% 
  pivot_longer(!c(WOS_ID)) %>% 
  pivot_wider(names_from = WOS_ID, values_from = value) %>% 
  column_to_rownames("name") %>% 
  igraph::graph_from_incidence_matrix(multiple = TRUE,
                                      directed = TRUE,
                                      weighted = NULL) %>%
  igraph::bipartite_projection() %>%  #projecting to unipartite
  pluck("proj2") %>% #selecting node type 1
  as_tbl_graph(directed = TRUE) %>%  #convert to table graph object
  tidygraph::activate(nodes) %>% #activating nodes
  left_join(
    ref_tab %>%
      dplyr::select(WOS_ID,SEARCH_TYPE),
    by=c("name"="WOS_ID")
  )  


# Plotting  -------------------------------------------------------------------------------------------------------
theme_set(theme_minimal())
  theme_update(
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    text = element_text(size = 12, colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.y = element_blank(),
    
    line = element_line(colour = "black", size = 1),
    strip.text = element_text(colour = "white"),
    strip.background = element_rect(
      color = "black",
      fill = "black",
      size = 1.5,
      linetype = "solid"
    ),
    
    plot.title.position = "plot",
    plot.background = element_blank(),
    
    legend.justification = 'center',
    legend.position = 'bottom',
    legend.box = 'horizontal',
    legend.box.just = 'left'
  )
 

figure_1a <-
  network_words %>% 
    ggraph::ggraph(. ,  layout_with_kk(.)) +
  geom_edge_arc(aes(width=weight),
                 strength = .2,
                color="#bfd3e6", alpha = .8) +
  geom_node_point(col="grey10", 
                  aes(size=N, fill=Strength),
                  shape = 21) + 
  geom_node_text(aes(label = stringr::str_to_sentence(gsub("_"," ",name))),
                     size=4,
                     color="gray10",
                     repel = TRUE,
                     force=10)+
  guides(fill = guide_colorbar(title.position = 'top', label.position = 'bottom',order=2),
         edge_width = guide_legend(title.position = 'top', 
                                 label.position = 'bottom',order=3),
         size = guide_legend(title.position = 'top', label.position = 'bottom',order=1))+
  scale_fill_gradient2("Word importance (Strength)",
                       low= "#f7fcfd",
                       mid = "#8c96c6",
                       high = "#4d004b") +
  scale_edge_width_continuous("Connection strength (Weight)",range=c(0.1,1.1))+
  scale_size_continuous(breaks = scales::breaks_pretty(4))+
  labs(size= "Number of papers")

ggsave("Figures/Figure_1a.png", plot = figure_1a, width=8,height=8, dpi = 600)


figure_s1 <-
  network_papers %>% igraph::simplify(edge.attr.comb = "sum") %>%
  ggraph::ggraph(. , layout_with_kk(.)) +
  geom_edge_fan(width = .1,
                color = "#bfd3e6",
                alpha = .8) +
  geom_node_point(aes(fill = SEARCH_TYPE),
                  col = "grey10",
                  shape = 21) +
  guides(
    fill = guide_legend(
      title.position = 'top',
      label.position = 'bottom',
      order = 2
    ),
    edge_width = guide_legend(
      title.position = 'top',
      label.position = 'bottom',
      order = 3
    ),
    size = guide_legend(
      title.position = 'top',
      label.position = 'bottom',
      order = 1
    )
  ) +
  labs(size = "Number of words", fill = "Search group")

ggsave("Figures/Figure_S1.pdf", plot = figure_s1, device=cairo_pdf, width=7,height=7)

# fitting a model ---------------------------------------------------------

ResponseNetwork <- network_papers %>% 
  get.adjacency(sparse = FALSE) %>%
  as.matrix %>%  as.network(
    matrix.type = "a",
    ignore.eval = FALSE,
    names.eval = "weight"
  ) 
ResponseNetwork %v%  "SEARCH_TYPE"  <- network_papers %>% pull(SEARCH_TYPE)

# Fitting the model
ERGM1 <- ergm::ergm(ResponseNetwork ~ edges + nodefactor("SEARCH_TYPE") + nodematch("SEARCH_TYPE"), estimate = "MLE")

summary(ERGM1)

Figure_s2 <-  ERGM1 %>%
  summary %>%
  extract2("coefficients") %>%
  data.frame %>%
  rownames_to_column("Variable") %>%
  left_join(ERGM1 %>%
              confint %>%  # Grabbing the 95% confidence intervals
              data.frame %>%
              rename(Lower = 1, Upper = 2) %>% 
              rownames_to_column("Variable")) %>% 
  mutate(Variable = fct_recode(Variable, 
                               "Edges" = "edges",
                               "Node factor" = "nodefactor.SEARCH_TYPE.Ecological complexity",
                               "Node match" = "nodematch.SEARCH_TYPE"))  %>% 
 ggplot2::ggplot(aes(Variable, Estimate)) +
  geom_hline(lty = 2, yintercept = 0, col = "grey30") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = .2, size=1) +
  geom_point(size= 4) +
    theme_bw()+
  coord_flip()+
  labs(x="", y="Mean effect size (95% confidence interval)")










network_words <- db_graph_original %>%
  left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE)) %>% as_tibble() %>% 
  filter(SEARCH_TYPE == "Ecological complexity") %>%
  select(-SEARCH_TYPE) %>% 
  pivot_longer(!c(WOS_ID)) %>% 
  pivot_wider(names_from = WOS_ID, values_from = value) %>% 
  column_to_rownames("name") %>% 
  igraph::graph_from_incidence_matrix(multiple = TRUE,
                                      directed = TRUE,
                                      weighted = NULL) %>%
  igraph::bipartite_projection() %>%  #projecting to unipartite
  pluck("proj1") %>% #selecting node type 1
  as_tbl_graph(directed = TRUE) %>%  #convert to table graph object
  tidygraph::activate(nodes) %>% #activating nodes
  left_join(
    #merging nodes with number of articles using words
    db_graph_original %>%
      left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE)) %>% 
      filter(SEARCH_TYPE == "Ecological complexity") %>%
      select(-SEARCH_TYPE) %>% 
      pivot_longer(!c(WOS_ID)) %>% 
      pivot_wider(names_from = WOS_ID, values_from = value) %>% 
      column_to_rownames("name") %>% 
      mutate(across(.fns =  ~ ifelse(. > 0 , 1, 0))) %>% #convert abundance of words into occurrence
      transmute(N =  rowSums(.))  %>% #sums of the articles using a word
      rownames_to_column("name")
  )  %>%
  left_join(
    #add the node level attributes
    db_graph_original %>%
      left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE)) %>% 
      filter(SEARCH_TYPE == "Ecological complexity") %>%
      select(-SEARCH_TYPE) %>% 
      pivot_longer(!c(WOS_ID)) %>% 
      pivot_wider(names_from = WOS_ID, values_from = value) %>% 
      column_to_rownames("name") %>% 
      igraph::graph_from_incidence_matrix(multiple = TRUE,
                                          directed = TRUE,
                                          weighted = NULL) %>%
      igraph::bipartite_projection() %>% #projecting to unipartite
      pluck("proj1") %>%  #selecting node type 1
      as_tbl_graph(directed = TRUE) %>%  #convert to table graph object
      NodeTraitGet(mode = "in", dir = FALSE)  %>% #calculate node level attributes using our custom function
      bind_cols()  #collapse columns
    ,
    by = c("name" = "ID")
  )  %>%
  tidygraph::activate(edges) %>%
  mutate(weight = weight / max(weight)) %>%
  tidygraph::activate(nodes) %>%
  mutate(Strength = Strength / max(Strength))





getMap(resolution="high") %>% 
  ggplot() +
  geom_polygon(aes(long,lat, group=group), colour="black", fill="black", size = 0.25)+
  with_outer_glow(geom_point(data = coord_authors, 
                             aes(x = lat, 
                                 y = long) , size=.1, shape = 19, colour="#FFEA46FF"), colour="#FFEA46FF", sigma=2, expand= 2) +
  coord_fixed(ratio = 1)+
  scale_colour_gradientn(colours = c("yellow","white"))+
  scale_size_continuous(range = c(.1, 2))+
  ggthemes::theme_map()+
  theme( 
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="none",
    panel.background=element_rect(fill ="gray10", colour="gray10"),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill ="gray10", colour="gray10")
  )

coord_authors <-  read_csv("coordinates_coauthors.csv")




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
library(bibliometrix)
NetMatrix3 <-
  bibliometrix::metaTagExtraction(MATRIX_1, Field = "AU_SO", sep = ";") %>% 
  bibliometrix::biblioNetwork(
                analysis = "coupling",
                network = "references",
                sep = ";")
networkPlot(NetMatrix3,
            normalize = "jaccard",
            weighted = TRUE,
            cluster = "louvain",
            remove.multiple = TRUE)
prep_chords <- NetMatrix3 %>%
  as.matrix %>%
  as.dist %>%
  as.matrix %>%
  reshape2::melt() %>%
  mutate(across(
    .cols = c(Var1, Var2),
    .fns = ~
      fct_recode(., "United States of America" = "USA")
  )) %>%
  mutate(
    across(.cols=c(Var1,Var2), .fns =~ StandardizeText::standardize.countrynames(input = ., 
                                                                               standard="iso",
                                                                               suggest="auto")
    )
  ) %>% 
  mutate(Var = map2_chr(Var1, Var2, ~ toString(sort(c(
    .x, .y
  ))))) %>%
  distinct(Var, .keep_all = TRUE) %>%
  select(-Var) %>%
  rename(Origin = "Var1",
         Destination = "Var2",
         strength = "value") 

prep_centroid <- NetMatrix3 %>%
  as.matrix %>%
  diag %>% 
    data.frame %>% 
  rownames_to_column("Country") %>% 
  mutate(
    across(.cols=c(Country), .fns = ~ fct_recode(.,
                                                 "United States of America" = "USA")
           )
    ) %>% 
  rename(N = ".") %>% 
  mutate(
    across(.cols=c(Country), .fns =~ StandardizeText::standardize.countrynames(input = ., 
                                                                            standard="iso",
                                                                            suggest="auto")
           )
    )


# get world map

ggmap::register_google('AIzaSyBP7U9mHiqB2q83f9JrNq4yjn4b6rAKhTg')
wmap <- getMap(resolution="high")
PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
# or use the short form "+proj=robin"
map  <- spTransform(wmap, CRSobj = PROJ)
# get centroids
country_sf <- gCentroid(map, byid=TRUE)


country_coord <- country_sf %>%
  data.frame %>%
  rownames_to_column("country") %>% 
  mutate(across(.cols=country, .fns = ~ StandardizeText::standardize.countrynames(input = ., standard="iso",suggest="auto"))) %>% 
           filter(country %in% prep_centroid$Country)

chords <- prep_chords %>% 
  left_join(country_coord, by = c("Origin" = "country")) %>% 
  left_join(country_coord, by = c("Destination" = "country")) %>% 
  rename_with(.cols = c(x.x,y.x,x.y,y.y), .fn = ~ c("Long_origin","Lat_origin","Long_destiny","Lat_destiny")) 

centroid <- prep_centroid %>%
  left_join(country_coord, by = c("Country" = "country")) %>% 
  rename(Lat = "y", Long = "x")

network_global<- ggplot() +
  # with_outer_glow(geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="white", fill="gray10", size = .01), colour="white", sigma=.1) +
  geom_polygon(data=NE_countries_rob, aes(long,lat, group=group), colour="black", fill="black", size = 0.25) +
  coord_fixed(ratio = 1) +
  with_outer_glow(geom_curve(data = chords,
                             aes(x = jitter(Long_origin,0.0001), 
                                 y = jitter(Lat_origin,0.0001), 
                                 xend = jitter(Long_destiny, 0.0001), 
                                 yend = jitter(Lat_destiny, 0.0001),  # draw edges as arcs
                                 colour = strength),
                             curvature = 0.2,
                             alpha = 0.02, 
                             color = "#FFEA46FF"),colour="white",sigma=2) +
  with_outer_glow(geom_point(data = centroid, 
                             aes(x = Long, 
                                 y = Lat,
                                 size=sqrt(N)) , shape = 19, colour="#FFEA46FF"), colour="#FFEA46FF", sigma=3)+
  scale_colour_gradientn(colours = c("yellow","white"))+
  scale_size_continuous(range = c(.1, 2))+
  ggthemes::theme_map()+
  theme( 
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="none",
    panel.background=element_rect(fill ="gray10", colour="gray10"),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill ="gray10", colour="gray10")
  )

ggsave("Figures/network_map.pdf", network_global, device=cairo_pdf)


#network_global<-
  getMap(resolution="high") %>% 
  ggplot() +
   geom_polygon(aes(long,lat, group=group), colour="black", fill="black", size = 0.25)+
  with_outer_glow(geom_point(data = coord_authors, 
                             aes(x = lat, 
                                 y = long) , size=.1, shape = 19, colour="#FFEA46FF"), colour="#FFEA46FF", sigma=2, expand= 2) +
  coord_fixed(ratio = 1)+
  scale_colour_gradientn(colours = c("yellow","white"))+
  scale_size_continuous(range = c(.1, 2))+
  ggthemes::theme_map()+
  theme( 
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="none",
    panel.background=element_rect(fill ="gray10", colour="gray10"),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill ="gray10", colour="gray10")
  )

  coord_authors <-  read_csv("coordinates_coauthors.csv")

  authors_affiliation <-bibliometrix::metaTagExtraction(MATRIX_1, Field = "AU_UN", sep = ";")  %>%  as_tibble() 
    bibliometrix::biblioNetwork(
      analysis = "coupling",
      network = "references",
      sep = ";")
  
  coord_authors<-data.frame(lat=NA,long=NA)
  for (i in 1:nrow(MATRIX_1)){
    coord_authors[i,]<- tryCatch(ggmap::geocode(MATRIX_1$C1[i]), error = function(e) c(NA,NA))
    
  }
  coord_authors[32,]<- ggmap::geocode("QUITO, ECUADOR")
  MATRIX_1[33,"C1"]
  
  ggmap::register_google('AIzaSyBP7U9mHiqB2q83f9JrNq4yjn4b6rAKhTg')
  
all_authors_affiliation<-   authors_affiliation %>% 
  mutate(C1 = gsub('.{1}$', '', C1)) %>%  select(AU_UN) %>% slice(5)
    separate_rows(C1,AU, sep=";") %>%  drop_na(C1) 
authors_affiliation %>%  glimpse
authors_affiliation$C1

all_authors_affiliation<- ref_tab %>% 
  filter(SEARCH_TYPE == "Ecological complexity") %>% 
  select(Authors,Addresses,WOS_ID) %>% 
 mutate(Addresses= gsub("\\.;", "\\./", Addresses)) %>% 
  separate_rows(Addresses, sep=";") %>% 
  mutate(authors_new = unlist(qdapRegex::ex_bracket(Addresses))) %>% 
  mutate(Addresses = rm_square(Addresses)) 

coord_authors<-data.frame(lat=NA,long=NA)
for (i in 1:nrow(all_authors_affiliation)){
  coord_authors[i,]<- tryCatch(ggmap::geocode(all_authors_affiliation$Addresses[i]), error = function(e) c(NA,NA))
  
}

x<-"[Butler, Joy I.] Univ British Columbia, Vancouver, BC V6T 1Z4, Canada"
gsub("\\[([^()]*)\\]|.", "\\1", x, perl=T)
test<- qdapRegex::ex_bracket("[Butler, Joy I.] Univ British Columbia, Vancouver, BC V6T 1Z4")
unlist(test)
  text_2<-gsub("\\.;", "\\./", text)
  
  sub("^.*?\\((.*)\\)[^)]*$", "\\1", text_2)
  