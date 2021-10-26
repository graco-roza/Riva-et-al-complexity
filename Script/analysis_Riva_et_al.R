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

# hacksaw package missing from list above

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

coord_authors <-  read_csv("coordinates_coauthors.csv")

all_authors_affiliation<- ref_tab %>% 
  filter(SEARCH_TYPE == "Ecological complexity") %>% 
  select(Authors,Addresses,WOS_ID) %>% 
  mutate(Addresses= gsub("\\.;", "\\./", Addresses)) %>% view()
  separate_rows(Addresses, sep=";") %>% 
  mutate(authors_new = unlist(qdapRegex::ex_bracket(Addresses))) %>% 
  mutate(Addresses = rm_square(Addresses)) %>% 
  view



all_authors_affiliation %>%
  bind_cols(coord_authors) %>% 
  mutate(authors_new = ifelse(is.na(authors_new), Authors, authors_new)) %>% 
  hacksaw::keep_na(lat) %>% 
  data.frame() %>% 
  view()


all_authors_affiliation<- ref_tab %>% 
  filter(SEARCH_TYPE == "Ecological complexity") %>% 
  select(Authors,Addresses,WOS_ID) %>% 
  mutate(Addresses= gsub("\\[", "(", Addresses)) %>% 
  mutate(Addresses= gsub("\\]", ")", Addresses)) %>% 
  mutate(Addresses= gsub(";(?=[^()]*\\))" , " sep ",Addresses, perl = T)) %>% 
  separate_rows(Addresses, sep=";") %>% 
  mutate(authors_new = unlist(qdapRegex::ex_round(Addresses))) %>%
  separate_rows(authors_new, sep=" sep ") %>%
  mutate(Addresses = rm_round(Addresses))  %>% 
  distinct(Addresses, authors_new, WOS_ID, .keep_all=TRUE) %>%
  mutate(authors_new = ifelse(is.na(authors_new), Authors, authors_new)) %>% 
  separate_rows(authors_new, sep=";")  %>%  
  mutate(authors_new = str_trim(str_to_title(authors_new))) %>% 
  distinct(Addresses, authors_new, WOS_ID, .keep_all=TRUE) %>%
  drop_na(Addresses) %>% 
  bind_cols(coord_authors) %>% 
  mutate(authors_new =   sapply(authors_new, function(x) make_clean_names(x, case = "all_caps", sep_out=" "))) %>%
  mutate(authors_new = factor(authors_new, levels = unique(authors_new))) %>% 
  distinct(authors_new, WOS_ID, .keep_all = TRUE) %>% 
  mutate(ID = as.numeric(authors_new))


coord_authors<-data.frame(lat=NA,long=NA)
for (i in 1:nrow(all_authors_affiliation)){
  print(i)
  coord_authors[i,]<- tryCatch(ggmap::geocode(all_authors_affiliation$Addresses[i]), error = function(e) c(NA,NA))

network_collaborations<-  all_authors_affiliation %>%  
  select(WOS_ID, authors_new) %>% 
  mutate(value = 1 ) %>% 
  pivot_wider(id_cols = c(WOS_ID, authors_new),names_from = WOS_ID, values_from = value, values_fill = 0, values_fn=sum) %>% 
  column_to_rownames("authors_new") %>% 
  igraph::graph_from_incidence_matrix(multiple = TRUE,
                                      directed = TRUE,
                                      weighted = NULL) %>%
  igraph::bipartite_projection() %>%  #projecting to unipartite
  pluck("proj1") %>% #selecting node type 1
  as_tbl_graph(directed = TRUE)  #convert to table graph object

  # Separate out edges and node data frames
  colab_nodes <-
    network_collaborations %>%
  activate(nodes) %>%
  data.frame() %>%
  rownames_to_column("rowid") %>%
  mutate(rowid = as.integer(rowid))
 
  colab_edges <-
    network_collaborations %>%
  activate(edges) %>%
  data.frame()

named_edge_list <-
  colab_edges %>%
  # Rename from nodes
  left_join(colab_nodes, by = c("from" = "rowid")) %>%
  select(-from) %>%  # Remove unneeded column
  rename(from = name) %>%  # Rename column with names now
  
  # Rename to nodes
  left_join(colab_nodes, by = c("to" = "rowid")) %>%
  select(-to) %>%  # Remove unneeded column
  rename(to = name) %>%  # Rename column with names now
  
  # Cleaning up
  select(from, to, weight) %>% 
  left_join(all_authors_affiliation %>%  select(lat,long,authors_new), by = c("from" = "authors_new")) %>% 
  left_join(all_authors_affiliation %>%  select(lat,long,authors_new), by = c("to" = "authors_new")) %>% 
  rename_with(.cols = c(lat.x,long.x,lat.y,long.y), .fn = ~ c("Lat_from","Long_from","Lat_to","Long_to")) 
  
  
  
library(tidygraph)


getMap(resolution="high") %>% 
  ggplot() +
geom_polygon(aes(long,lat, group=group), colour="black", fill="black", size = 0.1)+
  with_outer_glow(geom_point(data = all_authors_affiliation, 
                             aes(x = lat, 
                                 y = long) , size=.1, shape = 19, colour="#FFEA46FF"), colour="#FFEA46FF", sigma=2) +
  with_outer_glow(geom_curve(data = named_edge_list,
                             aes(y = jitter(Long_from,0.0001), 
                                 x = jitter(Lat_from,0.0001), 
                                 yend = jitter(Long_to, 0.0001), 
                                 xend = jitter(Lat_to, 0.0001),  # draw edges as arcs
                                 colour = strength),
                             curvature = 0.2,
                             alpha = 0.02, 
                             color = "#FFEA46FF"),colour="white",sigma=2) +
  coord_fixed(ratio = 1)+
  scale_colour_gradientn(colours = c("yellow","white"))+
  
  ggthemes::theme_map()+
  theme( 
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),legend.position="none",
    panel.background=element_rect(fill ="#152238", colour="#152238"),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill ="#152238", colour="#152238")
  )



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
                analysis = "collaboration",
                network = "authors",
                sep = ";")

networkPlot(NetMatrix3,
            weighted = TRUE,
            cluster = "louvain",
            remove.multiple = TRUE)

prep_chords <- NetMatrix3 %>%
  as.matrix %>%
  as.dist %>%
  as.matrix %>%
  reshape2::melt() %>%
  filter(value > 0)
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
  


all_authors_affiliation<- ref_tab %>% 
  filter(SEARCH_TYPE == "Ecological complexity") %>% 
  select(Authors,Addresses,WOS_ID) %>% 
 mutate(Addresses= gsub("\\[", "(", Addresses)) %>% 
  mutate(Addresses= gsub("\\]", ")", Addresses)) %>% 
  mutate(Addresses= gsub(";(?=[^()]*\\))" , " sep ",Addresses, perl = T)) %>% 
  separate_rows(Addresses, sep=";") %>% 
  mutate(authors_new = unlist(qdapRegex::ex_round(Addresses))) %>%
  separate_rows(authors_new, sep=" sep ") %>%
  mutate(Addresses = rm_round(Addresses))  %>% 
  distinct(Addresses, authors_new, WOS_ID, .keep_all=TRUE) %>%
  mutate

coord_authors<-data.frame(lat=NA,long=NA)
for (i in 1:nrow(all_authors_affiliation)){
  coord_authors[i,]<- tryCatch(ggmap::geocode(all_authors_affiliation$Addresses[i]), error = function(e) c(NA,NA))
  
}
x<-"(Butler, Joy I.; blabla) Univ British Columbia, Vancouver (dedede), BC V6T 1Z4"
gsub("(?m);(?=[^()]*\))"," ", text, perl = T)
x<- "a: a(1; 2; 3); b: b[1; 2]"
gsub(";(?=[^()]*\\))" , ",",x, perl = T)



#####################################################
#####################################################
#####################################################
# Alpha and Beta diversity analysis

# this is copied from above for ease, simply re-open the original tables
ref_tab   <- read_csv("Database/Table_papers.csv") #database listing papers 
db_graph_original <- read_csv("Database/text_compiled_relative.csv") #database of ngrams by paper

# merge to create the analysis table below
data_beta <- merge(db_graph_original, ref_tab, by = "WOS_ID")
data_beta <- data_beta[!(data_beta$SEARCH_TYPE=="Complex system science"),] # remove the CSS papers, which are not in the analysis anymore
table(data_beta$SEARCH_TYPE) # check sample sizes; 180 controls vs 172 ecological complexity papers

# main table for analysis: retain only themese (species) and type of research
table_sp <- data_beta[ , c(1:26, 28)]

# there are 6 empty rows that can
# mess with the calculations; we should discuss solutions
table_sp <- table_sp[apply(table_sp[,c(-1, -27)], 1, function(x) !all(x==0)),]
# there are now 4 less controls and 2 less ecological complexity papers


##
## PART 1: alpha diversity
##


library(vegan)
library(ggpubr)

richness <- vegan::specnumber(table_sp[,2:26])  
div <- exp(vegan::diversity(table_sp[,2:26])) # exp of Shannon entropy, Hill number of order 1; 
# see http://www.loujost.com/Statistics%20and%20Physics/Diversity%20and%20Similarity/EffectiveNumberOfSpecies.htm#:~:text=The%20number%20of%20equally%2Dcommon%20species%20required%20to%20give%20a,4.5)%20%3D%2090%20effective%20species. 
# Shannon entropy as a golden number for ecologists (MacArthur 1965; https://onlinelibrary.wiley.com/doi/10.1111/j.1469-185X.1965.tb00815.x)

summary(lm(richness ~ table_sp$SEARCH_TYPE))
summary(lm(div ~ table_sp$SEARCH_TYPE))
# the difference between control and complexity papers is   


library(ggplot2)
library(ggpubr)

boxplot <- data.frame(richness, div, table_sp$SEARCH_TYPE)

plot1 <- boxplot %>%
  ggplot( aes(x=table_sp$SEARCH_TYPE, y=richness, fill=table_sp$SEARCH_TYPE)) +
  #geom_violin() +
  scale_fill_manual(values=c("gainsboro", "cyan2")) +
  #geom_jitter(color="black", size=1, alpha=0.9) +
  theme_bw()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ggtitle("True diversity of complexity themes in a publication") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.7) +
  geom_boxplot(width=0.1, outlier.shape = NA, alpha = 0.8) +
  ylim(0,20)+
  ylab("Richness")+
  xlab("") + 
  geom_signif(comparisons = list(c("Control", "Ecological complexity")), 
              map_signif_level=TRUE)

plot2 <- boxplot %>%
  ggplot( aes(x=table_sp$SEARCH_TYPE, y=div, fill=table_sp$SEARCH_TYPE)) +
  #geom_violin() +
  scale_fill_manual(values=c("gainsboro", "cyan2")) +
  #geom_jitter(color="black", size=1, alpha=0.9) +
  theme_bw()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ggtitle("True diversity of complexity themes in a publication") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.7) +
  geom_boxplot(width=0.1, outlier.shape = NA, alpha = 0.8) +
  ylim(0,20)+
  ylab("True diversity")+
  xlab("")+ 
  geom_signif(comparisons = list(c("Control", "Ecological complexity")), 
              map_signif_level=TRUE)

ggarrange(plot1, plot2)


##
## PART 2: beta diversity
##

# perhaps convert into presence/absence matrix
#table_sp <- table_sp  %>% mutate_if(is.numeric, ~1 * (. > 0))


# indicator words: are there themes typical of complexity papers?
library(indicspecies)
ind <- indicspecies::multipatt(table_sp[, 2:26], 
                table_sp$SEARCH_TYPE, 
                func = "r.g", 
                control = how(nperm=9999))
summary(ind)

# several words typical of ecological complexity studies
# scale_dependency  0.276  0.0001 ***
#   self_organization 0.201  0.0001 ***
#   dynamicity        0.197  0.0001 ***
#   hierarchy         0.186  0.0001 ***
#   non_linearity     0.171  0.0004 ***
#   resilience        0.156  0.0001 ***
#   fractality        0.152  0.0001 ***
#   feedback          0.149  0.0024 ** 
#   attractor         0.149  0.0001 ***
#   memory            0.137  0.0039 ** 
#   tipping_point     0.132  0.0121 *  
#   emergence         0.130  0.0126 *  
#   adaptation        0.128  0.0108 *  
#   non_equilibrium   0.109  0.0048 ** 


# similarity between vs within groups
similarity <- vegan::anosim(table_sp[, 2:26], 
                     table_sp$SEARCH_TYPE, 
                     permutations = 999, 
                     distance = "jaccard", 
                     strata = NULL,
                     parallel = 10)
similarity
# statistically significant difference in the themes touched between controls and ecological complexity papers




# permanova; note that adonis does not run if we retain the six columns with all 0s
perm_diss <- vegan::adonis2(table_sp[, 2:26] ~ table_sp$SEARCH_TYPE, 
                  permutations = 999, 
                  method = "jaccard", 
                  sqrt.dist = FALSE, 
                  add = FALSE, 
                  by = NULL, 
                  parallel = 10)


perm_diss
# significant difference, but only 0.016 of variation explained
# when using presence/absence data, we go up to 3% of variation explaines

# multi-response permutation procedure (MRPP)
mrpp <- vegan::mrpp(table_sp[, 2:26], table_sp$SEARCH_TYPE)
mrpp
# the control and ecological complexity papers differ significantly also based on the frequency of themes use
# but observed and expected delta are very similar, suggesting very small effect size



# # nMDS, I leave it here but virtually useless; no apparent difference between the two groups
# # BTW, same for PCA
# NMDS <- metaMDS(table_sp[, 2:26], k=2, trymax=100)
# stressplot(NMDS)
# plot(NMDS)
# 
# plot(NMDS,type="n")
# ordihull(NMDS,groups=table_sp$SEARCH_TYPE,draw="polygon",col="grey90",label=F)
# orditorp(NMDS,display="species",col="red",air=0.01)
# orditorp(NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
#          air=0.01,cex=1.25)

#contribution of themes to bray curtis similarity between papers
vegan::simper(table_sp[, 2:26], table_sp$SEARCH_TYPE)


# # distance-based RDA
# # construct full model and calculate VIF
# dbRDA <- capscale(table_sp[, 2:26] ~ table_sp$SEARCH_TYPE)
# vif.cca(dbRDA)
# 
# #model is significantly better than null model
# anova(dbRDA)
# anova(dbRDA, by = "terms")
# summary(dbRDA) # only 2% of variation explained
# plot(dbRDA)


# Caio's proposed analysis 

dec <- adespatial::beta.div.comp((table_sp[, 2:26]), coef = "J")
dec$part
# 42% of beta diversity is due to replacement, 57% to richness

BDLG <- adespatial::beta.div((table_sp[, 2:26]), 
                             method="hellinger")

# Local contributions to beta diversity (LCBD indices)
LGLCBD = data.frame(BDLG$LCBD, BDLG$p.LCBD)
LGLCBD <-cbind(LGLCBD, table_sp)
LGLCBD$pval <- ifelse(LGLCBD$BDLG.p.LCBD >= 0.05, c("non-sig"),
                      c("sig"))

LGLCBD$richness <- richness
LGLCBD$div <- div

# Local contributions to beta diversity (LCBD indices) represent the degree of uniqueness of the sites
# in terms of their species compositions. They can be computed in all cases: raw (not recommended) 
# or transformed data, as well as dissimilarity matrices. See Legendre and De Cáceres (2013) for details. 
# LCBD indices are tested for significance by random, independent permutations within the columns of Y. 
# This permutation method tests H0 that the species are distributed at random, independently of one another, 
# among the sites, while preserving the species abundance distributions in the observed data. 
# See Legendre and De Cáceres (2013) for discussion.

# paper in control are more unique that the paper in the ecological complexity researc
summary(lm(LGLCBD$BDLG.LCBD ~ LGLCBD$SEARCH_TYPE))

# local contribution to beta diversity is substantially higher in control papers
plot3 <- boxplot %>%
  ggplot( aes(x=LGLCBD$SEARCH_TYPE, y=LGLCBD$BDLG.LCBD, fill=table_sp$SEARCH_TYPE)) +
  #geom_violin() +
  scale_fill_manual(values=c("gainsboro", "cyan2")) +
  #geom_jitter(color="black", size=1, alpha=0.9) +
  theme_bw()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  #ggtitle("True diversity of complexity themes in a publication") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.7) +
  geom_boxplot(width=0.1, outlier.shape = NA, alpha = 0.8) +
  #ylim(0,1)+
  ylab("True diversity")+
  xlab("")+ 
  geom_signif(comparisons = list(c("Control", "Ecological complexity")), 
              map_signif_level=TRUE)
plot3



# alternative to plot 3; same information
# plot3 <- ggplot(LGLCBD, aes(SEARCH_TYPE, WOS_ID, color=SEARCH_TYPE, size =BDLG.LCBD, shape=pval)) + 
#   geom_point() +                
#   scale_shape_manual(values=c(1, 19)) +
#   scale_color_manual(values=c("gainsboro", "cyan2")) +
#   theme_bw() +                      
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.text.y = element_blank())+
#   labs(y="Paper ID", x = "Type of search")
# plot3

# local contribution to beta diversity is inversely related to the number of themes tackled in the paper
plot4 <- ggplot(LGLCBD, aes(x=richness, y=BDLG.LCBD, shape=SEARCH_TYPE, color=SEARCH_TYPE)) +
  geom_point(size = 2)+
  theme_bw() +
  scale_color_manual(values=c("gainsboro", "cyan2")) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  labs(y="Local contributions to beta diversity", x = "Number of complexity themes addressed")+
  theme(legend.title = element_blank())

plot4

ggarrange(plot1, plot2, plot3, plot4)


## some additional tests
table_sp <- table_sp  %>% mutate_if(is.numeric, ~1 * (. > 0))




library(ade4)
d <- dist.binary(table_sp[, 2:26], method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels = table_sp$SEARCH_TYPE)    # plot the dendrogram
# little evidence of differences



# playing with something new - copulas; check FYI, it runs
library(ecoCopula)
# https://cran.r-project.org/web/packages/ecoCopula/vignettes/the_basics.html the tutorial is here

# fit marginal model
model <- stackedsdm(table_sp[, 2:26],~ table_sp$SEARCH_TYPE, data = table_sp, family="binomial",ncores = 10) 
model_gr=cgr(model, seed=3)
plot(model_gr, pad=1)


#I looked also at model based ordinations; can look further if you are keen into this
library(gllvm)

model <- gllvm(table_sp[, 2:26], # response
      as.data.frame(table_sp$SEARCH_TYPE), #covariate
      family = "negative.binomial",
      num.lv = 3,
      #formula = ~ "table_sp$SEARCH_TYPE",
      seed = 619)

coefplot(model, cex.ylab = 0.7, mar = c(4, 9, 2, 1),
         xlim.list = list(NULL, NULL, c(-4, 4)), mfrow=c(1,1))


ordiplot(model, biplot = TRUE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3),
         main = "Biplot")
ordiplot(model, biplot = FALSE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3),
         main = "Ordination plot", predict.region = TRUE)