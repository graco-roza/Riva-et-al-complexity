# Article: Towards a cohesive understanding of ecological complexity

#' ------------------------------------------------------------------------
#'R script to reproduce the analyses'
#' ------------------------------------------------------------------------

#' Authors:  Caio Graco-Roza & Stefano Mammola
#' Last update: 11 Apr 2023, Verbania, Italy.
#' Software: R (v. R 4.2.2) 

#'##############################################################

# Loading useful packages -------------------------------------------------
options(device = "RStudioGD")

pacman::p_load(
  tidyverse,
  ggthemes,
  bibliometrix,
  ergm,
  fs,
  ggraph,
  network,
  SemNetCleaner,
  tidygraph,
  rgeos,
  rworldmap,
  igraph,
  magrittr,
  ggmap,
  ggfx,
  qdapRegex,
  useful,
  grid,
  patchwork,
  ggtext,
  janitor,
  ggpubr,
  ggforce,
  showtext,
  rstatix,
  grid,
  ggtext,
  showtext,
  showtextdb,
  vegan,
  colorspace,
  patchwork,
  ggpmisc,
  indicspecies,
  adespatial,
  ggregplot,
  ggdist,
  modelr,
  hacksaw,
  DataEditR,
  rworldmap
)

#register key for using ggmaps
ggmap::register_google('AIzaSyBP7U9mHiqB2q83f9JrNq4yjn4b6rAKhTg')


# Loading fonts --------------------------------------------------------------------------------------------------

  
font_install(showtextdb::google_fonts("Montserrat"))
font_install(showtextdb::google_fonts("Lato"))
font_install(showtextdb::google_fonts("Noto Sans"))
font_add_google("Montserrat", "Montserrat")
font_add_google("Noto Sans","Noto")
font_add_google("Lato", "Lato")
showtext::showtext_auto()


# Set colors ------------------------------------------------------------------------------------------------------

main_color <-"#CA3542" #color for everything that is not related to groups
title_color <- "#0A0B0B" #true black
subtitle_color <- lighten("#0A0B0B",.2) #80% black
cocit_colors <- c( "#d4af37", darken("#27647B",.2), lighten("#CA3542",.5), lighten("#57575F",.5) , "black")#"#AEC0C9"
searchtype_colors<- c("#CA3542", "#57575F")

# Functions -------------------------------------------------------------------------------------------------------

#' Function to extract node traits in a graph 
#' For a discussion:
#' https://medium.com/@615162020004/social-network-analysis-with-r-centrality-measure-86d7fa273574
#' Graph should be a tidygraph dataset

NetworkTraitGet <- function(Graph){
  
  data.frame(
    
    Size = vcount(Graph),
    
    Edge = gsize(Graph),
    
    Diameter = diameter(Graph),
    
    MeanDegree = mean(igraph::degree(Graph)),
    
    DegreeVariance = sd(igraph::degree(Graph)),
    
    Components = igraph::components(Graph)$no,
    
    Transitivity = transitivity(Graph),
    
    Density = ggregplot::Prev(get.adjacency(Graph, sparse = F) > 0),
    
    LouvainModularity = Graph %>% cluster_louvain %>% membership %>% modularity(Graph, .),
    
    RealizedConnectance = ecount(Graph) / ((vcount(Graph)*(vcount(Graph)-1))/2)
    
  ) %>% return
  
}

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

coords2continent <- function(lon,lat) {  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(cbind(lon,lat), proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  return(indices$REGION)   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

roundUp <- function(x,to=100)
{
  to*(x%/%to + as.logical(x%%to))
}

# Loading the data ------------------------------------------------------------------------------------------------

ref_tab   <- readr::read_csv("Database/Table_papers.csv") #database listing papers 
db_graph_original <- read_csv("Database/text_compiled_relative.csv") #database of ngrams by paper
MATRIX_1 <- bibliometrix::convert2df("Database/Search_1.txt", dbsource = "wos", format = "plaintext")
authors_coordinates<- read_csv("Database/coordinates_coauthors.csv") #database listing papers 
   
# Summary statistics  ---------------------------------------------------------------------------------------------

#Sample size
db_graph_original %>%
  left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE)) %>% as_tibble() %>% 
  select(SEARCH_TYPE) %>% table

#papers without features 
zero_sum<- names(which(rowSums(db_graph_original %>% 
                                 column_to_rownames("WOS_ID")) == 0))

#Number of articles with no features
data.frame(WOS_ID = zero_sum) %>% 
  left_join(ref_tab) %>%  
  select(SEARCH_TYPE, WOS_ID)
  

# Collaboration network  ------------------------------------------------------------------------------------------

#'@Data wrangling ----

all_authors_affiliation <- ref_tab %>% 
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
  #We run this to get the address for each author
  # rowwise() %>% 
  # mutate(ggmap::geocode(Addresses)) %>% 
  #then we saved the addresses in `authors_coordinates`
  #and merged it with the code below
  add_column(authors_coordinates) %>%
  distinct(authors_new, WOS_ID, .keep_all = TRUE) %>% 
  mutate(authors_new =   sapply(authors_new, function(x) janitor::make_clean_names(x, case = "all_caps", sep_out=" "))) %>%
  mutate(authors_new = factor(authors_new, levels = unique(authors_new))) %>% 
  mutate(ID = as.numeric(authors_new)) %>% 
  drop_na(lat, lon) 

#'@Make Network ----

network_collaborations <-  all_authors_affiliation %>%  
  select(WOS_ID, authors_new) %>% 
  mutate(value = 1 ) %>% 
  pivot_wider(id_cols = authors_new, names_from = WOS_ID, values_from = value, values_fill = 0, values_fn=sum) %>% 
  column_to_rownames("authors_new") %>% 
  igraph::graph_from_incidence_matrix(multiple = TRUE,
                                      directed = TRUE,
                                      weighted = NULL) %>%
  igraph::bipartite_projection() %>%  #projecting to unipartite
  pluck("proj1") %>% #selecting node type 1
  as_tbl_graph(directed = FALSE)  #convert to table graph object

colab_summary <-network_collaborations %>% 
  NetworkTraitGet()

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
  left_join(all_authors_affiliation %>%  select(lat,lon,authors_new), by = c("from" = "authors_new"), relationship = "many-to-many") %>% 
  left_join(all_authors_affiliation %>%  select(lat,lon,authors_new), by = c("to" = "authors_new"), relationship = "many-to-many") %>% 
  rename_with(.cols = c(lat.x,lon.x,lat.y,lon.y), .fn = ~ c("Lat_from","Long_from","Lat_to","Long_to")) 
  
#@Plot Collaboration map ----

world_map = map_data("world") %>% 
  filter(! long > 180)

collaboration_map <- 
  distinct(world_map, region) %>% 
  ggplot(aes(map_id = region)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") + 
geom_segment(data = named_edge_list,
               aes(y = jitter(Long_from,0.0001), 
                   x = jitter(Lat_from,0.0001), 
                   yend = jitter(Long_to, 0.0001), 
                   xend = jitter(Lat_to, 0.0001),  # draw edges as arcs
                   alpha = weight),
               show.legend = FALSE,
               #curvature = 0.2,
               linewidth=.2,
               color = main_color, inherit.aes=FALSE) +
  geom_point(data = all_authors_affiliation,
             aes(x = lat,
                 y = lon) , size=.5, shape=19, colour=colorspace::darken(main_color,0.1), inherit.aes=FALSE) +
  labs(#title = "&nbsp; Global network of collaborations <br>",
    subtitle = glue::glue("<br><i>N<sub>Nodes</sub></i> = {colab_summary$Size},
                             <i>N<sub>Edges</sub></i> = {colab_summary$Edge},
                             <i>Diameter</i> = {colab_summary$Diameter},
                             <i>Realized connectance</i> = {round(colab_summary$RealizedConnectance,3)}")) +
  #remove the background and default gridlines
  theme(line = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(), #element_rect(fill = "white", color = "white"),
        plot.title = element_markdown(color = title_color, #vjust=-4,
                                      family = "Montserrat", face = "bold",
                                      size = 14, hjust= 0.05),
        plot.subtitle = element_markdown(color = subtitle_color, margin = margin(b=0,unit="pt")),
        axis.text = element_blank(),
        axis.title = element_blank())

#Get the continent of the institutions where authors are affiliated to
all_authors_affiliation %>% 
  mutate(continent = coords2continent(lat,lon)) %>% 
  pull(continent) %>%  table()

# Scientometric analysis ------------------------------------------------------

#'@Time trend ----

#Timetrend table was generated using the script "Generate_timetrend"
annual_production <- "Database/timetrend.csv" %>% 
  readr::read_csv()

complex<- annual_production %>%  
  group_split(group) %>% 
  pluck(1) %>% 
  pull(N) %>% 
  seq_range(n=5, pretty = FALSE) %>% 
  roundUp() %>% 
  as.integer()

ecol_complex<- annual_production %>%  
  group_split(group) %>% 
  pluck(2) %>% 
  pull(N) %>% 
  seq_range(n=5, pretty = TRUE) %>% 
  as.integer()

year_data <- annual_production %>%
  arrange(year) %>% 
  group_by(group)

year_data_log <- year_data |> 
  mutate(Articles = log(N+1,10)) 


plot_year_log <- year_data_log |> 
  ggplot(aes(y=Articles,x=year, colour=group)) + 
  geom_line(size=2) +
  scale_y_continuous(breaks=c(1:5,5.8)) +
  scale_colour_manual(values=rev(searchtype_colors))  + 
  geom_text(data=data.frame(), aes(x=2020.5,y=5.8,label = "Complexity"),colour=searchtype_colors[2],hjust=0,family = "Lato")+
  geom_text(data=data.frame(), aes(x=2020.5,y=3.2,label = "Ecological Complexity"),colour=searchtype_colors[1],hjust=0,family = "Lato")+
  labs(y="Number of Articles Log10(x+1)", x = "Publication year", colour="")+
  theme(
    plot.margin = margin(0, 0, 0, 5),
    axis.text = element_text(colour = subtitle_color, size = 10),
    axis.title = element_text(
      colour = subtitle_color,
      size = 14,
      face = "bold",
    ),
    plot.title = element_markdown(
      lineheight = 1.5,
      hjust = 0,
      color = title_color,
      # vjust = -2,
      family = "Montserrat",
      face = "bold",
      size = 14
    ),
    # plot.title.position = "plot",
    legend.position = "none",
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor =  element_blank(),
    axis.text.x = element_text(colour = subtitle_color, size = 10, family = "Lato"),
    axis.text.y = element_text(colour = subtitle_color, size = 10, family = "Lato")
  ) +
  scale_x_continuous(breaks=seq(1970,2020,by=10), limits = c(1970,2030))

ggsave(plot = plot_year_log, filename="Figures/Figure_S1.pdf", device=cairo_pdf)


plot_year <- annual_production %>%
    arrange(year) %>% 
    group_by(group) %>% 
    mutate(Articles = (N - min(N)) / (max(N) - min(N))) %>% 
    ggplot(aes(x = year, y = Articles)) +
    geom_line(aes(colour= group), size = 2, show.legend = FALSE) +
    scale_colour_manual(values=searchtype_colors) +
    scale_x_continuous(limits=c(1966,2040)) +
         annotate(
      "text",
      x = rep(1967, 5),
      y = sapply(ecol_complex, 
                function(i) {(i - min(ecol_complex))/
                                (max(ecol_complex)-min(ecol_complex))}) ,
      label = paste0(ecol_complex,c(rep("",4)," papers")),
      colour =  searchtype_colors[1],
      fontface="bold",
      size = 5,
      family = "Lato",
      hjust=0
    ) +
    annotate(
      "text",
      x = rep(2023, 5),
      y = sapply(complex, 
                 function(i) {(i - min(complex))/
                     (max(complex)-min(complex))}) ,
      label = paste0(complex,c(rep("",4)," papers")),
      colour =  searchtype_colors[2],
      fontface="bold",
      size = 5,
      family = "Lato",
      hjust=0
    ) +
    annotate(
      "segment",
      x = 1967.5,
      xend = 1967.5,
      y = sapply(ecol_complex[-5], #discards last value
                 function(i) {(i - min(ecol_complex))/
                     (max(ecol_complex)-min(ecol_complex))}) + .05
        ,
      yend = sapply(ecol_complex[-1], #discards first value 
                    function(i) {(i - min(ecol_complex))/
                        (max(ecol_complex)-min(ecol_complex))}) - .05,
      colour =   searchtype_colors[1],
      size = .7,
      arrow = arrow(
        ends = "both",
        angle = 90,
        length = unit(.1, "cm")
      )
    ) +
    annotate(
      "segment",
      x = 2023.5,
      xend = 2023.5,
      y = sapply(complex[-5], 
                 function(i) {(i - min(complex))/
                     (max(complex)-min(complex))}) +.05,
      yend = sapply(complex[-1], 
                    function(i) {(i - min(complex))/
                        (max(complex)-min(complex))}) - .05,
      colour =   searchtype_colors[2],
      size = .7,
      arrow = arrow(
        ends = "both",
        angle = 90,
        length = unit(.1, "cm")
      )
    ) +
    #Make x-axis
    annotate(
      "segment",
      x = 1974,
      xend = 2019,
      y = -.1,
      yend = -.1,
      colour =   subtitle_color,
      size = 0.5,
      arrow = arrow(length = unit(.3, "cm"))
    ) +
    annotate(
      "text",
      y = -.1,
      x = 2021,
      label = "2021",
      family = "Lato",
      colour =  subtitle_color,
      size = 5,
      hjust = 0
    ) +
    annotate(
      "text",
      y = -.1,
      x = 1970,
      label = "1970",
      family = "Lato",
      colour =  subtitle_color,
      size = 5,
      hjust = .5
    ) +
    labs(
      y = "",
      x = "",
     # title = "Cummulative production of  <br> <span style = 'color:#57575F;'>Complexity</span> and \n <span style = 'color:#CA3542;'>Ecological complexity</span> papers",
      size = 16
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 5),
      axis.text = element_text(colour = subtitle_color, size = 10),
      axis.title = element_text(
        colour = subtitle_color,
        size = 14,
        face = "bold",
      ),
      plot.title = element_markdown(
        lineheight = 1.5,
        hjust = 0,
        color = title_color,
       # vjust = -2,
        family = "Montserrat",
        face = "bold",
        size = 14
      ),
     # plot.title.position = "plot",
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor =  element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    coord_cartesian(clip = "off", expand=TRUE)

Figure_2  <- collaboration_map + 
plot_year +  plot_annotation(tag_levels="a", tag_suffix = ")") & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 14, hjust = 0, vjust=0,  face="bold")) 

ggsave(filename = "Figures/Figure_2.pdf", Figure_2, device= cairo_pdf(), width = 16, height =6, units= "in")


#'------------------------------------------------------------------------------------------
# Alpha and Beta diversity analysis ########################################################
#'------------------------------------------------------------------------------------------

# Add Search type to table
data_beta <- db_graph_original %>%  
  left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE), by = "WOS_ID") %>% 
  filter(SEARCH_TYPE != "Complex system science") # We did not analyses CSS papers

# main table for analysis: retain only features (species) and type of research
table_sp <- data_beta %>%
  select(-WOS_ID) %>%
  relocate(SEARCH_TYPE) 

table_sp <- table_sp[rowSums(table_sp[-1]) > 0,]

# @ Alpha Diversity ------------------------------------------------------------------------------------------------

richness <- vegan::specnumber(table_sp[,-1])  
div <- exp(vegan::diversity(table_sp[,-1])) # exp of Shannon entropy, Hill number of order 1; 


# see http://www.loujost.com/Statistics%20and%20Physics/Diversity%20and%20Similarity/EffectiveNumberOfSpecies.htm#:~:text=The%20number%20of%20equally%2Dcommon%20species%20required%20to%20give%20a,4.5)%20%3D%2090%20effective%20species. 
# Shannon entropy as a golden number for ecologists (MacArthur 1965; https://onlinelibrary.wiley.com/doi/10.1111/j.1469-185X.1965.tb00815.x)

alpha <- data.frame(richness, div, group = table_sp$SEARCH_TYPE)

alpha_iqr <- alpha %>%
  group_by(group) %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = list(
      q25 = ~ quantile(.x, .25),
      median = median,
      q75 = ~ quantile(.x, .75),
      max = max,
      sd = sd
    )
  )) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(group_number = as.numeric(fct_rev(group)))

richness_rect <-
  tibble(
    xmin = c(-Inf, 13.1),
    xmax = c(Inf, Inf),
    ymin = c(2, 1),
    ymax = c(Inf, Inf)
  )


div_rect <-
  tibble(
    xmin = c(-Inf, 9.9),
    xmax = c(Inf, Inf),
    ymin = c(2, 1),
    ymax = c(Inf, Inf)
  )

plot_richness <-
  alpha_iqr %>%
  ggplot(aes(x = richness, y = group_number - .25)) +
  geom_rect(
    data = richness_rect,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = F,
    fill = "white"
  ) +
  geom_linerange(
    data = alpha_iqr %>%
      group_by(group, group_number) %>%
      summarize(m = unique(richness_median)),
    aes(
      xmin = -Inf,
      xmax = m,
      y = group_number,
      color = group
    ),
    inherit.aes = F,
    linetype = "dotted",
    size = .7
  ) +
  geom_boxplot(aes(color = group,
                   color = after_scale(darken(color, .1, space = "HLS"))),
               width = 0,
               size = .9) +
  geom_rect(
    aes(
      xmin = richness_q25,
      xmax = richness_median,
      ymin = group_number - .1,
      ymax = group_number - .4,
      fill = group,
      fill = after_scale(lighten(fill, .7, space = "HLS"))
    )
  ) +
  geom_rect(
    aes(
      xmin = richness_q75,
      xmax = richness_median,
      ymin = group_number - .1,
      ymax = group_number - .4,
      fill = group,
      fill = after_scale(lighten(fill, .5, space = "HLS"))
    )
  ) +
  geom_segment(
    aes(
      x = richness_q25,
      xend = richness_q25,
      y = group_number - .1,
      yend = group_number - .4,
      color = group,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    linewidth = .25
  ) +
  geom_segment(
    aes(
      x = richness_q75,
      xend = richness_q75,
      y = group_number - .1,
      yend = group_number - .4,
      color = group,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    linewidth = .25
  ) +
  geom_point(aes(color = group),
             shape = "|",
             size = 3,
             alpha = .3) +
  ggdist::stat_halfeye(
    aes(
      y = group_number,
      color = group,
      fill = after_scale(lighten(color, .7))
    ),
    shape = 19,
    point_size = 2,
    interval_size = 1.8,
    adjust = 1,
    .width = c(0, 1),
    height = .6
  ) +
  geom_text(
    data = alpha_iqr %>%
      group_by(group, group_number) %>%
      summarise(m = unique(richness_median)),
    aes(
      x = m,
      y = group_number + .15,
      label = format(round(m, 2), nsmall = 2),
      colour = group
    ),
    inherit.aes = F,
    family = "Lato",
    size = 5,
    fontface = "bold"
  ) +
  geom_text(
    data = alpha_iqr %>%
      group_by(group, group_number) %>%
      distinct(n, richness_max),
    aes(
      x = richness_max * 1.01,
      y = group_number * 1.02,
      label = glue::glue("n = {n}"),
      color = group
    ),
    inherit.aes = F,
    family = "Lato",
    size = 5,
    hjust = 0
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    limits = c(0, 22),
    breaks = seq(2, 18, by = 2),
    expand = c(.001, .001)
  ) +
  scale_y_continuous(
    limits = c(.55, 2.6),
    breaks = 1:2,
    labels = rev(c("Control", "Ecological\ncomplexity")),
    expand = c(0, 0)
  ) +
  scale_color_manual(values = rev(searchtype_colors),
                     guide = "none") +
  scale_fill_manual(values = rev(searchtype_colors),
                    guide = "none") +
  labs(
    x = "Number of features addressed by the study",
    y = "",
    #title = "Richness of features per study",
    subtitle = get_test_label(alpha_iqr %>%  anova_test(richness ~
                                                          group), detailed = TRUE)
  ) +
  theme_minimal(base_size = 15, base_family = "Montserrat") +
  theme(
    panel.grid.major = element_line(color = "grey92", size = .4),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(color = subtitle_color, size = 12),
    axis.title.y = element_text(color = subtitle_color),
    axis.text = element_text(color = subtitle_color),
    axis.ticks =  element_line(color = "grey92", size = .4),
    axis.ticks.length = unit(.6, "lines"),
    legend.position = "top",
    plot.title = element_text(
      hjust = 0,
      color = title_color,
      vjust = -1,
      family = "Montserrat",
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      hjust = 0,
      color = "grey30",
      family = "Noto",
      size = 14
    ),
    plot.title.position = "plot"
  )

   #summary of alpha diversity across groups (Richness)
   alpha_iqr %>%  distinct(group, .keep_all = TRUE) %>%  select(group,richness_median, richness_sd)

   plot_div <-
     alpha_iqr %>%
     ggplot(aes(x = div, y = group_number - .25)) +
     geom_rect(
       data = div_rect,
       aes(
         xmin = xmin,
         xmax = xmax,
         ymin = ymin,
         ymax = ymax
       ),
       inherit.aes = F,
       fill = "white"
     ) +
     geom_linerange(
       data = alpha_iqr %>%
         group_by(group, group_number) %>%
         summarize(m = unique(div_median)),
       aes(
         xmin = -Inf,
         xmax = m,
         y = group_number,
         color = group
       ),
       inherit.aes = F,
       linetype = "dotted",
       size = .7
     ) +
     geom_boxplot(aes(color = group,
                      color = after_scale(darken(color, .1, space = "HLS"))),
                  width = 0,
                  size = .9) +
     geom_rect(
       aes(
         xmin = div_q25,
         xmax = div_median,
         ymin = group_number - .1,
         ymax = group_number - .4,
         fill = group,
         fill = after_scale(lighten(fill, .7, space = "HLS"))
       )
     ) +
     geom_rect(
       aes(
         xmin = div_q75,
         xmax = div_median,
         ymin = group_number - .1,
         ymax = group_number - .4,
         fill = group,
         fill = after_scale(lighten(fill, .5, space = "HLS"))
       )
     ) +
     geom_segment(
       aes(
         x = div_q25,
         xend = div_q25,
         y = group_number - .1,
         yend = group_number - .4,
         color = group,
         color = after_scale(darken(color, .05, space = "HLS"))
       ),
       size = .25
     ) +
     geom_segment(
       aes(
         x = div_q75,
         xend = div_q75,
         y = group_number - .1,
         yend = group_number - .4,
         color = group,
         color = after_scale(darken(color, .05, space = "HLS"))
       ),
       size = .25
     ) +
     geom_point(aes(color = group),
                shape = "|",
                size = 3,
                alpha = .3) +
     ggdist::stat_halfeye(
       aes(
         y = group_number,
         color = group,
         fill = after_scale(lighten(color, .7))
       ),
       shape = 19,
       point_size = 2,
       interval_size = 1.8,
       adjust = 1,
       .width = c(0, 1),
       height = .6,
     ) +
     geom_text(
       data = alpha_iqr %>%
         group_by(group, group_number) %>%
         distinct(n, div_max),
       aes(
         x = div_max * 1.01,
         y = group_number * 1.02,
         label = glue::glue("n = {n}"),
         color = group
       ),
       inherit.aes = F,
       family = "Lato",
       size = 5,
       hjust = 0
     ) +
     geom_text(
       data = alpha_iqr %>%
         group_by(group, group_number) %>%
         summarise(m = unique(div_median)),
       aes(
         x = m,
         y = group_number + .15,
         label = format(round(m, 2), nsmall = 2),
         colour = group
       ),
       inherit.aes = F,
       family = "Lato",
       size = 5,
       fontface = "bold"
     ) +
     coord_cartesian(clip = "off") +
     scale_x_continuous(
       limits = c(0,17),
       breaks = seq(2, 12, by = 2),
       expand = c(.001, .001),
       labels = seq(2, 12, by = 2)
     ) +
     scale_y_continuous(
       limits = c(.55, NA),
       breaks = 1:2,
       labels = rev(c("Control", "Ecological\ncomplexity")),
       expand = c(0, 0)
     ) +
     scale_color_manual(values = rev(searchtype_colors),
                        guide = "none") +
     scale_fill_manual(values = rev(searchtype_colors),
                       guide = "none") +
     labs(
       x = "Exponential Shannon-wiener diversity index",
       y = "",
       #titles= "True diversity of features per study",
       subtitle = get_test_label(alpha_iqr %>%  anova_test(div ~ group), detailed = TRUE)
     ) +
     theme_minimal(base_size = 15, base_family = "Montserrat") +
     theme(
       panel.grid.major = element_line(color = "grey92", size = .4),
       panel.grid.minor = element_blank(),
       panel.grid.major.y = element_blank(),
       axis.title.x = element_text(color = subtitle_color, size = 12),
       axis.title.y = element_text(color = subtitle_color),
       axis.text = element_text(color = subtitle_color),
       axis.ticks =  element_line(color = "grey92", size = .4),
       axis.ticks.length = unit(.6, "lines"),
       legend.position = "top",
       plot.title = element_text(
         hjust = 0,
         color = title_color,
         vjust = -1,
         family = "Montserrat",
         face = "bold",
         size = 14
       ),
       plot.subtitle = element_text(
         hjust = 0,
         color = "grey30",
         family = "Noto",
         size = 14
       ),
       plot.title.position = "plot"
     )

#@ Beta diversity -------------------------------------------------------------------------------------------------

# Convert into presence/absence matrix
table_pa <- table_sp %>%  select(-"SEARCH_TYPE") %>%  mutate(across(everything(),.fns = ~ ifelse(.x > 0,1,0)))

betadiv <- betadisper(vegdist(table_sp %>%
                                select(-SEARCH_TYPE)),
                      table_sp %>%  pull(SEARCH_TYPE), type="median") %>% 
  keep(names(.) %in% c("distances","groups")) %>% 
  bind_cols() %>% 
  add_column(richness,div, group = table_sp %>% pull(SEARCH_TYPE))


perm_test<- betadisper(vegdist(table_sp %>%  select(-SEARCH_TYPE)),table_sp$SEARCH_TYPE, type="median") 

data.frame(dist = perm_test$distances, group = perm_test$group) %>% 
  group_by(group) %>% 
  summarise(sd = sd(dist))

# paper in control are more unique than the papers in the ecological complexity research

beta_iqr <- betadiv %>%
  group_by(group) %>%
  select(-c("richness","div")) %>% 
  mutate(across(
    .cols = where(is.numeric),
    .fns = list(
      q25 = ~ quantile(.x, .25),
      median = median,
      q75 = ~ quantile(.x, .75),
      max = max,
      min= min
    )
  )) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(group_number = as.numeric(fct_rev(group)))

  dist_rect <-
  tibble(
    xmin = c(-Inf, 0.314),
    xmax = c(Inf, Inf),
    ymin = c(2, 1),
    ymax = c(Inf, Inf)
  )

  dist_plot <- 
  beta_iqr %>%
  ggplot(aes(x= distances, y = group_number-.25)) +
  geom_rect(
    data = dist_rect,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax
    ),
    inherit.aes = F,
    fill = "white"
  ) +
  geom_linerange(
    data = beta_iqr %>% 
      group_by(group, group_number) %>% 
      summarize(m = unique(distances_median)),
    aes(
      xmin = -Inf, 
      xmax = m, 
      y = group_number,
      color = group
    ),
    inherit.aes = F,
    linetype = "dotted",
    size = .7
  ) +
  geom_boxplot(
    aes(
      color = group,
      color = after_scale(darken(color, .1, space = "HLS"))
    ),
    width = 0,
    size = .9
  ) + 
  geom_rect(
    aes(
      xmin = distances_q25,
      xmax = distances_median,
      ymin = group_number - .1,
      ymax = group_number - .4,
      fill = group,
      fill = after_scale(lighten(fill, .7, space = "HLS"))
    )
  ) +
  geom_rect(
    aes(
      xmin = distances_q75,
      xmax = distances_median,
      ymin = group_number - .1,
      ymax = group_number - .4,
      fill = group,
      fill = after_scale(lighten(fill, .5, space = "HLS"))
    )
  )+
  geom_segment(
    aes(
      x = distances_q25, 
      xend = distances_q25,
      y = group_number - .1,
      yend = group_number - .4,
      color = group,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    size = .25
  ) +
  geom_segment(
    aes(
      x = distances_q75, 
      xend = distances_q75,
      y = group_number - .1,
      yend = group_number - .4,
      color = group,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    size = .25
  ) +
  geom_point(
    aes(
      color = group
    ), 
    shape="|",
    size = 3,
    alpha=.3
  ) +
  ggdist::stat_halfeye(
    aes(
      y = group_number,
      color = group,
      fill = after_scale(lighten(color, .7))
    ),
    shape = 19,
    point_size = 2,
    interval_size = 1.8,
    adjust = 1,
    .width = c(0, 1),
    height=.6
  ) +
  geom_text(
    data = beta_iqr %>% 
      group_by(group, group_number) %>% 
      summarise(m = unique(distances_median)),
    aes(
      x = m, 
      y = group_number + .15,
      label = format(round(m, 2), nsmall = 2),
      colour = group
    ),
    inherit.aes = F,
    family = "Lato",
    size = 5, 
    fontface="bold"
  ) +
  geom_text(
    data = beta_iqr %>%
      group_by(group, group_number) %>%
      distinct(n, distances_max),
    aes(
      x = distances_max * 1.01,
      y = group_number * 1.02,
      label = glue::glue("n = {n}"),
      color = group
    ),
    inherit.aes = F,
    family = "Lato",
    size = 5,
    hjust = 0
  ) +
  scale_x_continuous(
    limits = c(NA, 1),
   breaks = seq(0.2, .8, by = 0.1),
    expand = c(.1, .1),
   name = "Mean distance to the group median"
  ) +
  scale_y_continuous(
    limits = c(.55, 2.6),
    breaks = 1:2,
    labels = rev(c("Control", "Ecological\ncomplexity")),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = rev(searchtype_colors),
    guide = "none"
  ) +
  scale_fill_manual(
    values = rev(searchtype_colors),
    guide = "none"
  ) + 
  labs(
       y = "",
       #title = " Study uniqueness based on features",
       subtitle = get_test_label(beta_iqr %>%  anova_test(distances~group), detailed = TRUE)) +
  theme_minimal(base_size = 15, base_family = "Montserrat") +
  theme(
    panel.grid.major = element_line(color = "grey92", size = .4),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_markdown(size=12),
    axis.title.y = element_text(color = subtitle_color),
    axis.text = element_text(color = subtitle_color),
    axis.ticks =  element_line(color = "grey92", size = .4),
    axis.ticks.length = unit(.6, "lines"),
    legend.position = "top",
    plot.title = element_text(hjust = 0, color = title_color, vjust=-1, 
                              family = "Montserrat", face = "bold",
                              size = 14),
    plot.subtitle = element_text(hjust = 0, color = "grey30",
                                 family = "Noto", 
                                 size = 14),
    plot.title.position = "plot"
  ) +
  coord_cartesian(clip = "off") 
  
fit_formula <- y~poly(x,2)

dist_richness <- betadiv %>% 
  mutate(dist = scale(distances)) %>% 
  ggplot(aes(x=richness, y=dist, color=group)) +
  geom_point(size = 2, shape=19, alpha=.7)+
  scale_color_manual(values=rev(searchtype_colors)) +
  scale_fill_manual(values=rev(searchtype_colors)) +
    geom_ribbon(stat='smooth', aes(fill=group), colour=NA, method = "lm", alpha=0.1, formula=fit_formula) +
    geom_line(stat='smooth', method = "glm", formula=fit_formula, size=2)+
  stat_poly_eq(formula = fit_formula,
               aes(label =  tolower(gsub("italic\\(y\\)","hat(y)",paste(after_stat(eq.label),"'; '",
                                 "italic(p)~' <0.001 '",
                                 "'; '",
                                 gsub("italic","plain",after_stat(rr.label)), sep="~")))
                   ),label.y='top',
               label.x='right', 
               coef.digits = 4,
               vstep=.08,
               size=4,
               parse = TRUE)+
  labs(y="Mean distance to<br> the group median (<span style='font-family:Noto;'>&#x0240;</span>-score)",
       x = "Number of features addressed by the study")+
  scale_x_continuous(
    limits = c(0, 22),
    breaks = seq(2, 18, by = 2),
    expand = c(.001, .001)
    ) +
  scale_y_continuous(
    limits = c(-2.5, 4),
    breaks = seq(-3, 3, by = 1),
    expand = c(.01, .01)
  ) +
  theme_minimal(base_size = 15, base_family = "Montserrat") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_markdown(color = subtitle_color, size=12),
    axis.title.x = element_text(color = subtitle_color, size=12),
    axis.text = element_text(color = subtitle_color),
    axis.ticks =  element_line(color = "grey92", size = .4),
    axis.ticks.length = unit(.6, "lines"),
    legend.position = "none",
    plot.title = element_text(hjust = 0, color = title_color, vjust=-1, 
                              family = "Montserrat", face = "bold",
                              size = 14),
    plot.subtitle = element_text(hjust = 0, color = "grey30",
                                 family = "Noto", 
                                 size = 14),
    plot.title.position = "plot"
  )

summary_plots <- ((plot_richness/plot_div) |(wrap_elements(full = dist_plot) / wrap_elements(full = dist_richness))) +
  plot_annotation(tag_levels="a", tag_suffix = ")") & 
  theme(title = element_blank(),
    plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 15, hjust = 1, vjust = 0, face="bold"))


ggsave(filename="Figures/Figure_3.pdf", plot = summary_plots, device=cairo_pdf(), height=7, width=12.5)



# Network of features -----------------------------------------------------------------------------------------------

# indicator words: are there features typical of complexity papers?
ind <- indicspecies::multipatt(table_sp %>%  select(-"SEARCH_TYPE"), 
                               cluster = table_sp %>%  pull(SEARCH_TYPE), 
                               func = "r.g", 
                               control = how(nperm=999))
themes_sig<- ind %>% 
  pluck("sign") %>% 
  rownames_to_column("themes") %>% 
  select(themes,p.value)



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
      mutate(across(everything(), ~ ifelse(.x > 0 , 1, 0))) %>% #convert abundance of words into occurrence
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


theme_summary <- network_words %>%  NetworkTraitGet()

#Adjustment to positioning words in the plot.
nudged<-c(-.3, #diversity
  -.35, #interaction
  .35, #aggreg
  .45, #self_org
  .35, #adapt
  .4, #non_lin
 -.3, #hierarchy
  -.3, # mod
 .35, #emergence
  .3, #scaling
  .3, #fractality
  .2, #flow
  .4, #dinamic
  .3, #memory
  .35, #feedback 
  .45, #Non equilibrium
  .3, #Attractor
  .35, #homeostasis
  .3, #stability 
  -.35, #resilience
  -.35, #threshold
  -.3,  #network
  .25 #chaos
)


#@Oficial Figure ----
{
  set.seed(202022)
 ( figure_4 <-
    network_words %>%
    activate("nodes") %>%
    mutate(id = as.numeric(fct_reorder(name, desc(
      N
    )))) %>%
    left_join(themes_sig, by = c("name" = "themes")) %>%
    mutate(name = str_replace(name, "scale_dependency", "scaling")) %>% 
    create_layout(layout = 'igraph', algorithm = 'dh') %>%
    ggraph() +
    geom_edge_arc(
      aes(color = weight),
      strength = .2,
      width = .5,
      alpha = .7
    ) +
    geom_edge_arc(
      aes(filter=weight >= 0.5, color=weight),
      strength = .2,
      width = .5,
      alpha = .7
    ) +
    geom_node_point(
      aes(
        size = N,
        fill = Strength,
        fill = after_scale(colorspace::lighten(fill, .1, space = "HLS"))
#        colour = after_scale(colorspace::darken(fill, .5, space = "HLS"))
      ),
colour="black",
      shape = 21
    ) +
    geom_node_text(
      aes(label = stringr::str_to_sentence(gsub("_"," ", name)),
          colour = p.value >= 0.05),
      size = 5,
      fontface = ifelse(themes_sig$p.value < 0.05, "bold", "plain"),
      repel=FALSE,
      nudge_x= nudged 
    ) +
    guides(
      fill = guide_colorbar(
        title.position = 'top',
      label.position = 'bottom',
        order = 2
      ),
      edge_color = guide_edge_colorbar(
        title.position = 'top',
        label.position = 'bottom',
        order = 3
      ),
      size = guide_legend(
        title.position = 'top',
        label.position = 'bottom',
        order = 1,
        override.aes = list(fill = title_color)
      )
    ) + 
     scale_fill_gradient(low = "white", high = "black")+
     scale_color_manual(
       values = c(searchtype_colors),
       guide = "none"
     ) +
     scale_edge_color_gradient("Co-occurrence strength",
                               low = "#ede5cf",
                               high = "#541f3f") +
     scale_size_continuous(breaks = c(9, 64, 150), range = c(8, 15)) +
    labs(size = "Occurrence in papers", 
         fill = "Feature importance",
         #title = "The most addressed features of ecological complexity",
         subtitle=glue::glue("<i>N<sub>Nodes</sub></i> = {theme_summary$Size},
                             <i>N<sub>Edges</sub></i> = {theme_summary$Edge},
                             <i>Diameter</i> = {round(theme_summary$Diameter,4)},
                             <i>Realized connectance</i> = {round(theme_summary$RealizedConnectance,3)}")
         ) +
     theme(line = element_blank(),
                        plot.title = element_text(size = 20, vjust=0, color = title_color, face="bold"),
                        plot.subtitle = element_markdown(
                          size = 14,
                          colour = subtitle_color,
                          vjust=2
                        ),
                        panel.background = element_blank(),
                        plot.background = element_blank(),
                        legend.title = element_text(colour = title_color, family="Montserrat", size=14, face="bold"),
                        legend.text = element_text(colour = subtitle_color, family = "Lato", size=12),
                        legend.key = element_rect(colour = NA, fill = NA),
                 legend.box.just = "top",
                 legend.position="bottom",
                 legend.title.align = 0.5,
                 legend.justification = "center",
                 legend.box = "horizontal",
                 legend.direction = "horizontal",
                 legend.key.width = unit(1.5,"cm"))+
     scale_x_continuous(expand = c(0, 0.5))+
     coord_cartesian(clip="off")
    )
}


ggsave("Figures/Figure_4.pdf", plot = figure_4, width=13,height=8, device=cairo_pdf())


adj_freatures<- igraph::as_adjacency_matrix( network_words %>%
                                               activate("nodes") %>%
                                               mutate(id = as.numeric(fct_reorder(name, desc(
                                                 N
                                               )))) %>%
                                               left_join(themes_sig, by = c("name" = "themes")), attr = "weight") |> 
  as.matrix() |>
  as.data.frame.table() |> 
  mutate(Freq = ifelse(Freq == 0, NA, Freq)) |> 
  left_join(themes_sig, by = c("Var1" = "themes")) |> 
  left_join(themes_sig, by = c("Var2" = "themes")) |> 
  mutate(Var1 = gsub(" ","",Var1)) |> 
  mutate(
    Var1_colour = glue::glue("<span style='color:{ifelse(p.value.x < 0.05,'#CA3542', '#57575F')};'>{Var1}</span>"),
    Var2_colour = glue::glue("<span style='color:{ifelse(p.value.y < 0.05,'#CA3542', '#57575F')};'>{Var2}</span>"),
    sig.x = p.value.x < 0.05,
    sig.y = p.value.y < 0.05
  ) |> 
  mutate(Var1_colour = fct_reorder2(Var1_colour, Var1, sig.x),
         Var2_colour = fct_reorder2(Var2_colour, Var2, sig.y)) 

plot_adj_features <- adj_freatures |> 
  ggplot(aes(x=Var2_colour,y=Var1_colour,fill=Freq)) + 
  geom_tile() + 
  scale_fill_gradient(low = "#ede5cf",
                      high = "#541f3f", na.value=NA)+
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_markdown(size=15),
        axis.text.x = element_markdown(angle=90, hjust=1,size=15),
        panel.grid = element_blank()) + 
  scale_x_discrete(limits = rev(levels(adj_freatures$Var2_colour))) + 
  labs(x="",y="", fill = "Weight")

ggsave("Figures/Figure_S3.pdf", plot_adj_features)


# Co-citation network -------------------------------------------------------------

#@Data wrangling -----

eco_complex <- ref_tab %>%
  filter(SEARCH_TYPE == "Ecological complexity") %>% 
  arrange(WOS_ID)

list_of_papers<- eco_complex %>% 
  filter(WOS_ID %in% gsub(":","_",MATRIX_1$UT)) %>% 
  pull("WOS_ID")

M = MATRIX_1 %>% 
  mutate(UT = gsub(":","_",UT)) %>% 
  filter(UT %in% list_of_papers) %>% 
  arrange(UT) 

{
  M$CR <- gsub("DOI;", "DOI ", as.character(M$CR))
  Fi <- strsplit(M[, "CR"], ";")
  Fi <- lapply(Fi, trim.leading)
  Fi <- lapply(Fi, function(l) l <- l[nchar(l) > 10])
  allField <- unlist(Fi)
  allField <- allField[!is.na(allField)]
  S <- gsub("\\).*", ")", allField)
  S <- gsub(",", " ", S)
  S <- gsub(";", " ", S)
  S <- bibliometrix:::reduceRefs(S)
  allField <- trimES(S)
  Fi <- lapply(Fi, function(l) {
    l <- gsub("\\).*", ")", l)
    l <- gsub(",", " ", l)
    l <- gsub(";", " ", l)
    l <- l[nchar(l) > 0]
    l <- bibliometrix:::reduceRefs(l)
    l <- trimES(l)
    return(l)
  })
  }

refs<-lapply(Fi, function(x) data.frame(reference = x))
names(refs) <- list_of_papers
refs_by_study <- bind_rows(refs, .id="ID")


data_cocit<- refs_by_study %>%  
  left_join(eco_complex, by = c("ID" = "WOS_ID"))


#@Generating the matrix ----

NetMatrix_cocit <-
  M %>% 
  biblioNetwork(
    analysis = "co-citation",
    network = "references",
    sep = ";",
    shortlabel=TRUE)

cocit_list_df<- 
  NetMatrix_cocit %>% 
  as.matrix %>%
  as.dist %>%
  as.matrix %>%
  reshape2::melt() %>% 
  filter(value != 0)

number_of_cocit <- 
  cocit_list_df %>% 
  arrange(desc(value)) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  filter(value != 0) %>% 
  mutate(grp = paste0(pmin(Var1,Var2), pmax(Var1, Var2))) %>% 
  distinct(grp, .keep_all=TRUE) %>% 
  slice(1:136) %>% 
  ungroup() 


top_cocited <- unique(c(number_of_cocit$Var1, number_of_cocit$Var2)) #Top 100 most co-cited papers

number_of_appearance <- 
  cocit_list_df %>% 
  pull(Var1) %>%
  table() %>%  
  data.frame() %>% 
  rename("Papers" = ".", "Frequency" = "Freq")

co_citation_data <- 
  cocit_list_df %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  mutate(grp = paste0(pmin(Var1,Var2), pmax(Var1, Var2))) %>% 
  distinct(grp, .keep_all=TRUE) %>%
  igraph::graph_from_data_frame(vertices = number_of_appearance, directed = FALSE) 


vertices_del<- 
  co_citation_data %>% 
  as_tbl_graph %N>% 
  filter(!name %in%  top_cocited) %>%  
  data.frame() %>%  
  pull(name)

final_cocit<- 
  co_citation_data %>% 
  delete_vertices(vertices_del)


{
  set.seed(570)
clustered_co_citation <- 
  final_cocit %>% 
  cluster_louvain
}

ref_groups<- data.frame(ref=clustered_co_citation$names,group= clustered_co_citation$membership) %>% 
  group_split(group) 

cocit_summary <- final_cocit %>% 
  NetworkTraitGet()

#Number of times each paper (name) is cited (Freq), along with its cluster ID (group)
final_cocit %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  left_join(clustered_co_citation %>% 
              pluck(membership) %>% 
              magrittr::set_class("numeric") %>% 
              data.frame %>% 
              rename("group" = ".") %>% 
              rownames_to_column("name"), by="name") %>% 
  mutate(group = factor(group)) %>% 
  data.frame() %>% 
  filter(group == 1) %>% 
  arrange(Frequency)

{
set.seed(1)
cocit_fig <- final_cocit %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  left_join(clustered_co_citation %>% 
              pluck(membership) %>% 
              magrittr::set_class("numeric") %>% 
              data.frame %>% 
              rename("group" = ".") %>% 
              rownames_to_column("name"), by="name") %>% 
  mutate(group = factor(group)) %>% 
  ggraph::ggraph(., layout = "kk") +
  geom_edge_arc(aes(alpha = value),
                strength = .2,
                colour=colorspace::lighten("#541f3f",.3)) +
  geom_node_point(aes(size = Frequency,
                      fill = group,
                      fill = after_scale(colorspace::lighten(fill, .3, space = "HLS")),
                      colour = after_scale(colorspace::darken(fill, .1, space = "HLS"))),
                  shape = 21) +
  scale_fill_manual("Schools of thought (Groups)",
    values = cocit_colors
  ) +
  scale_edge_alpha_continuous("Number of co-occurrences\nbetween references (Weight)", range = c(0.2, .8)) +
  scale_size_continuous(breaks =c(60,300,600), range = c(3, 15)) +
  labs(size = "Overall number of co-citations") +
  theme_void()+
  theme(
    line = element_blank(),
    plot.title = element_text(size = 32, vjust=-2, color = title_color, face="bold"),
    plot.subtitle = element_markdown(
      size = 20,
      colour = subtitle_color,
      vjust=2
    ),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position = c(.01,.85),
    legend.title = element_text(colour = title_color, family="Montserrat", size=14, face="bold"),
    legend.text = element_text(colour = subtitle_color, family = "Lato", size=14),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.box.just = "left",
    legend.title.align = 0,
    legend.justification = "left",
    legend.box = "vertical",
    legend.direction = "horizontal"
  ) +
  guides(
    colour = "none",
    edge_alpha = guide_legend(
      title.position = 'top',
      label.position = 'bottom',
      order = 1
    ),
    size = guide_legend(
      title.position = 'top',
      label.position = 'bottom',
      order = 2,
      override.aes = list(colour = title_color, shape=19)
    ),
    fill = "none") +
  labs(#title = "The 100 most co-cited references",
       subtitle = glue::glue("<br><i>N<sub>Nodes</sub></i> = {cocit_summary$Size},
                             <i>N<sub>Edges</sub></i> = {cocit_summary$Edge},
                             <i>Diameter</i> = {cocit_summary$Diameter},
                             <i>Realized connectance</i> = {round(cocit_summary$RealizedConnectance,2)}"))
}

text_data <- cocit_fig$data %>% 
  group_by(group) %>% 
  arrange(desc(Frequency)) %>% 
  slice_head(n=1) %>%
  ungroup()

text_position <- text_data %>%
  select(x,y,group) %>% 
  add_row(x=c(2,5),y=c(5.3,-.7),group=factor(rep(1,2))) %>% #Levin 1992
  add_row(x=c(-1.5,-4, -4.5),y=c(0.5,2, -4),group=factor(rep(2,3))) %>%  #Levin 1998
  add_row(x=c(0,-1.6),y=c(-1,-7),group=factor(rep(3,2))) %>% #May
  add_row(x=c(2,1),y=c(-3,-8),group=factor(rep(4,2))) %>% #Vandermeer 
  add_row(x=c(-3.5,-3),y=c(-8,-9),group=factor(rep(5,2))) %>%  #Ulanowicz
  arrange(group)
	
box_text <- tribble( 
  ~x, ~y, ~label, ~group,
  #group1  
  4.3 ,  -.7 ,  "&nbsp;70. S. A. Levin,<br>The problem of pattern and scale in ecology: <br>The Robert H. macarthur award lecture.<br><i>Ecology</i>. 73, 1943–1967 (1992). "     , 1 ,
  #group2  
  -3.5 , -4  ,   "&nbsp;18. S. A. Levin,<br>Ecosystems and the biosphere as complex adaptive systems.<br><i>Ecosystems</i>. 1, 431–436 (1998)."     , 2,
  #group3  
  -2, -6.5   , "&nbsp;2. R. M. May,<br><i>Stability and Complexity in Model Ecosystems</i><br>(Princeton University Press, 1973)."      , 3,
  #group4  
  3,  -8 , "&nbsp;151. J. Vandermeer, I. Perfecto, S. Philpott,<br>Ecological complexity and pest control in organic coffee production:<br>Uncovering an autonomous ecosystem service.<br><i>Bioscience</i>. 60, 527–537 (2010).", 4,
  #group5  
  -2,  -9 ,  "&nbsp;43. R. E. Ulanowicz,<br><i>Growth and Development</i><br>(Springer New York, 1986)."     , 5
  
) %>% 
  mutate(group = factor(group))

(cocit_fig_final<-
  cocit_fig +
  geom_bspline(
    data = text_position,
    aes(x, y,
        group = group,
        colour = group,
        colour = after_scale(colorspace::darken(colour, .1, space = "HLS"))
        ),
    size = 1
  ) +
    geom_textbox(data=box_text |>  slice(1), aes(x=x,
                                    y=y,
                                    group= group,
                                    colour = group,
                                    label=label,
                                    colour = after_scale(colorspace::darken(colour, .1, space = "HLS")))
                 ,
                 box.size=1,
                 width = .3,
                 text.colour = "black",
                 hjust=0.5,
                 family = "Lato",
                 size = 5,
                 inherit.aes=FALSE
    ) +
  geom_textbox(data=box_text |>  slice(2), aes(x=x,
                                               y=y,
                                               group= group,
                                               colour = group,
                                               label=label,
                                               colour = after_scale(colorspace::darken(colour, .1, space = "HLS")))
               ,
               box.size=1,
               width = .3,
               text.colour = "black",
               hjust=0.5,
               family = "Lato",
               size = 5, 
               inherit.aes=FALSE
  ) +
    geom_textbox(data=box_text |>  slice(3), aes(x=x,
                                                 y=y,
                                                 group= group,
                                                 colour = group,
                                                 label=label,
                                                 colour = after_scale(colorspace::darken(colour, .1, space = "HLS")))
                 ,
                 box.size=1,
                 width = .3,
                 text.colour = "black",
                 hjust=0.5,
                 family = "Lato",
                 size = 5, 
                 inherit.aes=FALSE
    ) +
    geom_textbox(data=box_text |>  slice(4), aes(x=x,
                                                 y=y,
                                                 group= group,
                                                 colour = group,
                                                 label=label,
                                                 colour = after_scale(colorspace::darken(colour, .1, space = "HLS")))
                 ,
                 box.size=1,
                 width = .45,
                 text.colour = "black",
                 hjust=0.5,
                 family = "Lato",
                 size = 5, 
                 inherit.aes=FALSE
    ) +
    geom_textbox(data=box_text |>  slice(5), aes(x=x,
                                                 y=y,
                                                 group= group,
                                                 colour = group,
                                                 label=label,
                                                 colour = after_scale(colorspace::darken(colour, .1, space = "HLS")))
                 ,
                 box.size=1,
                 width = .2,
                 text.colour = "black",
                 hjust=0.5,
                 family = "Lato",
                 size = 5, 
                 inherit.aes=FALSE
    ) +
  geom_node_point(
    data = ~ .x %>%  group_by(group) %>% arrange(desc(Frequency)) %>%  slice(1),
    aes(x,
        y,
        size = Frequency,
        fill = group,
        fill = after_scale(colorspace::lighten(fill, .3, space = "HLS")),
        colour = after_scale(colorspace::darken(fill, .1, space = "HLS"))),
    shape = 21
    ,stroke=1.5
  ) +
    scale_colour_manual(values=cocit_colors)+
  coord_cartesian(clip = "off"))
  

ggsave(filename = "Figures/Figure_5.pdf", plot = cocit_fig_final, device=cairo_pdf, width= 15, height=9)




adj_cocit<- igraph::as_adjacency_matrix( final_cocit %>%
                                           as_tbl_graph() %>%
                                           activate(nodes),attr = c("value")) |> 
  as.matrix() |>
  as.data.frame.table() |> 
  # mutate(Freq = ifelse(Freq == 0, NA, Freq)) |> 
  left_join(clustered_co_citation |> 
              pluck(membership) |> 
              magrittr::set_class("numeric") |> 
              data.frame() |>  
              set_names("group.x") |> 
              rownames_to_column("Var1")) |> 
  left_join(clustered_co_citation |> 
              pluck(membership) |> 
              magrittr::set_class("numeric") |> 
              data.frame() |>  
              set_names("group.y") |> 
              rownames_to_column("Var2")) |> 
  left_join(data.frame(group.x= 1:5 ,colors.x = cocit_colors)) |> 
  left_join(data.frame(group.y= 1:5 ,colors.y = cocit_colors)) |> 
  mutate(
    Var1_colour = glue::glue("<span style='color:{colors.x};'>{Var1}</span>"),
    Var2_colour = glue::glue("<span style='color:{colors.y};'>{Var2}</span>"),
  ) |> 
  mutate(Var1_colour = fct_reorder2(Var1_colour, Var1, group.x),
         Var2_colour = fct_reorder2(Var2_colour, Var2, group.y)) 

plot_adj_cocit <- adj_cocit |> 
  ggplot(aes(x=Var2_colour,y=Var1_colour,fill=Freq)) + 
  geom_tile() + 
  scale_fill_gradient(low = "#ede5cf",
                      high = "#541f3f", na.value=NA)+
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_markdown(size=10),
        axis.text.x = element_markdown(angle=90, hjust=1,size=10),
        panel.grid = element_blank()) + 
  scale_x_discrete(limits = rev(levels(adj_cocit$Var2_colour))) + 
  labs(x="",y="", fill = "Co-occurrences")


ggsave("Figures/Figure_S4.pdf", plot_adj_cocit)

# Network regression ---------------------------------------------------------

network_ergm <- db_graph_original %>%
  left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE)) %>% as_tibble() %>% 
  filter(SEARCH_TYPE != "Complex system science") %>%
  select(-SEARCH_TYPE) %>% 
  pivot_longer(!c(WOS_ID)) %>% 
  pivot_wider(names_from = WOS_ID, values_from = value) %>% 
  column_to_rownames("name") %>% 
  igraph::graph_from_incidence_matrix(multiple = TRUE,
                                      directed = TRUE,
                                      weighted = NULL) %>%
  igraph::bipartite_projection() %>%  #projecting to unipartite
  pluck("proj2") %>% #selecting node type 1
  as_tbl_graph(directed = TRUE) %>%  #convert to table graph object
  tidygraph::activate(nodes) |> 
  left_join(ref_tab %>%  select(WOS_ID, SEARCH_TYPE), by = c("name"="WOS_ID")) 
  
ResponseNetwork <- network_ergm %>%
  get.adjacency(sparse = FALSE) %>%
  as.matrix %>%  as.network(
    matrix.type = "a",
    ignore.eval = FALSE,
    names.eval = "weight"
  )
ResponseNetwork %v%  "SEARCH_TYPE"  <- network_ergm %>%  pull(SEARCH_TYPE)

#@Fitting the model ----
ERGM1 <- ergm::ergm(ResponseNetwork ~ edges + nodefactor("SEARCH_TYPE") + nodematch("SEARCH_TYPE"), estimate = "MLE")

summary(ERGM1)
