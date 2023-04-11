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

pacman::p_load(pdftools,tidyverse,tidytext,stringr)

#Fix file names whenever needed ----
# old_names<-list.files(path="Input/", pattern=":")
# new_names<- gsub(":","_",list.files(path="Input/", pattern=":"))
# setwd("Input/")
# file.rename(old_names,new_names)
# setwd("..")
# sort(downloaded)

#Load files 
#directory <- "Input"
#pdfs <- paste(directory, "/", list.files(directory, pattern = "*.pdf"), sep = "")
#pdf_names <- list.files(directory, pattern = "*.pdf")
#pdfs_text <- map(pdfs, pdftools::pdf_text)


pacman::p_load(pdftools,tidyverse,tidytext,stringr, SnowballC,h2o, tidygraph, igraph,ggraph, topicmodels,SemNetCleaner,udpipe,tabulizer,tesseract, patchwork)

#' Load data @**********************************************************************

directory <- "Input"
pdf_names <- list.files(directory, pattern = "*.pdf")
ref_tab   <- read_csv("Database/Table_papers.csv") #database listing papers 


eng <- tesseract(language = "eng", options = list(tessedit_pageseg_mode = 2))
#'read text files from pdfs @******************************************************
pdfs_text <- list() 
for (i in 340:length(pdf_names)){
   print(i)
   # Convert papers 78, 143, 189, 190 and 339 an image
   if(i %in% c(78,143,189,190,339)){
      scan <- pdftools::pdf_convert(paste(directory, "/", pdf_names, sep = "")[i], format = 'tiff',  dpi = 600)
      pdfs_text[[i]] <- tesseract::ocr(scan, engine = eng)
   } else {
      pdfs_text[[i]]<- tabulizer::extract_text(paste(directory, "/", pdf_names, sep = "")[i])
   }
   
}
#'@********************************************************************************

#saveRDS(pdfs_text, "articles.rds")
pdfs_text <- readRDS("articles.rds")

text_parsed<-lapply(pdfs_text, function(x) {out1<- stringi::stri_replace_all_regex(x, "-\\s+", "")
out2 <- stringi::stri_replace_all_regex(out1, "-", "_")
return(out2)})
#' Get n-grams (old way) @*********************************************************

my_data_unigram <- tibble(document = pdf_names, text = pdfs_text)  %>% 
   unnest(cols=c(text)) %>% # transforming list into data frame
   unnest_tokens(word, text, strip_numeric = TRUE) %>%  # no number
   group_by(document, word) %>% 
   summarise(count=n()) |> 
   mutate(document = gsub(".pdf","",document))

my_data_bigram <- tibble(document = pdf_names, text = pdfs_text)  %>% 
   unnest(cols=c(text)) %>% # transforming list into data frame
   unnest_tokens(bigram, text,  token = "ngrams", n = 2) %>% 
   group_by(document, bigram) %>% 
   summarise(count=n()) |> 
   mutate(document = gsub(".pdf","",document))

#Diversity search  ---------

diversity <-  my_data_unigram %>% 
   filter(str_detect(word, "^diversit")) %>% 
   group_by(document) %>% 
   summarise(diversity = sum(count))

#Interaction search  ---------

interaction <-  my_data_unigram %>% 
   filter(str_detect(word, "^interact")) %>% 
   group_by(document) %>% 
   summarise(interaction = sum(count))

#Aggregation search  ---------

aggregation <-  my_data_unigram %>% 
   filter(str_detect(word, "^aggregat")) %>% 
   group_by(document) %>% 
   summarise(aggregation = sum(count))


#Self organization search ----

self_organization_unigram <-  my_data_unigram %>% 
   filter(str_detect(word, "^selforga") | str_detect(word, "^self-orga")) %>% 
   group_by(document) %>% 
   summarise(self_organization = sum(count))

self_organization_bigram <-  my_data_bigram %>% 
   filter(str_detect(bigram, "^self orga") ) %>% 
   group_by(document) %>% 
   summarise(self_organization = sum(count))

self_organization <- bind_rows(self_organization_unigram,self_organization_bigram) %>% 
   group_by(document) %>% 
   summarise(self_organization = sum(self_organization))

#Adaptation search  ---------

adaptation <-  my_data_unigram %>% 
   filter(str_detect(word, "^adapt")) %>% 
   group_by(document) %>% 
   summarise(adaptation = sum(count))

#Non linearity search ----

non_linearity_unigram <-  my_data_unigram %>% 
   filter(str_detect(word, "^nonlinear") | str_detect(word, "^non-linear")) %>% 
   group_by(document) %>% 
   summarise(non_linearity = sum(count))

non_linearity_bigram <-  my_data_bigram %>% 
   filter(str_detect(bigram, "^non linear") ) %>% 
   group_by(document) %>% 
   summarise(non_linearity = sum(count))

non_linearity <- bind_rows(non_linearity_unigram,non_linearity_bigram) %>% 
   group_by(document) %>% 
   summarise(non_linearity = sum(non_linearity))

#Hierarchy search ----

hierarchy <-  my_data_unigram %>% 
   filter(str_detect(word, "^hierarch")) %>% 
   group_by(document) %>% 
   summarise(hierarchy = sum(count))

#Modularity search ----

modularity <-  my_data_unigram %>% 
   filter(str_detect(word, "^modul")) %>% 
   group_by(document) %>% 
   summarise(modularity = sum(count))

#emergence search ----

emergence <-  my_data_unigram %>% 
   filter(str_detect(word, "^emergen")) %>% 
   group_by(document) %>% 
   summarise(emergence  = sum(count))

#Scale dependency search ----

scale_dependency_unigram <-  my_data_unigram %>% 
   filter(str_detect(word, "^scal") | str_detect(word, "^scale-depen") | str_detect(word, "^scaledepen")) %>%
   filter(word != "scaled") %>% 
   group_by(document) %>% 
   summarise(scale_dependency = sum(count))

scale_dependency_bigram  <-  my_data_bigram %>% 
   filter(str_detect(bigram, "^scale depen")) %>% 
   group_by(document) %>% 
   summarise(scale_dependency = sum(count))

scale_dependency <- bind_rows(scale_dependency_unigram,scale_dependency_bigram) %>% 
   group_by(document) %>% 
   summarise(scale_dependency = sum(scale_dependency))

#Fractality search ----

fractality <-  my_data_unigram %>% 
   filter(str_detect(word, "^fractal")) %>% 
   group_by(document) %>% 
   summarise(fractality = sum(count))

#Flow search ----

flow <-  my_data_unigram %>% 
   filter(str_detect(word, "^flow")) %>% 
   group_by(document) %>% 
   summarise(flow = sum(count))

#dynamicity search ----

dynamicity <-  my_data_unigram %>% 
   filter(str_detect(word, "^dynamic")) %>% 
   group_by(document) %>% 
   summarise(dynamicity  = sum(count))

#memory search ----

memory <-  my_data_unigram %>% 
   filter(str_detect(word, "^memory") | str_detect(word, "^memories")) %>% 
   group_by(document) %>% 
   summarise(memory  = sum(count))

#feedback search ----

feedback <-  my_data_unigram %>% 
   filter(str_detect(word, "^feedback")) %>% 
   group_by(document) %>% 
   summarise(feedback  = sum(count))

#Hysteresis search ----

hysteresis <-  my_data_unigram %>% 
   filter(str_detect(word, "^hystere")) %>% 
   group_by(document) %>% 
   summarise(hysteresis  = sum(count))

#Non equilibrium ------
non_equilibrium_unigram <-  my_data_unigram %>% 
   # mutate(word = case_when(word == "1a" ~ "selforganization", word == "1c" ~ "self-organisation", TRUE ~ word)) %>% 
   filter(str_detect(word, "^nonequilib") | str_detect(word, "^non-equilib")) %>%
   group_by(document) %>% 
   summarise(non_equilibrium = sum(count))

non_equilibrium_bigram  <-  my_data_bigram %>% 
   # mutate(word = case_when(word == "1a" ~ "selforganization", word == "1c" ~ "self-organisation", TRUE ~ word)) %>% 
   filter(str_detect(bigram, "^non equilib")) %>% 
   group_by(document) %>% 
   summarise(non_equilibrium = sum(count))

non_equilibrium <- bind_rows(non_equilibrium_unigram,non_equilibrium_bigram) %>% 
   group_by(document) %>% 
   summarise(non_equilibrium = sum(non_equilibrium))

#Attractor search ----

attractor <-  my_data_unigram %>% 
   filter(str_detect(word, "^attractor")) %>% 
   group_by(document) %>% 
   summarise(attractor = sum(count))

#Homeostasis search ----

homeostasis <-  my_data_unigram %>% 
   filter(str_detect(word, "^homeosta")) %>% 
   group_by(document) %>% 
   summarise(homeostasis = sum(count))

#Stability search ----

stability <-  my_data_unigram %>% 
   filter(str_detect(word, "^stabilit")) %>% 
   group_by(document) %>% 
   summarise(stability  = sum(count))

#Resilience search ----

resilience <-  my_data_unigram %>% 
   filter(str_detect(word, "^resilien")) %>% 
   group_by(document) %>% 
   summarise(resilience  = sum(count))

#tipping point search ----

tipping_point <-  my_data_bigram %>% 
   filter(str_detect(bigram, "^tipping point")) %>% 
   group_by(document) %>% 
   summarise(tipping_point  = sum(count))

#threshold search ----

threshold <-  my_data_unigram %>% 
   filter(str_detect(word, "^thresho")) %>% 
   group_by(document) %>% 
   summarise(threshold  = sum(count))

#heterarchy search ----

heterarchy <-  my_data_unigram %>% 
   filter(str_detect(word, "^heterarch")) %>% 
   group_by(document) %>% 
   summarise(heterarchy  = sum(count))


#chaos search ----

chaos <-  my_data_unigram %>% 
  filter(str_detect(word, "chaos") | str_detect(word, "chaotic")) %>%
  group_by(document) %>% 
  summarise(chaos  = sum(count))

#network search ----

network <-  my_data_unigram %>% 
   filter(str_detect(word, "^network")) %>% 
   group_by(document) %>% 
   summarise(network  = sum(count))

#Total number of words ----

word_count <- my_data_unigram %>%
   anti_join(tidytext::stop_words) %>% 
   group_by(document) %>%
   summarise(word_total = sum(count))

#Combining datasets -------

documents_list <- my_data_unigram %>% group_by(document) %>%  top_n(1) %>% select(document)

output <- Reduce(
   full_join,
   list(
      documents_list,
      diversity,
      interaction,
      aggregation,
      self_organization,
      adaptation,
      non_linearity,
      hierarchy,
      modularity,
      emergence,
      scale_dependency,
      fractality,
      flow, 
      dynamicity,
      memory,
      feedback,
      non_equilibrium,
      attractor,
      homeostasis,
      stability, 
      resilience,
      threshold,
      network, 
      chaos
   )
) %>%
   mutate_at(vars(-document) , replace_na, replace = 0) %>%
   mutate_at("document", ~ str_sub(., end = nchar(.) - 4)) %>% #removing .pdf from document name
   rename(WOS_ID = document) %>% 
   mutate_at("WOS_ID", factor) %>%
   distinct(WOS_ID, .keep_all= TRUE)


output <- Reduce(
   full_join,
   list(
      documents_list,
      diversity,
      interaction,
      aggregation,
      self_organization,
      adaptation,
      non_linearity,
      hierarchy,
      modularity,
      emergence,
      scale_dependency,
      fractality,
      flow, 
      dynamicity,
      memory,
      feedback,
      non_equilibrium,
      attractor,
      homeostasis,
      stability, 
      resilience,
      threshold,
      network, 
      chaos
   )) 

output<-output |> 
   pivot_longer(!document, names_to = "term", values_to = "mentions") |> 
   mutate(mentions = ifelse(is.na(mentions),0,1)) |> 
   filter(mentions > 0) |> 
   ungroup()

figure_a<-  output %>% mutate(term=factor(term, levels=rev(sort(unique(.$term))))) %>%
   ggplot(aes(x=term)) +
   geom_bar() +
   geom_text(stat='count', aes(label=after_stat(count)), hjust=-.2) + 
   coord_flip() + theme_bw() + 
   labs(x="",y="Number of studies") 
#Save absolute word count 
write_csv(output %>%
             select(-word_total),
          file = "Database/text_compiled_absolute.csv")

#save relative word count
write_csv(output %>%
             mutate_at(vars(-WOS_ID), ~ ./word_total * 1e4)  %>% 
             select(-word_total),
          file = "Database/text_compiled_relative.csv")




# Supplementary Material Figure S5 --------------------------------------------------------------------------------

get_unigram <- function(term,string) {
   foo<- my_data_unigram %>% 
      filter(str_detect(word, string)) %>% 
      group_by(document) %>% 
      summarise("{{term}}" := sum(count))
   return(foo)
}

get_bigram <- function(term,string) {
   foo<- my_data_bigram %>% 
      filter(str_detect(bigram, string)) %>% 
      group_by(document) %>% 
      summarise("{{term}}" := sum(count))
   return(foo)
}

#adapt search  ---------


niche <-  my_data_unigram %>% 
   filter(str_detect(word, "niche")) %>% 
   group_by(document) %>% 
   summarise(niche = sum(count))

plasticity <-  my_data_unigram %>% 
   filter(str_detect(word, "plasticity")) %>% 
   group_by(document) %>% 
   summarise(plasticity = sum(count))

phenology <-  my_data_unigram %>% 
   filter(str_detect(word, "phenology")) %>% 
   group_by(document) %>% 
   summarise(phenology = sum(count))



consortia <-  my_data_unigram %>% 
   filter(str_detect(word, "consortia")) %>% 
   group_by(document) %>% 
   summarise(consortia = sum(count))

superstructure <-  my_data_unigram %>% 
   filter(str_detect(word, "superstructure")) %>% 
   group_by(document) %>% 
   summarise(superstructure = sum(count))

#attractor search  ---------


hysteresis <-  my_data_unigram %>% 
   filter(str_detect(word, "hysteresis")) %>% 
   group_by(document) %>% 
   summarise(hysteresis = sum(count))

tipping_point <-  get_bigram(tipping_point, "tipping point")

stable_state <-  my_data_bigram %>% 
   filter(str_detect(bigram, "stable state")) %>% 
   group_by(document) %>% 
   summarise(stable_state = sum(count))



#Chaos search  ---------

sensibility <- my_data_unigram %>% 
   filter(str_detect(word, "sensibility")) %>% 
   group_by(document) %>% 
   summarise(sensibility = sum(count))

divergence <-  my_data_unigram %>% 
   filter(str_detect(word, "divergence")) %>% 
   group_by(document) %>% 
   summarise(divergence = sum(count))

phase_space <-  my_data_bigram %>% 
   filter(str_detect(bigram, "phase space")) %>% 
   group_by(document) %>% 
   summarise(phase_space = sum(count))

#Diversity search  ---------

entropy <-  my_data_unigram %>% 
   filter(str_detect(word, "entropy")) %>% 
   group_by(document) %>% 
   summarise(entropy = sum(count))

heterogeneity <-  my_data_unigram %>% 
   filter(str_detect(word, "heterogeneity")) %>% 
   group_by(document) %>% 
   summarise(heterogeneity = sum(count))

information <-  my_data_unigram %>% 
   filter(str_detect(word, "information")) %>% 
   group_by(document) %>% 
   summarise(information = sum(count))

variation <-  my_data_unigram %>% 
   filter(str_detect(word, "variation")) %>% 
   group_by(document) %>% 
   summarise(variation = sum(count))


#Dynamicity search  ---------

evolution <-  my_data_unigram %>% 
   filter(str_detect(word, "evolution")) %>% 
   group_by(document) %>% 
   summarise(evolution = sum(count))

stasis <-  my_data_unigram %>% 
   filter(str_detect(word, "stasis")) %>% 
   group_by(document) %>% 
   summarise(stasis = sum(count))

transformation <-  my_data_unigram %>% 
   filter(str_detect(word, "transformation")) %>% 
   group_by(document) %>% 
   summarise(transformation = sum(count))


#Emergence search  ---------

collective_intelligence <-  my_data_bigram %>% 
   filter(str_detect(bigram, "collective intelligence")) %>% 
   group_by(document) %>% 
   summarise(collective_intelligence = sum(count))

gestalt_principles <-  my_data_unigram %>% 
   filter(str_detect(word, "gestalt principles")) %>% 
   group_by(document) %>% 
   summarise(gestalt_principles = sum(count))

#Feedback search  ---------

reinforcement <-  my_data_unigram %>% 
   filter(str_detect(word, "reinforcement")) %>% 
   group_by(document) %>% 
   summarise(reinforcement = sum(count))

top_down <-  my_data_bigram %>% 
   filter(str_detect(bigram, "top down")) %>% 
   group_by(document) %>% 
   summarise(top_down = sum(count))


#Flow search  ---------

linkages <-  my_data_unigram %>% 
   filter(str_detect(word, "linkages")) %>% 
   group_by(document) %>% 
   summarise(linkages = sum(count))

#Fractality search  ---------

regularity <-  my_data_unigram %>% 
   filter(str_detect(word, "regularity")) %>% 
   group_by(document) %>% 
   summarise(regularity = sum(count))

scale_invariance <-  my_data_bigram %>% 
   filter(str_detect(bigram, "scale invariance")) %>% 
   group_by(document) %>% 
   summarise(scale_invariance = sum(count))

#Hierarchy search  ---------

levels <-  my_data_unigram %>% 
   filter(str_detect(word, "levels")) %>% 
   group_by(document) %>% 
   summarise(levels = sum(count))

nestedness <-  my_data_unigram %>% 
   filter(str_detect(word, "nestedness")) %>% 
   group_by(document) %>% 
   summarise(nestedness = sum(count))

#Homeostasis search  ---------

control <-  my_data_unigram %>% 
   filter(str_detect(word, "control")) %>% 
   group_by(document) %>% 
   summarise(control = sum(count))


#Interaction search  ---------

competition <- my_data_unigram %>% 
   filter(str_detect(word, "competition")) %>% 
   group_by(document) %>% 
   summarise(competition = sum(count))

dependence <-  my_data_unigram %>% 
   filter(str_detect(word, "dependence")) %>% 
   group_by(document) %>% 
   summarise(dependence = sum(count))

parasitism <-  my_data_unigram %>% 
   filter(str_detect(word, "parasitism")) %>% 
   group_by(document) %>% 
   summarise(parasitism = sum(count))

mutualism <-  my_data_unigram %>% 
   filter(str_detect(word, "mutualism")) %>% 
   group_by(document) %>% 
   summarise(mutualism = sum(count))

synergy <-  my_data_unigram %>% 
   filter(str_detect(word, "synergy")) %>% 
   group_by(document) %>% 
   summarise(synergy = sum(count))


#Memory search  ---------

lagged <- get_unigram(lagged,"lagged")

response <- get_unigram(response,"response")

markov_processes <- get_bigram(markov_process,"markov process")

#Modulatiry search -----------------

cluster <- get_unigram(cluster,"cluster")

connectivity <- get_unigram(connectivity,"connectivity")


#Network search -----------------

food_webs <- get_bigram(food_webs,"food webs")

nodes <- get_unigram(nodes,"nodes")

#Non-equilibrium search -----------------

balance <- get_bigram(balance,"balance")

disturbance <- get_unigram(disturbance,"disturbance")

stable_states <- get_bigram(stable_states,"stable states")

instability <- get_unigram(instability,"instability")

#Non-lineatiry search -----------------

higher_order <- get_bigram(higher_order,"higher order")

#Resilience search -----------------

brittleness <- get_unigram(brittleness,"brittleness")

robustness <- get_unigram(robustness,"robustness")

stability <- get_unigram(stability,"stability")
heterarchy <- get_unigram(heterarchy,"heterarchy")

#Scaling search -----------------

discrete_hierarchy <- get_unigram(discrete_hierarchy,"discrete hierarchy")
grain <- get_unigram(grain,"grain")
levels<- get_unigram(levels,"levels")

#self-organization search -----------------
evolution <- get_unigram(evolution, "evolution")
multicellularity <- get_unigram(multicellularity, "multicellularity")
pattern_formation <- get_bigram(pattern_formation, "pattern formation")

#stability search -----------------
invasibility <- get_unigram(invasibility, "invasibility")
persistence <- get_unigram(persistence, "persistence")
resistance <- get_unigram(resistance, "resistance")
robustness <- get_unigram(robustness, "robustness")
criticality <- get_unigram(criticality, "criticality")
panarchy <- get_unigram(panarchy, "panarchy")
scaling <- get_unigram(scaling, "scaling")

#threshold search -----------------
tipping_point <- get_bigram(tipping_point, "tipping_point")

objects<- ls()

objects<- objects[which(!objects %in% c("my_data_unigram","plt","terms","test","documents_list","terms","text_parsed","my_data_bigram","pdfs_text","objects","pdf_names","ref_tab","directory","get_bigram","get_unigram"))]

terms <- lapply(objects, function(x) eval(parse(text=x)))

test <- Reduce(full_join, terms)

test <- test |>  
   pivot_longer(!document, names_to = "term", values_to = "mentions") |> 
   mutate(mentions = ifelse(is.na(mentions),0,1)) |> 
   filter(mentions > 0) |> 
   left_join(ref_tab |>  select(WOS_ID,SEARCH_TYPE) |> rename("document" = WOS_ID)) |> 
   filter(SEARCH_TYPE == "Ecological complexity")


plt<-    test %>%
       mutate(term=factor(term, levels=rev(sort(unique(.$term))))) %>%
       group_by(term) |> 
       count() |> 
       mutate(prop = n/172) |> 
   ggplot(aes(y=term,x=prop*100)) +
   geom_bar(stat="identity") +
#  geom_text(stat='identity', aes(label=glue::glue("{round(prop*100,0)}%")), hjust=-.2) + 
  theme_bw() + 
   labs(y="",x="Proportion of studies") +
   xlim(0,100)


ggsave(plot=plt,filename="figure_s5_3.pdf",device=cairo_pdf(), height=7 ,width=10 , units="in")   

britleness
