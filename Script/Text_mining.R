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

#TRASH 
# mutate(word = case_when(word == "1a" ~ "selforganization", word == "1c" ~ "self-organisation", TRUE ~ word)) %>% 

 directory <- "Input"
 pdfs <- paste(directory, "/", list.files(directory, pattern = "*.pdf"), sep = "")
 pdf_names <- list.files(directory, pattern = "*.pdf")
 pdfs_text <- map(pdfs, pdftools::pdf_text)
pdfs_text<-list() 
for (i in 1:259){
   pdfs_text[[i]]<- pdf_text(pdfs[i])
   print(pdf_names[i])
}
   pdfs_text <- map(pdfs, pdftools::pdf_text) 
 
   pdf_text(pdfs[259])
 my_data_unigram <- tibble(document = pdf_names, text = pdfs_text)  %>% 
    unnest(cols=c(text)) %>% # transforming list into data frame
    unnest_tokens(word, text, strip_numeric = TRUE) %>%  # no number
    group_by(document, word) %>% 
    summarise(count=n())
 
 
 my_data_bigram <- tibble(document = pdf_names, text = pdfs_text)  %>% 
    unnest(cols=c(text)) %>% # transforming list into data frame
    unnest_tokens(bigram, text,  token = "ngrams", n = 2) %>% 
    group_by(document, bigram) %>% 
    summarise(count=n())


 #Diversity search  ---------
 
diversity <-  my_data_unigram %>% 
   filter(str_detect(word, "^diversit") | str_detect(word, "^heterogen")) %>% 
   group_by(document) %>% 
   summarise(diversity = sum(count))
 
 #Interaction search  ---------
 
interaction <-  my_data_unigram %>% 
   filter(str_detect(word, "^interact")) %>% 
   group_by(document) %>% 
   summarise(interaction = sum(count))
 
 #Aggregation search  ---------
 
aggregation <-  my_data_unigram %>% 
   filter(str_detect(word, "^aggregat") | str_detect(word, "^cluster")) %>% 
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

#network search ----

network <-  my_data_unigram %>% 
   filter(str_detect(word, "^network")) %>% 
   group_by(document) %>% 
   summarise(network  = sum(count))


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
      hysteresis,
      non_equilibrium,
      attractor,
      homeostasis,
      stability, 
      resilience,
      tipping_point,
      threshold,
      heterarchy, 
      network
   )
) %>%
   mutate_at(vars(-document) , replace_na, replace = 0) %>%
   mutate_at("document", ~ str_sub(., end = nchar(.) - 4)) %>% #removing .pdf from document name
   rename(WOS_ID = document)


    

