pacman::p_load(pdftools,tidyverse,tidytext,stringr)

 
 directory <- "Input"
 
 pdfs <- paste(directory, "/", list.files(directory, pattern = "*.pdf"), sep = "")
 pdf_names <- list.files(directory, pattern = "*.pdf")
 pdfs_text <- map(pdfs, pdftools::pdf_text)
 
 
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


diversity <-  my_data_unigram %>% 
   filter(str_detect(word, "^diversit") | str_detect(word, "^heterogen")) %>% 
   group_by(document) %>% 
   summarise(diversity = sum(count))

interaction <-  my_data_unigram %>% 
   filter(str_detect(word, "^interact")) %>% 
   group_by(document) %>% 
   summarise(interaction = sum(count))

aggregation <-  my_data_unigram %>% 
   filter(str_detect(word, "^aggregat") | word == "cluster") %>% 
   group_by(document) %>% 
   summarise(aggregation = sum(count))



self_organization_unigram <-  my_data_unigram %>% 
  # mutate(word = case_when(word == "1a" ~ "selforganization", word == "1c" ~ "self-organisation", TRUE ~ word)) %>% 
   filter(str_detect(word, "^selforga") | str_detect(word, "^self-orga")) %>% 
   group_by(document) %>% 
   summarise(organization = sum(count))

self_organization_bigram <-  my_data_bigram %>% 
   # mutate(word = case_when(word == "1a" ~ "selforganization", word == "1c" ~ "self-organisation", TRUE ~ word)) %>% 
   filter(str_detect(bigram, "^self orga") ) %>% 
   group_by(document) %>% 
   summarise(organization = sum(count))

self_organization <- bind_rows(self_organization_unigram,self_organization_bigram) %>% 
   group_by(document) %>% 
   summarise(organization = sum(organization))

documents_list <- my_data_unigram %>% group_by(document) %>%  top_n(1) %>% select(document)

output<- Reduce(full_join,list(documents_list,diversity,interaction,aggregation,self_organization)) %>% 
   mutate_at(vars(-document) , replace_na, replace = 0) %>% 
   mutate_at("document", ~ str_sub(.,end = nchar(.)-4)) %>% #removing .pdf from document name 
   rename(WOS_ID = document)


    

