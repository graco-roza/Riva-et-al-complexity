# Article: Towards a cohesive understanding of ecological complexity

#' ------------------------------------------------------------------------
#'R script to reproduce the Latent Dirichlet Analysis (LDA)
#' ------------------------------------------------------------------------

#' Authors:  Caio Graco-Roza
#' Last update: 17 Apr 2023, Helsinki, Finland.
#' Software: R (v. R 4.2.2) 

#'##############################################################

#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
pacman::p_load(pdftools,tidyverse,tidytext,stringr, SnowballC,h2o, tidygraph, igraph,ggraph, topicmodels,SemNetCleaner,udpipe,tabulizer,tesseract, patchwork)

#' DONT RUN (CODE FOR RETRIEVING TEXT FROM PDFS)
#' Load data @**********************************************************************
#' directory <- "~/Library/CloudStorage/OneDrive-UniversityofHelsinki/Ongoing manuscripts/Riva-et-al-complexity/Input"
#' pdf_names <- list.files(directory, pattern = "*.pdf")
#' ref_tab   <- read_csv("Database/Table_papers.csv") #database listing papers 
#' 
#' 
#' eng <- tesseract(language = "eng", options = list(tessedit_pageseg_mode = 2))
#' #'read text files from pdfs @******************************************************
#' pdfs_text <- list() 
#' for (i in c(1:length(pdf_names))){
#' print(i)
#'   
#'   # Convert papers 78, 143, 189, 190 and 339 to an image
#'   if(i %in% c(78,143,189,190,339)){
#'   scan <- pdftools::pdf_convert(paste(directory, "/", pdf_names, sep = "")[i], format = 'tiff',  dpi = 600)
#'   pdfs_text[[i]] <- tesseract::ocr(scan, engine = eng)
#'   } else {
#' pdfs_text[[i]]<- tabulizer::extract_text(paste(directory, "/", pdf_names, sep = "")[i])
#' }
#' 
#' }
#' saveRDS(pdfs_text, "articles.rds")
#'@********************************************************************************


pdfs_text <- readRDS("articles.rds")

text_parsed<-lapply(pdfs_text, function(x) {out1<- stringi::stri_replace_all_regex(x, "-\\s+", "")
                              out2 <- stringi::stri_replace_all_regex(out1, "-", "_")
                              return(out2)})
#' Get n-grams @*********************************************************

papers_unigrams <- tibble(WOS_ID = pdf_names, text = text_parsed)  %>% 
  unnest(cols=c(text)) %>% # transforming list into data frame
  unnest_tokens(word, text, strip_numeric = TRUE) %>%  # no number
  mutate(WOS_ID = gsub(".pdf","",WOS_ID)) %>% 
  left_join(ref_tab %>%  dplyr::select(WOS_ID, SEARCH_TYPE)) 

fixed_terms <- papers_unigrams %>% 
  mutate(word = textstem::lemmatize_words(word)) %>% 
  mutate(word = ifelse(str_detect(word, "^diversit"),"diversity",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^interact"),"interaction",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^aggregat"),"aggregation",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^\\organization\\b|\\organized\\b|\\organizing\\b|\\organize\\b"),"self-organization",word)) %>%   
  mutate(word = ifelse(str_detect(word, "^adapt"),"adaptation",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^nonlinear|^non_linear"),"non-linearity",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^hierarch"),"hierarchy",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^modul"),"modularity",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^emergence|emerge|emergent|emerged"),"emergence",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^scal|^scale_depen"),"scale-dependency",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^fractal"),"fractality",word)) %>% 
  mutate(word = ifelse(str_detect(word, "flow|flowing|flows"),"flow",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^dynamic"),"dinamicity",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^memory|^memories"),"memory",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^feedback"),"feedback",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^hystere"),"hysteresis",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^nonequilib|^non_equilib"),"non-equilibrium",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^attractor"),"attractor",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^homeosta"),"homeostasis",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^stabilit"),"stability",word)) %>% 
  mutate(word = ifelse(str_detect(word, "^resilien"),"resilience",word)) %>% 
    mutate(word = ifelse(str_detect(word, "tipping"),"tipping point",word)) %>% 
    mutate(word = ifelse(str_detect(word, "^thresho"),"threshold",word)) %>% 
    mutate(word = ifelse(str_detect(word, "^heterarch"),"heterarchy",word)) %>% 
    mutate(word = ifelse(str_detect(word, "^chaos|chaotic"),"chaos",word)) %>% 
    mutate(word = ifelse(str_detect(word, "^network"),"network",word)) %>% 
    filter(!str_detect(word,c("\\.com|www\\.|http|[0-9]"))) %>%  #remove links
    mutate(word=str_replace(word,"_","-")) %>% #back-convert underscore to hyphen
    filter(!str_detect(word,"[^-[:^punct:]]")) %>% #remove punctuation
  group_by(SEARCH_TYPE, WOS_ID) %>% 
  count(word) %>% 
  mutate(nchar= nchar(word)) 

#Remove stop words, and other unimportant strings

uni_sw <- data.frame(word = c("al","figure","image","table","box","i.e", "l3","e.g")) #list of words we do not want

fixed_terms <- fixed_terms %>%
  anti_join(tidytext::stop_words, by="word") %>% #stop words
  anti_join(uni_sw, by = "word") %>%  #bag of words
  filter(nchar >= 4 & nchar < 20) #small words and very long words are mostly typos and bad reading from the algorithm

papers_dtm_overall <- fixed_terms %>%
  ungroup() %>% 
  cast_dtm(term=word,document=WOS_ID,value=n)

papers_dtm_control <- fixed_terms %>%
  ungroup() %>% 
  filter(SEARCH_TYPE == "Control") %>%  
  cast_dtm(term=word,document=WOS_ID,value=n)

papers_dtm_EC <- fixed_terms %>%
  ungroup() %>% 
  filter(SEARCH_TYPE != "Control") %>%  
  cast_dtm(term=word,document=WOS_ID,value=n)


papers_lda_overall <- LDA(papers_dtm_overall, k=100, control = list(seed = 1234))
papers_lda_control <- LDA(papers_dtm_control, k=100, control = list(seed = 1232))
papers_lda_EC <- LDA(papers_dtm_EC, k=100, control = list(seed = 1235))


papers_topics <-   list(papers_lda_overall %>% tidy(matrix = "beta") %>% mutate(group="Overall"),
     papers_lda_control %>% tidy(matrix = "beta") %>% mutate(group="Control"),
    papers_lda_EC %>% tidy(matrix = "beta") %>% mutate(group="Ecological complexity")) %>%  
  bind_rows() %>% 
  mutate(group = factor(group, levels=c("Overall","Control","Ecological complexity"))) %>% 
  mutate(term= str_replace(term, "scale-dependency","scaling"))

features <-sort(c("diversity",
             "interaction",
             "aggregation",
             "self-organization", 
             "adaptation",
             "non-linearity",
             "hierarchy",
             "modularity",
             "emergence",
             "scaling",
             "fractality",
             "flow",
             "dinamicity",
             "memory",
             "feedback",
             "non-equilibrium",
             "attractor",
             "homeostasis",
             "stability",
             "resilience",
             "threshold",
             "network",
             "chaos"))

quartile_data <- 
  papers_topics %>%
  arrange(group,topic, desc(beta)) %>%
  group_split(group,topic) %>% 
    map(~.x %>% 
    mutate(upper = HDInterval::hdi(object=.$beta, credMass = 0.99)[2]) 
    ) %>%  
    bind_rows() %>% 
    group_by(group,topic) %>%  
filter(beta > upper) %>% 
mutate(rank = rank(beta, ties.method = "max")) %>% 
mutate(Quartile = case_when(between(rank, 1, ceiling(max(rank)*0.25)) ~ "Q1",
                            between(rank, ceiling(max(rank)*0.25), ceiling(max(rank)*0.5)) ~ "Q2",
                            between(rank, ceiling(max(rank)*0.5), ceiling(max(rank)*0.75)) ~ "Q3",
                            between(rank, ceiling(max(rank)*0.75), ceiling(max(rank))) ~ "Q4"
                            )) %>% 
  ungroup() %>%
  filter(term %in% features ) %>% 
  mutate(term = factor(term, levels=features),
         Quartile = factor(Quartile, levels=c("Q1","Q2","Q3","Q4"))) %>% 
  group_by(group,term) %>% 
  count(Quartile) %>% 
  group_by(group) %>% 
  complete(term, Quartile) %>% 
  mutate(n = ifelse(is.na(n), 0,n)) %>% 
  arrange(desc(term)) %>% 
  ungroup()


terms_quartile <- quartile_data %>%  
  mutate(bold = "plain") %>% #add plain here because only the totals will have bold values
  bind_rows(
    #get bold values only for control and EC groups
    quartile_data %>% 
      filter(group != "Overall") %>% 
      group_by(group,term) %>% 
      summarise(n = sum(n), Quartile = "Total")  %>% 
      group_by(term) %>% 
      mutate(bold = ifelse(n == max(n), "bold","plain")) %>% 
      #add the Overall group
      bind_rows( 
        quartile_data %>% 
          filter(group == "Overall") %>% 
          group_by(group,term) %>% 
          summarise(n = sum(n), Quartile = "Total") %>% 
          mutate(bold = "plain")
        ) 
  ) %>%  arrange(group, term, Quartile) %>% 
  mutate(term = factor(term, levels=features),
         Quartile = factor(Quartile, levels=c("Q1","Q2","Q3","Q4","Total"))) 

ticks<- data.frame(x=rep(2.5:5.5,3),
                   y=rep(rep(22.2,4),3),
                   label=rep(c("25%","50%","75%","100%"),3),
                   groups=sort(rep(c("Control","Ecological complexity","Overall"),4))
                   )


searchtype_colors<- c("#CA3542", "#57575F")

(figure_S2A <- terms_quartile %>% 
    filter(group != "Overall") %>% 
mutate(term_num = rev(as.numeric(term)-1)) %>% 
 ggplot(aes(ymin=term_num,xmin=as.numeric(Quartile) + 1)) +
 facet_wrap(~group, scales="free_x") +
 geom_rect(data = ~ .x %>%  filter(Quartile != "Total"), aes(ymax=term_num + .9,xmax=as.numeric(Quartile) + 2, fill=group, alpha=n/100), linewidth=.3, colour="gray70")+
 geom_rect(data = ~ .x %>%  filter(Quartile == "Total"),aes(ymax=term_num + .9,xmax=as.numeric(Quartile) + 2), fill=NA, colour=NA)+
 geom_text(data = ~ .x %>%  filter(Quartile != "Total"), aes(y = term_num +.45, x = as.numeric(Quartile) + 1.5, label=n), inherit.aes=TRUE, colour="black", family="PT Sans",size=6/(14/5))+
 geom_text(data = ~ .x %>%  filter(Quartile == "Total"), aes(y = term_num +.45, x = as.numeric(Quartile) + 1.5, label=n, colour=group, fontface= bold), family="PT Sans", size=6/(14/5), inherit.aes=TRUE)+
    scale_y_continuous(breaks=seq(0.5,22.5,1), limits=c(0,23), labels =rev(str_to_sentence(unique(terms_quartile$term))), expand=c(0,0.05)) +
  scale_x_continuous(breaks=seq(2.5,6.5,1), labels =levels(terms_quartile$Quartile), position="top") +
     scale_alpha_continuous(range=c(0,1))+
theme_void()+
 theme(legend.position="none",
       axis.text = element_text(size=6, family="PT Sans"),
       axis.text.x.top=element_text(size=6, family="PT Sans")
       ,strip.text.x = element_text(size=6, face="bold", vjust=1,  hjust=0.4,  family="PT Sans")
       , strip.placement = "outside"
       ) + 
    scale_fill_manual(values= rev(searchtype_colors))+
    scale_colour_manual(values= rev(searchtype_colors))
  ) 
  

papers_topic_mean <- papers_topics %>% 
   group_by(group, term) %>% 
  summarise(mean = mean(beta), sd = sd(beta)) %>% 
  group_by(group) %>% 
  summarise(global_mean = mean(mean),
         global_sd = sd(sd)) %>% 
 filter(group != "Overall")
#Diversity search  ---------

terms_sum <- 
   papers_topics %>% 
  group_by(group,term) %>%
  filter(term %in% features) %>%
  group_by(group,topic, term) %>% 
  summarise(beta = sum(beta)) %>% 
  mutate(group = factor(group, levels=c("Overall","Control","Ecological complexity"))) 

terms_mean<- 
  terms_sum %>% 
  mutate(term=factor(str_to_sentence(term), levels=unique(str_to_sentence(term)))) %>%
  group_by(group,term) %>%
  mutate(mean.beta = mean(beta, na.rm = TRUE),
            sd.beta = sd(beta, na.rm = TRUE),
            n.beta = n()) %>%
  mutate(se.beta = sd.beta / sqrt(n.beta),
         lower.ci.beta = mean.beta - qt(1 - (0.05 / 2), n.beta - 1) * se.beta,
         upper.ci.beta = mean.beta + qt(1 - (0.05 / 2), n.beta - 1) * se.beta) %>%
  mutate(beta = ifelse(beta >= lower.ci.beta & beta <= upper.ci.beta, beta, NA)) %>%
ungroup() %>%
  mutate(grid_fill = as.numeric(term) %% 2 == 0,
         draw_min = min(beta,na.rm=TRUE),
         draw_max = round(max(beta, na.rm=TRUE),3)) %>%
  mutate(group = factor(group, levels=c("Overall","Control","Ecological complexity"))) %>% 
  filter(group != "Overall") 


figure_S2B<-terms_mean %>% 
  arrange(term) %>% 
  ggplot(aes(x=beta, y=term, fill = group, colour= group, group=group)) +
  geom_rect(
    data =~ .x %>% filter(grid_fill == TRUE),
    aes(
      xmin = draw_min+1,
      xmax = draw_max-1,
      ymin = as.numeric(term) - .5,
      ymax = as.numeric(term) + .5
    ),
    fill = "gray95", inherit.aes=FALSE,
  )+
  geom_errorbarh(aes(xmin=lower.ci.beta, xmax=upper.ci.beta, fill= group, colour=group), position = position_dodge(0.9), linewidth=.3)+
  geom_point(aes(x=mean.beta, fill=group, colour=group, group=group), shape=21, size=1, position = position_dodge(0.9))+
  geom_vline(data=papers_topic_mean, aes(xintercept=global_mean, colour=group), size=0.2)+
  labs(fill = "Group", colour = "Group", x="Beta parameter\n(Mean ± 95% confidence interval)")+
  theme_bw(base_family="PT Sans", base_size = 8)+
  scale_y_discrete(limits=rev, expand=c(0.02,0.02))+
  coord_cartesian(xlim=c(0,0.0045))+
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values= rev(searchtype_colors))+
  scale_colour_manual(values= rev(searchtype_colors))

Figure_S2 <- figure_S5A + figure_S5B + plot_layout(widths=c(.7,.3)) + plot_annotation(tag_levels="A") & 
  theme(
        plot.tag = element_text(size = 8, hjust = 0, vjust=0,  face="bold"),
        plot.margin = margin(t=0,b=0,l=1,r=1,unit="mm")) 

ggsave(filename = "Figures/Figure_S2.pdf", Figure_S2, device= cairo_pdf(), width = 7.25, height =.5*7.25, units= "in")
