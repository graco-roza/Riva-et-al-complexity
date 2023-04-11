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

library(tidyverse)
library(readxl)
files <- list.files("Control")
dir <- "Control"
control_database <-
  lapply(files, function(x)
    readxl::read_excel(paste(dir, x, sep = "/"), col_names = TRUE) %>%  mutate(across(.fns =
                                                                                        as.character))) %>% bind_rows()
control_old <- read_csv("Database/Control_papers.csv") %>% mutate(across(.fns =
                                                                           as.character)) 

control_database_pool <- control_database[which(!control_database$DOI %in% control_old$DOI),] 
control_new <-control_database_pool[sample(seq(nrow(control_database_pool)), 88),] 


control_new %>%
  write_csv('Database/Control_papers_new.csv') 
