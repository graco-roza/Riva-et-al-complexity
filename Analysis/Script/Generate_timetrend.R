# Article: Towards a cohesive understanding of ecological complexity

#' ------------------------------------------------------------------------
#'R script to extract the number of papers published by year in a given topic
#' ------------------------------------------------------------------------

#' Authors:  Stefano Mammola
#' Last update: 11 Apr 2023, Verbania, Italy.
#' Software: R (v. R 4.2.2) 

# Loading R package -------------------------------------------------------

library("wosr")      # Clients to the 'Web of Science' and 'InCites' APIs

# Loading data on number of publication  ---------------------------------

# WoS queries -------------------------------------------------------------

# Data source: Web of Science, accessed on 10.6.2021
# [Helsinki, Google Chrome, macOS High Sierra 10.13.6]

#Setting sid
sid <- auth(NULL, password = NULL)  #change with your WoS access

        #Setting WoS collection
coll <-  c("SCI", "SSCI", "AHCI", "ISTP", "ISSHP","BSCI", "BHCI", "ESCI")

# Extracting N° publication by year ---------------------------------------
range_year <- c(1970:2020)

N   <- c()
N2  <- c()
#Extracting WoS data 
for (i in 1:length(range_year)){
  
  query_1 <- query_wos(
    glue::glue("ALL = \"ecolog* complex*\"  AND PY = ({range_year[i]})")
    ,editions = coll, sid = sid)
  
  query_2 <- query_wos(
    glue::glue("ALL = (\"complexity\")  AND PY = ({range_year[i]})")
    ,editions = coll, sid = sid)
  

  N   <- append(N, query_1$rec_cnt)
  
  N2   <- append(N2, query_2$rec_cnt)
  
}

#Storing the result
db1 <- data.frame(year = range_year,N)
db2 <- data.frame(year = range_year,N2)

bind_rows(db2 %>%  mutate(group = "Complexity") %>%  rename(N = N2),
          db1 %>%  mutate(group = "Ecological complexity")) %>% 
  group_by(group) %>% 
  mutate(across(N, cumsum)) %>% 
  readr::write_csv("Database/timetrend.csv")
  
