library(fulltext)
library(tidytext)
library(tidyr)
cache_options_set(path = "Input", full_path = "~/OneDrive - University of Helsinki/Ongoing manuscripts/Riva-et-al-complexity/Input")

#API keys 
# Springer Nature link : https://dev.springernature.com/admin  user_key = c87e8660cce1e392df62d179849c6616
# Elsevier (including ELSEVIER_TDM_KEY) link : https://dev.elsevier.com/apikey/manage   user_key = 	f56a2a13d2aeed53259acf32bcefb21a

database <- readxl::read_excel("table_papers.xls") %>% tidyr::drop_na(DOI)
for (i in 1:nrow(database)){
doi<- database$DOI[i]
wos<- database$WOS_ID[i]
res <- ft_get(doi)
extension<-res[[1]]$data$path[[1]]$type
old<- paste0("Input/",stringr::str_replace_all(doi, c("\\." = "_", "/" = "_")),".",extension)
new<- paste0("Input/",wos,".",extension)
file.rename(old,new)
print(i)
}
