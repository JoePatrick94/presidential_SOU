#Scraping web for informal SOTUs - otherwise seen as address before a joint session of congress

## Scraping the presidential state of the union addresses

#load libraries
library(tidyverse) 
library(rvest)
library(lubridate)
library(data.table)

#set working directory
setwd("~/Personal/presidential_SOU")

#Function to get the political party of president
get_party <- function(presidential_biolinks){
  biopage <- read_html(presidential_biolinks)
  party <- biopage %>% html_nodes("#block-system-main :nth-child(9)") %>% html_text()
  return(party)
}

#Function to get the SOTU text for each president
get_speech <- function(SOTU_pagelinks){
  sotulink <- read_html(SOTU_pagelinks) 
  text <- sotulink %>% 
    html_nodes(".field-docs-content") %>% 
    html_text() %>%
    str_replace_all("\\s*\\[[^\\]]+\\]", "") %>% #remove bracket text, such as ...[laughter]...
    str_replace_all("\n","") %>% #remove instances of \n
    str_replace_all("Audience Members.*The President\\.", "") %>% #Take out text from The Audience. --> The President.
    str_replace_all("The President\\.", "") %>% # Take out text indicating the president is speaking -> The President.
    trimws(which = "both")
  return(text)
}

sotu_informal <- data.frame()

for (page_number in seq(from = 0, to = 3,by=1)) {
  
  #State of the Union page that has all the state of the union addresses
  SOTUlistpage <- read_html(paste("https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-",page_number, sep = ""))

  #Get president name
  president <- SOTUlistpage %>% 
    html_nodes(".diet-title a") %>%
    html_text()
  
  #Get year of SOTU speech
  year <- SOTUlistpage %>% 
    html_nodes(".date-display-single") %>%
    html_text() %>%
    str_sub(-4) %>% 
    as.numeric()
  
  #Get years active of president
  years_active <- SOTUlistpage %>%
    html_nodes(".dates") %>%
    html_text() %>%
    gsub("\\s+", "",.)
  
  #Get the party part of the president
  presidential_biolinks <- SOTUlistpage %>%
    html_nodes(".diet-title a") %>%
    html_attr("href") %>%
    paste("https://www.presidency.ucsb.edu",., sep="")
  
  party <- sapply(presidential_biolinks, get_party)
  
  
  #Ge the SOTU text
  text <- sapply(SOTU_pagelinks, get_speech)
  
  
  #Combine rows to the final dataset for each loop
  sotu_informal <- rbind(sotu_informal, data.frame(president, year, years_active, party, speech_type = "speech", text, stringsAsFactors = F))
  
  print(paste("page number:", page_number))
  
}


#save dataset as csv
fwrite(sotu_informal, "sotu_informalspeeches.csv")

#read in final dataset
sotu_final <- fread("sotu_final.csv")

#bind together final dataset
sotu_final2 <- rbind(sotu_final, sotu_informal)


#save final dataset
fwrite(sotu_final2, "sotu_final2.csv")