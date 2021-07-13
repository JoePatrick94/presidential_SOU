#Scraping web of state of the union written messages

#load libraries
library(tidyverse) 
library(rvest)
library(lubridate)
library(data.table)

setwd("~/Personal/presidential_SOU")

#Load in processing functions
source("SOTU_processingfunctions.R")

sotu_messages <- data.frame()

for (page_number in seq(from = 0, to = 13,by=1)) {
  
  #State of the Union page that has all the state of the union messages
  SOTUlistpage <- read_html(paste("https://www.presidency.ucsb.edu/documents/app-categories/written-messages/presidential/state-the-union-messages?page=",page_number, sep = ""))

  #Get president name
  president <- SOTUlistpage %>% 
    html_nodes(".margin-top a") %>%
    html_text()
  
  #Get year of SOTU speech
  year <- SOTUlistpage %>% 
    html_nodes(".date-display-single") %>%
    html_text() %>%
    str_sub(-4) %>% 
    as.numeric()
  
  #Get years active of president
  SOTU_pagelinks <- SOTUlistpage %>%
    html_nodes(".field-title a") %>%
    html_attr("href") %>% 
    paste("https://www.presidency.ucsb.edu",., sep="")
  
  years_active <- sapply(SOTU_pagelinks, get_years_active)
  
  #Get the party part of the president
  presidential_biolinks <- SOTUlistpage %>%
    html_nodes(".margin-top a") %>%
    html_attr("href") %>%
    paste("https://www.presidency.ucsb.edu",., sep="")
  
  party <- sapply(presidential_biolinks, get_party)
  
  
  #Ge the SOTU text
  text <- sapply(SOTU_pagelinks, get_speech)
  
  
  #Combine rows to the final dataset for each loop
  sotu_messages <- rbind(sotu_messages, data.frame(president, year, years_active, party, speech_type = "written", text, stringsAsFactors = F))
  
  print(paste("page number:", page_number))
  
}

#Read in SOTU speeches
sotu_speeches <- fread("sotu_speeches.csv")

#combine speeches and messages
sotu_final <- rbind(sotu_speeches, sotu_messages)

#save dataset as csv
fwrite(sotu_final, "sotu_final.csv")


