## Scraping the presidential state of the union addresses

#load libraries
library(tidyverse) 
library(rvest)
library(lubridate)
library(data.table)

setwd("~/Personal/presidential_SOU")

#Load in processing functions
source("SOTU_processingfunctions.R")

sotu_df <- data.frame()

start_page <- 0
end_page <- 9

for (page_number in start_page:end_page) {
  
  #State of the Union page that has all the state of the union addresses
  SOTUlistpage <- read_html(paste("https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/state-the-union-addresses?page=",page_number, sep = ""))
  
  #Get president name
  president <- SOTUlistpage %>% 
    html_nodes(".margin-top a") %>%
    html_text()
  
  #Get year of SOTU speech
  year <- SOTUlistpage %>% 
    html_nodes(".date-display-single") %>%
    html_text() %>%
    str_sub(-4) %>% #take last 4 digits
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
  sotu_df <- rbind(sotu_df, data.frame(president, year, years_active, party, speech_type = "speech", text, stringsAsFactors = F))
  
  print(paste("page number:", page_number))
  
}


#save dataset as csv
fwrite(sotu_df, "sotu_speeches.csv")


