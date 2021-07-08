#Scraping web of state of the union written messages

#load libraries
library(tidyverse) 
library(rvest)
library(lubridate)
library(data.table)

#set working directory
setwd("~/Personal/presidential_SOU")


#Function to get the years active of a president
get_years_active <- function(SOTU_pagelinks){
  
  sotupage <- read_html(SOTU_pagelinks)
  years_active <- sotupage %>% 
    html_nodes(".dates") %>%
    html_text() %>% 
    gsub("\\s+", "",.)
  return(years_active)
}

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
    str_replace_all("\n","") %>% #remove instances of \n
    trimws(which = "both")
  return(text)
}

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


