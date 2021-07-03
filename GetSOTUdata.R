## Scraping the presidential state of the union addresses

#load libraries
library(tidyverse) 
library(rvest)
library(lubridate)
library(data.table)

#package to get SOTU addresses is:
library(sotu)

#make dataset
sotu <- sotu_meta

#connect metadata with text
sotu$text <- sotu_text

#SOTU package is missing trump and biden speeches, so we need to pull these speeches and combine them in this data set
SOTUlistpage <- read_html("https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/state-the-union-addresses")

#Get president name
president <- SOTUlistpage %>% html_nodes(".margin-top a") %>%
  html_text()

#Get year of SOTU speech
year <- SOTUlistpage %>% html_nodes(".date-display-single") %>%
  html_text() %>% str_sub(-4) %>% as.numeric()

#Get years active of president
SOTU_pagelinks <- SOTUlistpage %>%
  html_nodes(".field-title a") %>%
  html_attr("href") %>% paste("https://www.presidency.ucsb.edu",., sep="")

get_years_active <- function(SOTU_pagelinks){
  
  sotupage <- read_html(SOTU_pagelinks)
  years_active <- sotupage %>% html_nodes(".dates") %>% html_text() %>% gsub("\\s+", "",.)
  return(years_active)
}
  
years_active <- sapply(SOTU_pagelinks, get_years_active)

#Get the party part of the president
presidential_biolinks <- SOTUlistpage %>%
  html_nodes(".margin-top a") %>%
  html_attr("href") %>%
  paste("https://www.presidency.ucsb.edu",., sep="")

get_party <- function(presidential_biolinks){
  biopage <- read_html(presidential_biolinks)
  party <- biopage %>% html_nodes("#block-system-main :nth-child(9)") %>% html_text()
  return(party)
}

party <- sapply(presidential_biolinks, get_party)


#Get the SOTU text
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

text <- sapply(SOTU_pagelinks, get_speech)


#Make Data frame
sotu2 <- data.frame(president, year, years_active, party, sotu_type = "speech", text)

#combine sotu datasets
sotu_final <- rbind(sotu, sotu2)


#save dataset as csv
fwrite(sotu_final, "sotu_final.csv")


