## Scraping the presidential state of the union addresses

#load libraries
library(tidyverse) 
library(rvest)
library(lubridate)

#package to get SOTU addresses is:
library(sotu)

#make dataset
sotu <- sotu_meta

#connect metadata with text
sotu$text <- sotu_text

#SOTU package is missing trump and biden speeches, so we need to pull these speeches and combine them in this data set
SOTUlistpage <- read_html("https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/state-the-union-addresses")
#need to turn this into a dataset that we can match with the sotu dataset
#lets build a function that can turn the html into this tidy dataset
# each speech starts with The President. Can start here.
# each speech ends with NOTE: - end here
# each paragraph is a element within a character vector - need to combine each into a single vector
#html children looks at child nodes, which allows us to see who is talking. We will probably want to filter for just the president

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
  sotulink <- read_html("https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-27")
  text <- sotulink %>% 
    html_nodes(".field-docs-content") %>% 
    html_text() %>%
    str_replace_all(x, "\\s*\\[[^\\]]+\\]", "") %>% #remove bracket text, such as ...[laughter]...
    str_replace_all(x, "\n","") %>% #remove instances of \n
    str_replace_all(x, "\n")
  text
  return(text)
}

#take out anything with [text], all instances of \n
#take out all text that says Audience members to The President. 


