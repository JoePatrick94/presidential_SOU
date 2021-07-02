## Scraping the presidential state of the union addresses

#load libraries
library(tidyverse) 
library(rvest)

#package to get SOTU addresses is:
library(sotu)

#make dataset
sotu <- sotu_meta

#connect metadata with text
sotu$text <- sotu_text

#SOTU package is missing trump and biden speeches, so we need to pull these speeches and combine them in this data set
SOTU <- read_html("https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-27")
SOTUlistpage <- read_html("https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/state-the-union-addresses")


simple %>% html_nodes("p") %>%
  html_text()

simple %>% html_nodes("p") %>%
  html_children()
#need to turn this into a dataset that we can match with the sotu dataset
#lets build a function that can turn the html into this tidy dataset
# each speech starts with The President. Can start here.
# each speech ends with NOTE: - end here
# each paragraph is a element within a character vector - need to combine each into a single vector
#html children looks at child nodes, which allows us to see who is talking. We will probably want to filter for just the president

SOTUlistpage %>% html_nodes(".margin-top a") %>%
  html_text()

