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
