#Scraping web for informal SOTUs - otherwise seen as address before a joint session of congress

## Scraping the presidential state of the union addresses

#load libraries
library(tidyverse) 
library(rvest)
library(lubridate)
library(data.table)

setwd("~/Personal/presidential_SOU")

#Load in processing functions
source("SOTU_processingfunctions.R")


  #State of the Union page that has all the state of the union addresses
Biden_Speech_Page <- read_html("https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-3")

  #Ge the SOTU text
  text <- Biden_Speech_Page %>% 
    html_nodes(".field-docs-content") %>% 
    html_text() %>%
    str_replace_all("\\s*\\[[^\\]]+\\]", "") %>% #remove bracket text, such as ...[laughter]...
    str_replace_all("\n","") %>% #remove instances of \n
    str_replace_all("Audience Members.*The President\\.", "") %>% #Take out text from The Audience. --> The President.
    str_replace_all("The President\\.", "") %>% # Take out text indicating the president is speaking -> The President.
    trimws(which = "both") %>%
    str_replace_all("\\.(?=\\w)", " ") %>% # if a period does not have a space after it, replace it with a space
    str_replace_all("â", "")

  
  #Combine rows to the final dataset for each loop
Biden <- data.frame(president = "Joseph R. Biden", 
                    year = 2021, 
                    years_active = "2021-Present",
                    party = "Democratic", 
                    speech_type = "speech", 
                    text, 
                    stringsAsFactors = F)
  



#save dataset as csv
fwrite(Biden, "Biden_Informal_speech.csv")

#read in final dataset
sotu_final <- fread("sotu_final.csv")

#bind together final dataset
sotu_final2 <- rbind(sotu_final, Biden)


#save final dataset
fwrite(sotu_final2, "sotu_final2.csv")
