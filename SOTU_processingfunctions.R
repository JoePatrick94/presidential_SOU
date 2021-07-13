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
  party <- biopage %>% 
    html_nodes("#block-system-main :nth-child(9)") %>% 
    html_text()
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