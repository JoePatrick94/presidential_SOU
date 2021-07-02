# single token analysis of SOTU

#load libraries
library(tidytext)

#Read in data
source("C:/Users/599126/Documents/Personal/presidential_SOU/GetSOTUdata.R")

#tokenise dataframe
singleword <- sotu %>% unnest_tokens(word, text)

#remove stop words
sw_nostop <- singleword %>% anti_join(stop_words)

#create counts of words for all SOTU speeches
sw_nostop %>% count(word, sort = T) %>% #war is number 8... above peace...
  slice(1:15) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "dodgerblue4") + 
  coord_flip()




