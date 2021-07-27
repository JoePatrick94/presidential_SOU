# single token analysis of all SOTUs

#load libraries
library(tidyverse)
library(tidytext)
library(scales)
library(textdata)
library(data.table)
library(plotly)

#Read in data
sotu <- fread("sotu_final2.csv")

#load in word lists
source("WordLists.R")

#tokenise dataframe
singleword <- sotu %>% 
  unnest_tokens(word, text) %>%
  select(president, year, party, speech_type, word)

#remove stop words
sw_nostop <- singleword %>% 
  anti_join(stop_words) %>%
  anti_join(my_stop_words) %>% 
  anti_join(common_words)


#Over time analysis of black key terms

#change plural black terms to singular to capture true amount
sw_nostop_blacktermedits <- sw_nostop %>%
  mutate(word = ifelse(word == "africans", "african",
                ifelse(word == "slaves", "slave",
                ifelse(word == "racists", "racist",
                ifelse(word == "racist", "racism",
                ifelse(word == "reparation", "reparations",
                ifelse(word == "blacks", "black",
                ifelse(word == "coloreds", "colored", word))))))))

years_count <- sw_nostop_blacktermedits %>%
  count(year, word, sort = T)

years_tf_idf <- years_count %>%
  bind_tf_idf(word, year, n)

#counts
most_common_black_term_by_year <- years_tf_idf %>%
  filter(word %in% black_terms) %>%
  group_by(year) %>%
  slice_max(n,n=1) %>%
  select(year, word)


years_black <- years_tf_idf %>%
  mutate(black_indicator = ifelse(word %in% black_terms, 1, 0)) %>%
  filter(black_indicator == 1) %>%
  complete(year, word, fill = list(n = 0)) %>%
  group_by(year, black_indicator) %>%
  summarise(count = sum(n),
            freq = sum(tf)) %>%
  left_join(most_common_black_term_by_year) %>%
  ggplot(aes(x = year, y = count,
             text = paste0("Most Common Term: ", word))) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank()) +
  labs(title = "Use of African American Related Terms Over Time",
       x = "",
       y = "Number of Uses in Address")

ggplotly(years_black)

htmlwidgets::saveWidget(as_widget(ggplotly(years_black)),"images/blackterms_overtime.html")


#find speech with maximum use of race terms
years_tf_idf %>%
  mutate(black_indicator = ifelse(word %in% black_terms, 1, 0)) %>%
  filter(black_indicator == 1) %>%
  complete(year, word, fill = list(tf = 0)) %>%
  group_by(year, black_indicator) %>%
  summarise(count = sum(n),
            freq = sum(tf)) %>%
  arrange(-freq)


#most used black terms
sw_nostop_blacktermedits %>%
  filter(word %in% black_terms) %>%
  count(word, sort = T) %>% #war is number 8... above peace...
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "dodgerblue4") + 
  coord_flip() +
  labs(x = "",
       y = "Frequency of Term",
       title = "Frequencies of Black/African American Related Terms") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,180, by = 20)) +
  theme(panel.grid.minor.x = element_blank()) +
  ggsave("images/mostusedblackterms.png")


#frequencies
years_tf_idf %>%
  filter(word %in% c("slave", "slavery", "african", "black", "colored", "reparations", "blacks", "segregation", "slaveholding", "racism")) %>%
  complete(year, word, fill = list(tf = 0)) %>%
  ggplot(aes(x = year, y = tf)) +
  geom_point() +
  geom_smooth(se = F) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank()) +
  facet_wrap(.~factor(word,levels = c("slave", "slavery", "african", "black", "colored", "reparations", "blacks", "segregation", "slaveholding", "racism")), 
             scales = "free") +
  labs(title = "Usage of Black/African American Terms Over Time in SOTU Addresses",
       x = "",
       y = "Term Frequency") +
  ggsave("images/blackterms_overtime.png")

#use of racism - first use of "racism", "racist", or "racists" was in 1979. Overall, each of these terms have only been used
# 6 total times in all speeches
years_tf_idf %>%
  filter(word == "racism")

#use of reparations - much of the usage is pertaining to other things - check bigram analysis
years_tf_idf %>% 
  filter(word == "reparations") %>% 
  arrange(year)

#use of black and blacks - jimmy carter in 1979 spent a lot of time discussing opportunities for blacks, hispanics, women, and other at-risk minority populations
years_tf_idf %>% 
  filter(word == "black") %>% 
  arrange(year)



#Bi Gram analysis of certain terms - energy, assault
bigram <- sotu %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#separate the bigram into columns and remove stop words
bigram_separated <- bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#filter out stop words and change black terms
bigram_filtered <- bigram_separated %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word1 %in% my_stop_words$word,
         !word2 %in% my_stop_words$word) %>%
  mutate(word1 = ifelse(word1 == "africans", "african",
                 ifelse(word1 == "slaves", "slave",
                 ifelse(word1 == "racists", "racist",
                 ifelse(word1 == "racist", "racism",
                 ifelse(word1 == "reparation", "reparations",
                 ifelse(word1 == "blacks", "black", 
                 ifelse(word1 == "coloreds", "colored", word1))))))),
         word2 = ifelse(word2 == "africans", "african",
                 ifelse(word2 == "slaves", "slave",
                 ifelse(word2 == "racists", "racist",
                 ifelse(word2 == "racist", "racism",
                 ifelse(word2 == "reparation", "reparations",
                 ifelse(word2 == "blacks", "black",
                 ifelse(word2 == "coloreds", "colored",
                 ifelse(word2 == "americans", "american",
                 ifelse(word2 == "persons", "people", word2))))))))))

#most common bigrams
bigram_filtered %>%
  count(word1, word2, sort = T) %>%
  filter(word1 %in% black_terms) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(!bigram %in% c("black sea", "black hills")) %>%
  slice(1:10) %>%
  ggplot(aes(x = n, y = reorder(bigram, n))) +
  geom_col(fill = "dodgerblue4") +
  labs(title = "Most Common Black Related Bigrams",
       x = "Count",
       y = "") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  theme(panel.grid.minor.x = element_blank())



# Bigram tf-idf
bigram_united <- bigram_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigram_united %>%
  count(president, bigram) %>%
  bind_tf_idf(bigram, president, n) %>%
  arrange(desc(tf_idf))



