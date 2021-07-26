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

#average lengths of written vs spoken speeches
singleword %>%
  count(year,president, speech_type) %>%
  ggplot(aes(x = speech_type, y = n)) +
  geom_boxplot(fill = "dodgerblue4") +
  labs(title = "Comparison of Speech and Written SOTU address Lengths",
       x = "",
       y = "Length of Speech") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,35000, by = 5000),
                     labels = comma) +
  theme(panel.grid.minor.y = element_blank()) +
  annotate("text",
           x = 1.75,
           y = 34000,
           label = "Jimmy Carter 1980\nand 1981 Speeches",
           size = 2.5) +
  ggsave("images/lengthcomparison.png")


#get max speeches
singleword %>%
  count(year, president, speech_type, sort = T)

#get average length
singleword %>%
  count(year, president, speech_type) %>%
  group_by(speech_type) %>%
  summarise(mean = mean(n))
  


#remove stop words
sw_nostop <- singleword %>% 
  anti_join(stop_words) %>%
  anti_join(my_stop_words) %>% 
  anti_join(common_words)

#create counts of words for all SOTU speeches
 sw_nostop %>%
  count(word, speech_type, sort = T) %>% #war is number 8... above peace...
  group_by(speech_type) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder_within(word, n, speech_type), y = n)) +
  geom_col(fill = "dodgerblue4") + 
  coord_flip() +
  labs(x = "",
       y = "Frequency of Term",
       title = "Top 15 Words Used in all SOTU Addresses") +
  theme_bw() +
  facet_wrap(~speech_type, scales = "free") +
  scale_x_reordered() +
  ggsave("images/top15_allsotu.png")
 
   

#tf-idf
speechwords <- singleword %>%
  count(president, word, sort = T)

totalwords <- speechwords %>%
  group_by(president) %>%
  summarise(total = sum(n))

speechwords <- left_join(speechwords, totalwords)

speech_tf_idf <- speechwords %>%
  bind_tf_idf(word, president, n) %>%
  anti_join(stop_words) %>%
  anti_join(my_stop_words) %>%
  anti_join(common_words)

#visualize biden, trump, obama, bush, clinton, and bush according to tf-idf
speech_tf_idf %>%
  group_by(president) %>%
  filter(president %in% c("George W. Bush", "Donald J. Trump", "Barack Obama",
                          "Joseph R. Biden", "William J. Clinton", "George Bush")) %>%
  mutate(president = factor(president, levels = c("George Bush", "William J. Clinton", "George W. Bush",
                                                  "Barack Obama", "Donald J. Trump", "Joseph R. Biden"))) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, president))) +
  geom_col(fill = "dodgerblue4" ,show.legend = FALSE) +
  facet_wrap(~as.factor(president), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "Top 10 Unique Words of Last 6 Presidents",
       x = "") +
  scale_y_reordered() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  ggsave("images/top_tfidf_presidents.png")



#visualize any president speech according to tf-idf with interactive chart


#visualize biden, trump, obama, bush, clinton, and bush according to tf
speech_tf_idf %>%
  group_by(president) %>%
  filter(president %in% c("Andrew Jackson", "Abraham Lincoln", "John Quincy Adams",
                          "Warren G. Harding", "William Howard Taft", "Chester A. Arthur")) %>%
  slice_max(tf, n = 9) %>%
  ungroup() %>%
  ggplot(aes(tf, reorder_within(word, tf, president))) +
  geom_col(fill = "dodgerblue4" ,show.legend = FALSE) +
  facet_wrap(~as.factor(president), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "Top 10 Words Used of Last 6 Presidents",
       x = "") +
  scale_y_reordered() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())


#tf-idf of political parties
speechwords <- singleword %>%
  count(party, word, sort = T)

totalwords <- speechwords %>%
  group_by(party) %>%
  summarise(total = sum(n))

speechwords <- left_join(speechwords, totalwords)

speech_tf_idf <- speechwords %>%
  bind_tf_idf(word, party, n) %>%
  anti_join(stop_words) %>%
  anti_join(my_stop_words) %>%
  anti_join(common_words)

#visualize democratic and republican words according to tf-idf
speech_tf_idf %>%
  filter(party %in% c("Democratic", "Republican")) %>%
  group_by(party) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, party), fill = party)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#0015BC", "#E9141D"))+
  facet_wrap(~as.factor(party), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "Top 20 Unique Words for Democrat and Republican Speeches",
       x = "") +
  scale_y_reordered() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  ggsave("images/top_tfidf_party.png")

  

#visualize democratic and republican words according to proportions
speech_tf_idf %>%
  group_by(party) %>%
  slice_max(tf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf, reorder_within(word, tf, party), fill = party)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~as.factor(party), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "Top 20 Words Used for Democrat and Republican ",
       x = "") +
  scale_y_reordered() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())


#Bi Gram analysis of certain terms - energy, assault
bigram <- sotu %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#separate the bigram into columns and remove stop words
bigram_separated <- bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_filtered <- bigram_separated %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word1 %in% my_stop_words$word,
         !word2 %in% my_stop_words$word)

#most common bigrams
bigram_filtered %>%
  count(word1, word2, sort = T) %>%
  filter(word1 %in% black_terms)



# Bigram tf-idf
bigram_united <- bigram_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigram_united %>%
  count(president, bigram) %>%
  bind_tf_idf(bigram, president, n) %>%
  arrange(desc(tf_idf))

#visualize bigrams of biden, trump, obama, bush, clinton, and bush according to tf-idf
bigram_tf_idf %>%
  group_by(president) %>%
  filter(president %in% c("George W. Bush", "Donald J. Trump", "Barack Obama",
                          "Joseph R. Biden", "William J. Clinton", "George Bush")) %>%
  mutate(president = factor(president, levels = c("George Bush", "William J. Clinton", "George W. Bush",
                                                  "Barack Obama", "Donald J. Trump", "Joseph R. Biden"))) %>%
  slice_max(tf_idf, n = 9, with_ties = F) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(bigram, tf_idf, president))) +
  geom_col(fill = "dodgerblue4" ,show.legend = FALSE) +
  facet_wrap(~as.factor(president), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "Top 10 Bigrams Words of Last 6 Presidents",
       x = "") +
  scale_y_reordered() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

#visualize bigrams of Andrew Jackson, Abraham Lincoln, Lyndon B. Johnson, Theodore Roosevelt, Warren Harding, and ronald reagan according to tf-idf
bigram_tf_idf %>%
  group_by(president) %>%
  filter(president %in% c("Andrew Jackson", "Abraham Lincoln", "Lyndon B. Johnson",
                          "Theodore Roosevelt", "Warren Harding", "Ronald Reagan")) %>%
  slice_max(tf_idf, n = 9, with_ties = F) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(bigram, tf_idf, president))) +
  geom_col(fill = "dodgerblue4" ,show.legend = FALSE) +
  facet_wrap(~as.factor(president), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "Top 10 Bigrams Words of Last 6 Presidents",
       x = "") +
  scale_y_reordered() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

#visualize bigrams according to tf
bigram_tf_idf %>%
  group_by(president) %>%
  filter(president %in% c("Andrew Jackson", "Abraham Lincoln", "Lyndon B. Johnson",
                          "Theodore Roosevelt", "Warren Harding", "Ronald Reagan")) %>%
  slice_max(tf, n = 9, with_ties = F) %>%
  ungroup() %>%
  ggplot(aes(tf, reorder_within(bigram, tf, president))) +
  geom_col(fill = "dodgerblue4" ,show.legend = FALSE) +
  facet_wrap(~as.factor(president), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "Top 10 Bigrams Used of Last 6 Presidents",
       x = "") +
  scale_y_reordered() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())



#Sentiment analysis - sentiments scores of speeches over time filled in with party
presidential_sentiment <- sw_nostop %>%
  inner_join(get_sentiments("bing")) %>%
  count(year, party, word, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  group_by(year, party) %>%
  summarise(sentiment = sum(sentiment))

ggplot(presidential_sentiment, aes(year, sentiment, fill = party)) +
  geom_col() +
  labs(title = "Sentiment Scores of Every SOTU Address",
       x = "Year",
       y = "Sentiment", 
       fill = "Party") +
  theme_bw() +
  geom_hline(yintercept = 0)



#Over time analysis of black key terms

#change plural black terms to singular to capture true amount
sw_nostop_blacktermedits <- sw_nostop %>%
  mutate(word = ifelse(word == "africans", "african",
                ifelse(word == "slaves", "slave",
                ifelse(word == "racists", "racist",
                ifelse(word == "racist", "racism",
                ifelse(word == "reparation", "reparations", word))))))

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
  

#frequencies
years_tf_idf %>%
  mutate(black_indicator = ifelse(word %in% black_terms, 1, 0)) %>%
  filter(black_indicator == 1) %>%
  complete(year, word, fill = list(tf = 0)) %>%
  group_by(year, black_indicator) %>%
  summarise(count = sum(n),
            freq = sum(tf)) %>%
  ggplot(aes(x = year, y = freq)) +
  geom_point() +
  geom_smooth(se = F) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank())

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
  theme(panel.grid.minor.x = element_blank())

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

#use of black and blacks - we find that jimmy carter 
years_tf_idf %>% 
  filter(word == "blacks") %>% 
  arrange(year)

  

#Over time analysis of race related key terms
#counts
years_tf_idf %>%
  mutate(race_indicator = ifelse(word %in% race_terms, 1, 0)) %>%
  filter(race_indicator == 1) %>%
  complete(year, word, fill = list(n = 0)) %>%
  group_by(year, race_indicator) %>%
  summarise(count = sum(n),
            freq = sum(tf)) %>%
  ggplot(aes(x = year, y = count)) +
  geom_point() +
  geom_smooth(se = F) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank()) +
  labs(x = "",
       y = "Count",
       title = "Counts of Any Race/Ethnicity Related Terms Over Time")

#frequencies
years_tf_idf %>%
  mutate(race_indicator = ifelse(word %in% race_terms, 1, 0)) %>%
  filter(race_indicator == 1) %>%
  complete(year, word, fill = list(tf = 0)) %>%
  group_by(year, race_indicator) %>%
  summarise(count = sum(n),
            freq = sum(tf)) %>%
  ggplot(aes(x = year, y = freq)) +
  geom_point() +
  geom_smooth(se = F) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "% of SOTU Speech",
       title = "Frequencies of Any Race/Ethnicity Related Terms Over Time")

#most used race related terms
sw_nostop %>%
  filter(word %in% race_terms) %>%
  count(word, sort = T) %>% #war is number 8... above peace...
  slice(1:20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "dodgerblue4") + 
  coord_flip() +
  labs(x = "",
       y = "Frequency of Term",
       title = "Frequencies of Race/Ethnicity Related Terms") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,30, by = 2)) +
  theme(panel.grid.minor.x = element_blank())


#top six black terms over time
#counts
years_tf_idf %>%
  filter(word %in% c("indian", "indians", "mexican", "chinese", "native")) %>%
  complete(year, word, fill = list(n = 0)) %>%
  ggplot(aes(x = year, y = n)) +
  geom_point() +
  geom_smooth(se = F) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank()) +
  facet_wrap(.~word,
             scales = "free") +
  labs(x = "",
       y = "Count",
       title = "Counts of Top 12 Most Used Race/Ethnicity Related Terms Over Time")
