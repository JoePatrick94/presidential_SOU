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
  count(word1, word2, sort = T)


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



