# single token analysis of SOTU

#load libraries
library(tidytext)
library(scales)
library(textdata)

#Read in data
sotu <- fread("sotu_final2.csv")

#load in word lists
source("WordLists.R")

#tokenise dataframe
singleword <- sotu %>% 
  filter(speech_type == "speech",
         party != "Federalist") %>% 
  unnest_tokens(word, text)

#remove stop words
sw_nostop <- singleword %>% 
  anti_join(stop_words) %>%
  anti_join(my_stop_words) %>% 
  anti_join(common_words)

#create counts of words for all SOTU speeches
sw_nostop %>%
  count(word, sort = T) %>% #war is number 8... above peace...
  slice(1:15) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "dodgerblue4") + 
  coord_flip() +
  labs(x = "",
       y = "Frequency of Term",
       title = "Top 15 Words Used in all SOTU Speeches") +
  theme_bw()


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
  slice_max(tf_idf, n = 9) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, president))) +
  geom_col(fill = "dodgerblue4" ,show.legend = FALSE) +
  facet_wrap(~as.factor(president), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "Top 10 Unique Words of Last 6 Presidents",
       x = "") +
  scale_y_reordered() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())

#visualize biden, trump, obama, bush, clinton, and bush according to tf
speech_tf_idf %>%
  group_by(president) %>%
  filter(president %in% c("George W. Bush", "Donald J. Trump", "Barack Obama",
                          "Joseph R. Biden", "William J. Clinton", "George Bush")) %>%
  mutate(president = factor(president, levels = c("George Bush", "William J. Clinton", "George W. Bush",
                                                  "Barack Obama", "Donald J. Trump", "Joseph R. Biden"))) %>%
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


#tf-idf of republican vs democrat
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
  theme(panel.grid.major.y = element_blank())

#visualize democratic and republican words according to proportions
speech_tf_idf %>%
  group_by(party) %>%
  slice_max(tf, n = 20) %>%
  ungroup() %>%
  ggplot(aes(tf, reorder_within(word, tf, party), fill = party)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#0015BC", "#E9141D"))+
  facet_wrap(~as.factor(party), ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "Top 20 Words Used for Democrat and Republican ",
       x = "") +
  scale_y_reordered() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())
  

#compare trump and biden
biden_trump <- sw_nostop %>% 
  group_by(president) %>%
  count(word, sort = T) %>%
  filter(president %in% c("Donald J. Trump", "Joseph R. Biden")) %>%
  mutate(freq = n/sum(n)) %>%
  select(-n)  %>%
  pivot_wider(names_from = president, values_from = freq) %>%
  pivot_longer("Joseph R. Biden",
               names_to = "Joseph R. Biden", values_to = "freq")


biden_trump %>% 
  ggplot(aes(x = freq, y = `Donald J. Trump`,color = abs(`Donald J. Trump` - freq))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~'Joseph R. Biden', ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Donald J. Trump", x = NULL)


#Bi Gram analysis of certain terms - energy, assault
bigram <- sotu %>%
  filter(speech_type == "speech",
         party != "Federalist") %>%
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


#Tri Gram analysis
trigram <- sotu %>%
  filter(speech_type == "speech",
         party != "Federalist") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3)

#separate the trigram into columns and remove stop words
trigram_separated <- trigram %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ")

trigram_filtered <- trigram_separated %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !word1 %in% my_stop_words$word,
         !word2 %in% my_stop_words$word,
         !word3 %in% my_stop_words$word)

#most common trigrams
trigram_filtered %>%
  count(word1, word2,word3, sort = T)


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

#visualize bigrams  biden, trump, obama, bush, clinton, and bush according to tf
bigram_tf_idf %>%
  group_by(president) %>%
  filter(president %in% c("George W. Bush", "Donald J. Trump", "Barack Obama",
                          "Joseph R. Biden", "William J. Clinton", "George Bush")) %>%
  mutate(president = factor(president, levels = c("George Bush", "William J. Clinton", "George W. Bush",
                                                  "Barack Obama", "Donald J. Trump", "Joseph R. Biden"))) %>%
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
  scale_fill_manual(values = c("#0015BC", "#E9141D")) +
  labs(title = "Sentiment Scores of Every SOTU Speech since 1913",
       x = "Year",
       y = "Sentiment", 
       fill = "Party") +
  theme_bw() +
  geom_hline(yintercept = 0)

presidential_sentiment %>%
  arrange(-sentiment)


