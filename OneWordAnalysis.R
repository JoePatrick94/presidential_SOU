# single token analysis of SOTU

#load libraries
library(tidytext)

#Read in data
sotu <- fread("sotu_final.csv")

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


#tf-idf
speechwords <- singleword %>%
  count(president, word, sort = T)

totalwords <- speechwords %>%
  group_by(president) %>%
  summarise(total = sum(n))

speechwords <- left_join(speechwords, totalwords)

speech_tf_idf <- speechwords %>%
  bind_tf_idf(word, president, n)

#visualize biden, trump, obama, bush, clinton
speech_tf_idf %>%
  group_by(president) %>%
  filter(president %in% c("Biden", "Donald J. Trump", "Barack Obama", "George W. Bush", "William J. Clinton")) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = president)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~president, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

