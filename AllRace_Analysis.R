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
