
years <- as.character(0:2020)
my_stop_words <- tibble(word = c(years, "tonight", "america's", "america", "ms", "announcing", "ira's", "guy", "brady",
                                 "al", "11th", "kayla", "c.j", "ellie", "92d", "nam"))

common_words <- tibble(word = c("world", "american", "people", "americans", "country", "congress", "nation",
                                "government", "time", "united"))

race_terms <- c("black", "blacks", "african", "africans", "negro", "negros", "hispanic", "hispanics",
                "latino", "latinos", "mexican", "mexicans", "latinx", "asian", "asians", "orient", "oriental",
                "chinese", "chinamen", "orientals", "japanese", "indian", "indians", "seminole", "seminoles",
                "native", "natives", "indigenous", "cherokee", "apache", "apaches", "arab", "arabs", "muslims", "jews",
                "islamic", "jewish")

black_terms <- c("black", "blacks", "african", "africans", "negro", "negros", "afro-american", "afro", "colored",
                 "reparations", "segregation", "integration", "racist", "racism", "racists")

hispanic_terms <- c("hispanic", "hispanics","latino", "latinos", "mexican", "mexicans", "latinx")

asian_terms <- c("asian", "asians", "orient", "oriental","chinese", "chinamen", "orientals", 
                 "japanese", "vietnamese", "filipino", "filipinos")

nativeamerican_terms <- c("indian", "indians", "seminole", "seminoles", "indigenous", "native", "natives",
                          "cherokee", "cherokees", "apache", "apaches")

jewish_terms <- c("jew", "jewish", "jews", "hebrews")

muslim_terms <- c("muslim", "islam", "islamic", "muslims")

