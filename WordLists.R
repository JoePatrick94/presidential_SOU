my_stop_words <- tibble(word = c(years, "tonight", "america's", "america", "ms", "announcing", "ira's", "guy", "brady",
                                 "al", "11th", "kayla", "c.j", "ellie", "toâ", "actâ", "92d", "war.we", "nations.we", "nam"))

race_terms <- c("black", "blacks", "african", "africans", "negro", "negros", "hispanic", "hispanics",
                "latino", "latinos", "mexican", "mexicans", "latinx", "asian", "asians", "orient", "oriental",
                "chinese", "chinamen", "orientals", "japanese", "indian", "indians", "seminole", "seminoles")

black_terms <- c("black", "blacks", "african", "africans", "negro", "negros")

hispanic_terms <- c("hispanic", "hispanics","latino", "latinos", "mexican", "mexicans", "latinx")

asian_terms <- c("asian", "asians", "orient", "oriental","chinese", "chinamen", "orientals", 
                 "japanese", "vietnamese", "filipino", "filipinos")

nativeamerican_terms <- c("indian", "indians", "seminole", "seminoles", "indigenous", "native", "natives",
                          "cherokee", "cherokees", "apache", "apaches")

jewish_terms <- c("jew", "jewish", "jews", "hebrews")

muslim_terms <- c("muslim", "islam", "islamic", "muslims")

