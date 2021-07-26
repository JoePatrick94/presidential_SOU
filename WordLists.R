
years <- as.character(0:2020)
leadingzeros <- sprintf('%0.2d', 0:10)
trailing_M <- paste0(0:10,"m")

my_stop_words <- tibble(word = c(years, leadingzeros, trailing_M, "tonight", "america's", "america", "ms", "announcing", "ira's", "guy", "brady",
                                 "al", "11th", "kayla", "c.j", "ellie", "92d", "nam", "united", "chargã",
                                 "00of", "â"))

common_words <- tibble(word = c("world", "american", "people", "americans", "country", "congress", "nation",
                                "government", "time", "united", "public", "national"))

race_terms <- c("black", "blacks", "african", "africans", "hispanic", "hispanics",
                "latino", "latinos", "mexican", "mexicans", "latinx", "asian", "asians", "orient", "oriental",
                "chinese", "chinamen", "orientals", "japanese", "indian", "indians", "seminole", "seminoles",
                "native", "natives", "indigenous", "cherokee", "apache", "apaches", "arab", "arabs", "muslims", "jews",
                "islamic", "jewish")

black_terms <- c("black", "blacks", "african", "africans", "afro-american", "afro", "colored",
                 "reparations", "segregation", "racist", "racism", "racists", "slave", "slavery", "slaves",
                 "slaveholding")

hispanic_terms <- c("hispanic", "hispanics","latino", "latinos", "mexican", "mexicans", "latinx")

asian_terms <- c("asian", "asians", "orient", "oriental","chinese", "chinamen", "orientals", 
                 "japanese", "vietnamese", "filipino", "filipinos")

nativeamerican_terms <- c("indian", "indians", "seminole", "seminoles", "indigenous", "native", "natives",
                          "cherokee", "cherokees", "apache", "apaches")

jewish_terms <- c("jew", "jewish", "jews", "hebrews", "holocaust")

muslim_terms <- c("muslim", "islam", "islamic", "muslims")

healthcare_terms <- c("healthcare", "hospital", "health", "medicare", "medicaid", "medicine", "surgery", "disease",
                      "virus")

gun_terms <- c("gun", "arms", "2nd", "weapon", "pistol", "rifle")

education_terms <- c("education", "school", "college", "university", "educate")

abortion_terms <- c("abortion", "abort", "fetus")

