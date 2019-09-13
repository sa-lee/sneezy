library(readr)
library(dplyr)

# this is example highD data from 
# http://ifs.tuwien.ac.at/dm/dataSets.html
# it consists of five subsets of 200 observations
url <- "http://ifs.tuwien.ac.at/dm/download/multiChallenge-matrix.txt"
multi <- read_tsv(url, col_names = FALSE) %>% 
  mutate(
    index = rep(seq_len(200), 5),
    key = rep(LETTERS[1:5], each = 200L)
  ) %>%
  select(key, index, X1:X10)

# turn it into a TourExperiment object
multi <- TourExperiment(multi,
                        S4Vectors::SimpleList(),
                        S4Vectors::SimpleList(),
                        X1:X10)


usethis::use_data(multi, overwrite = TRUE)
