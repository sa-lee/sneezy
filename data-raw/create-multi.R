library(readr)
library(dplyr)

# this is example highD data from 
# http://ifs.tuwien.ac.at/dm/dataSets.html
# it consists of five subsets of 200 observations
multi <- read_tsv("http://ifs.tuwien.ac.at/dm/download/multiChallenge-matrix.txt",
                col_names = FALSE) %>% 
  mutate(
    index = rep(seq_len(200), each = 5),
    key = rep(LETTERS[1:5], each = 200L)
  ) %>%
  select(key, index, X1:X10)



usethis::use_data(multi)
