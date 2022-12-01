# https://adventofcode.com/2021
# 
# 
# day 01
#


# part 1 ------------------------------------------------------------------

library(tidyverse)

# real dataset
calories <- as_tibble(read_lines("Data/aoc22_01.txt")) %>%
  mutate(value = as.integer(value))


id <- 1
for (i in 1:nrow(calories)) {
  if (!is.na(calories$value[i])) {
    calories$ID[i] = id
  } else {
    id <- id + 1
    calories$ID[i] = NA
  }
}

calories %>%
  group_by(ID) %>%
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  head(1)
