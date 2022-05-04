# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 02 --------------------------------------------------------------------------------------
# real dataset
move <- as.tibble(read_lines("Data/aoc21_02.txt"))

# part 1
temp <- move %>%
  mutate(dir = substr(value, 1, 1)) %>%
  mutate(amount = as.numeric(word(value, 2, 2))) %>%
  mutate(x = ifelse(dir == "f", amount, 0)) %>%
  mutate(y = case_when(dir == "d" ~ amount,
                       dir == "u" ~ -amount,
                       TRUE ~ 0)) %>%
  summarise(x = sum(x), y = sum(y))

temp$x * temp$y  

# part 2
temp <- move %>%
  mutate(dir = substr(value, 1, 1)) %>%
  mutate(amount = as.numeric(word(value, 2, 2))) %>%
  mutate(x = ifelse(dir == "f", amount, 0)) %>%
  mutate(y_change = case_when(dir == "d" ~ amount,
                              dir == "u" ~ -amount,
                              TRUE ~ 0)) %>%
  mutate(aim = cumsum(y_change)) %>%
  mutate(y = x * (aim)) %>%
  summarise(x = sum(x), y = sum(y))
