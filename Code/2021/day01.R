# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 01 --------------------------------------------------------------------------------------

# real dataset
depths <- as.tibble(read_lines("Data/aoc21_01_1.txt"))

# part 1
depths %>%
  mutate(value = as.numeric(value)) %>%
  mutate(diff = value - lag(value)) %>%
  mutate(incr = diff > 0) %>%
  summarise(n_incr = sum(incr, na.rm = TRUE))

# part 2
depths %>%
  mutate(value = as.numeric(value)) %>%
  mutate(slide1 =  lag(value, 1), slide2 = lag(value, 2)) %>%
  mutate(window = value + slide1 + slide2) %>%
  mutate(diff = window - lag(window)) %>%
  mutate(incr = diff > 0) %>%
  summarise(n_incr = sum(incr, na.rm = TRUE))  
  