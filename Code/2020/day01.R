# https://adventofcode.com/2020
# 
# 
# 
library(tidyverse)

# day 01 --------------------------------------------------------------------------------------

# test dataset

exp <- tibble(value = c(1721,
                        979,
                        366,
                        299,
                        675,
                        1456))

# real dataset
# 
exp <- as.tibble(read_lines("Varia/Data/aoc20_01.txt"))
  
# part 1
# find entries which sum up to 2020

expand_grid(x = exp$value, y = exp$value) %>%
  mutate(x = as.numeric(x), y = as.numeric(y)) %>%
  mutate(sum = x + y) %>%
  filter(sum == 2020) %>%
  mutate(prod = x * y)

# part 1
# find entries which sum up to 2020

expand_grid(x = exp$value, y = exp$value, z = exp$value) %>%
  mutate(x = as.numeric(x), y = as.numeric(y), z = as.numeric(z)) %>%
  mutate(sum = x + y + z) %>%
  filter(sum == 2020) %>%
  mutate(prod = x * y * z)        

