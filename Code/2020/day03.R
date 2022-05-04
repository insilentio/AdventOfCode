# https://adventofcode.com/2020
# 
# 
# 
library(tidyverse)

# day 03 --------------------------------------------------------------------------------------
# 
# test dataset
map <- tibble(value = c("..##.......",
        "#...#...#..",
        ".#....#..#.",
        "..#.#...#.#",
        ".#...##..#.",
        "..#.##.....",
        ".#.#.#....#",
        ".#........#",
        "#.##...#...",
        "#...##....#",
        ".#..#...#.#"))

# real dataset
map <- tibble(value = read_lines("Varia/Data/aoc20_03.txt"))

p_len <- map %>%
  mutate(length = str_length(value)) %>%
  distinct(length)
p_len <- p_len$length

steps_x <- 3

sequence <- seq(1, p_len * steps_x, by = steps_x) %% p_len
sequence[sequence == 0] <- p_len

map %>%
  mutate(pos = rep(sequence, length.out = n())) %>%
  mutate(found = str_sub(value, pos, pos) == "#") %>%
  summarise(sum = sum(found))

# part 2
map_func <- function(steps, data) {
  sequence <- seq(1, p_len * steps, by = steps) %% p_len
  sequence[sequence == 0] <- p_len
  
  data %>%
    mutate(pos = rep(sequence, length.out = n())) %>%
    mutate(found = str_sub(value, pos, pos) == "#") %>%
    summarise(sum = sum(found))
  
}

v1 <- unlist(sapply(c(1, 3, 5, 7), map_func, data = map))

map2 <- map %>%
  slice(seq(1, nrow(map), by = 2))

v2 <- map_func(1, map2)
v2 %>%
  add_row(sum = v1) %>%
  summarise(prod = prod(sum))
