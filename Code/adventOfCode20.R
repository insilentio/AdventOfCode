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

# day 02 --------------------------------------------------------------------------------------

# test dataset

pwlist <- tibble(cnt = c("1-3", "1-3", "2-9"),
                 let = c("a", "b", "c"),
                 pw = c("abcde", "cdefg", "ccccccccc"))

# real dataset
# 
pwlist <- as.tibble(read_delim("Varia/Data/aoc20_02.txt", " ", col_names = c("cnt", "let", "pw")))

pwlist <- pwlist %>%
  mutate(let = substr(let, 1, 1)) %>%
  mutate(lower = as.numeric(str_extract(cnt, "[0-9]*"))) %>% 
  mutate(upper = as.numeric(str_remove(str_extract(cnt, "-[0-9]*"), "-")))

# part 1
pwlist %>%
  mutate(found = str_count(pw, let)) %>%
  mutate(valid =  (found >= lower & found <= upper)) %>%
  summarise(sum = sum(valid))

# part 2
pwlist %>%
  mutate(found1 = str_sub(str_sub(pw, lower), 1, 1) == let) %>%
  mutate(found2 = str_sub(str_sub(pw, upper), 1, 1) == let) %>%
  mutate(valid = xor(found1, found2)) %>%
  summarise(sum = sum(valid))


# day 03 --------------------------------------------------------------------------------------
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
