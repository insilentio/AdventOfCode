# https://adventofcode.com/2020
# 
# 
# 
library(tidyverse)


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


