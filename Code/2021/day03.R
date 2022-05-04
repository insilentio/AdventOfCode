# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 03 --------------------------------------------------------------------------------------
library(janitor)
# real dataset
diag <- as.tibble(read_lines("Data/aoc21_03.txt"))

# part 1
#
nr <- nrow(diag)
diag1 <- as_tibble(str_split(diag$value, ""), .name_repair = "unique")
colnames(diag1) <- paste0("r", str_extract(colnames(diag1), "[0-9]+"))

diag1 <- diag1 %>%
  mutate(across(.fns = as.numeric)) %>%
  adorn_totals(name = "Ones", where = "col") %>%
  mutate(Zeroes = nr - Ones) %>%
  mutate(gamma = ifelse(Ones > Zeroes, 1, 0)) %>%
  mutate(epsilon = ifelse(Ones > Zeroes, 0, 1))

gamma <- strtoi(paste(as.character(t(diag1$gamma)), collapse = ""), base = 2)
epsilon <- strtoi(paste(as.character(t(diag1$epsilon)), collapse = ""), base = 2)
gamma * epsilon

# part 2
diag2 <- diag
i <- 1
while (nrow(diag2) > 1) {
  filters <- diag2 %>%
    mutate(i = as.numeric(str_sub(value, i, i))) %>%
    summarise(n = n(), ones = sum(i) >= n/2)
  
  diag2 <- diag2 %>%
    filter(str_sub(value, i, i) == filters$ones*1)
  
  i <- i + 1
}
o2 <- strtoi(diag2$value, base = 2)


diag3 <- diag
i <- 1
while (nrow(diag3) > 1) {
  filters <- diag3 %>%
    mutate(i = as.numeric(str_sub(value, i, i))) %>%
    summarise(n = n(), zeroes = sum(i) < n/2)
  
  diag3 <- diag3 %>%
    filter(str_sub(value, i, i) == filters$zeroes*1)
  
  i <- i + 1
}
co2 <- strtoi(diag3$value, base = 2)

o2 * co2
