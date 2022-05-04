# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 04 --------------------------------------------------------------------------------------
rand <- c(50,68,2,1,69,32,87,10,31,21,78,23,62,98,16,99,65,35,27,96,66,26,74,72,45,52,81,60,38,57,54,19,18,77,71,29,51,41,22,6,58,5,42,92,85,64,94,12,83,11,17,14,37,36,59,33,0,93,34,70,97,7,76,20,3,88,43,47,8,79,80,63,9,25,56,75,15,4,82,67,39,30,89,86,46,90,48,73,91,55,95,28,49,61,44,84,40,53,13,24)

boards <- read_lines("Data/aoc21_04.txt")
boards <- boards[boards != ""]

build_matrix <- function(text, nr = 5) {
  text <- gsub("\n", " ", text)
  text <- na.omit(as.numeric(unlist(str_split(text, " "))))
  matrix(text, nrow = nr, byrow = TRUE)
} 

bingo <- function(b) {
  c <- b * FALSE
  i <- 1
  found <- FALSE
  while (!found) {
    c <- c | (b == rand[i])
    if (sum(c(addmargins(c)[1:5,6], addmargins(c)[6,1:5]) == 5) >= 1) {
      found <- TRUE
    } else {
      i <- i + 1
      if (i > length(rand)) found <- TRUE
    }
  }
  tibble(score = sum(b * !c) * rand[i], multi = i)
}

i <- 1
while (i <= length(boards)) {
  m <- build_matrix(boards[i:(i+4)])
  b <- bingo(m)
  i <- i + 5
  if (exists("mybingo"))
    mybingo <- add_row(mybingo, b) else
      mybingo <- b
}

mybingo %>%
  arrange(multi) %>%
  slice(c(1, 100)) %>%
  select(1)
