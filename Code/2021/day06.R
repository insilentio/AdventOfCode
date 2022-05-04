# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 06 --------------------------------------------------------------------------------------

value <- c("3,4,3,1,2")
value <- readLines("Data/aoc21_06.txt")

fishes <- as.numeric(unlist(str_split(value, ",")))

lanternfish <- function(x, days = 80) {
  fish <- as.double(table(factor(x, levels = 0:8)))
  for (i in 1:days)
    fish <- c(fish[2:7], fish[8] + fish[1], fish[9], fish[1])
  sum(fish)
}
count <- lanternfish(fishes, 256)
