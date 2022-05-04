# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 07 --------------------------------------------------------------------------------------

value <- c("16,1,2,0,4,2,7,1,2,14")
value <- readLines("Data/aoc21_07.txt")

pos <- as.numeric(unlist(str_split(value, ",")))

median(pos)

fuel <- function(pos, target) {
  dist_to_cover <- pos - target
  return(dist_to_cover)
}
for (i in min(pos):max(pos)) {
  df <- tibble(fuel = sum(abs(fuel(pos, i))), pos = i)
  if (i == min(pos))
    calc <- df else
      calc <- calc %>% add_row(df)
}
calc %>% arrange(fuel)

sumup <- Vectorize(function(n) {
  sum(1:n)
}
)
for (i in min(pos):max(pos)) {
  temp <- abs(fuel(pos, i))
  
  df <- tibble(fuel = sum(sumup(temp)), pos = i)
  if (i == min(pos))
    calc <- df else
      calc <- calc %>% add_row(df)
}
calc %>% arrange(fuel)
t1 <- 0
