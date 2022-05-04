# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 11 --------------------------------------------------------------------------------------

value <- tibble( ints = c(5483143223,
                          2745854711,
                          5264556173,
                          6141336146,
                          6357385478,
                          4167524645,
                          2176841721,
                          6882881134,
                          4846848554,
                          5283751526))
value <- tibble(ints = readLines("Data/aoc21_11.txt"))

# functions

get_neighbours <- function(pos, rows, cols) {
  # check for edge position
  top_ok <- bottom_ok <- left_ok <- right_ok <- TRUE
  if (pos %% rows == 1) top_ok <- FALSE
  if (pos %% rows == 0) bottom_ok <- FALSE
  if (pos <= rows) left_ok <- FALSE
  if (pos > (cols-1)*rows) right_ok <- FALSE
  
  nb <- c()
  # upper and lower
  if (top_ok) nb <- c(nb, pos-1)
  if (bottom_ok) nb <- c(nb, pos + 1)
  if (left_ok) nb <- c(nb, pos-rows)
  if (right_ok) nb <- c(nb, pos+rows)
  
  # diag upper
  if (top_ok & left_ok) nb <- c(nb, pos-rows-1)
  if (top_ok & right_ok) nb <- c(nb, pos+rows-1)
  
  # diag lower
  if (bottom_ok & left_ok) nb <- c(nb, pos-rows+1)
  if (bottom_ok & right_ok) nb <- c(nb, pos+rows+1)
  
  return(nb)
}

# part 1
octo <- as.matrix(value %>%
                    separate(ints, into = paste0("i",  1:nchar(value[1,])),
                             sep = "(?<=[0-9])(?=[0-9])") %>%
                    mutate(across(everything(), as.numeric)))
rows <- nrow(octo)
cols <- ncol(octo)
flash <- 0
sync <- 0
for (i in 1:1100) {
  octo <- octo + 1
  tens <- grep("[0-9]{2,}", octo)
  
  while (length(tens) > 0) {
    for (j in 1:length(tens)) {
      nb <- get_neighbours(tens[j], rows, cols)
      nb <- nb[octo[nb] != -1]
      octo[nb] <- octo[nb] + 1
      octo[tens[j]] <- -1
      flash <- flash + 1
    }
    tens <- grep("[0-9]{2,}", octo)
  }
  octo[octo == -1] <- 0
  if (length(octo[octo == 0]) == rows*cols & sync == 0) sync <- i
}

# part 2
# added last row in for loop above
