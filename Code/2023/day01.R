# https://adventofcode.com/2023
# 
# day 01
#

# part 1 ------------------------------------------------------------------
# 
digits <- c(1:9)
names(digits) <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

convert <- function(x){
  if (x %in% names(digits)) {
    digits[x]
  }
  else
    as.numeric(x)
}

finddigits <- function(input){
  ex <- str_extract_all(input, "[0-9]|one|two|three|four|five|six|seven|eight|nine")
  ex1 <- map(ex, 1)
  ex2 <- map(ex, length)
  
  ex3 <- rep(NA, times = 4)
  for (i in seq_along(ex2)) {
    ex3[i] <- ex[[i]][ex2[[i]]]
  }
  
  ex1b <- map(ex1, convert) |> unlist()
  ex3b <- map(ex3, convert) |> unlist()
  
  paste0(ex1b, ex3b) |> as.numeric()
}

digs <- finddigits(read_lines("Data/aoc23_01.txt"))
sum(digs)
