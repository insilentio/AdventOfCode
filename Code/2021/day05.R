# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 05 --------------------------------------------------------------------------------------

value <- read_lines("Data/aoc21_05.txt")

size <- 1000
mymatrix <- matrix(rep(0, size^2), nrow = size)

endpoints <- function(fromto) {
  str_start <- sub(" .+", "", fromto)
  str_end <- sub(".+ ", "", fromto)
  
  start <- as.numeric(sub(",.+", "", str_start))
  start[2] <- as.numeric(sub(".+,", "", str_start))
  end <- as.numeric(sub(",.+", "", str_end))
  end[2] <- as.numeric(sub(".+,", "", str_end))
  
  list(start = start, end = end)
}

xloop <- function(endpoints) {
  #matrix goes by row, then column. For x-y coordinates, we need to do a switch
  #furthermore, R starts indexing with 1,1, hence add +1 everywhere
  xs <- endpoints$start[2] + 1
  ys <- endpoints$start[1] + 1
  xe <- endpoints$end[2] + 1
  ye <- endpoints$end[1] + 1
  
  cur_matrix <- matrix(rep(0, size^2), nrow = size)
  cur_matrix[xs, ys] <- 1
  cur_matrix[xe, ye] <- 1
  
  if (ys == ye) {
    while (xs != xe) {
      if (xs < xe) 
        xs <- xs + 1
      else
        xs <- xs -1
      cur_matrix[xs, ys] <- 1    
    }
  } else if (xs == xe) {
    while (ys != ye) {
      if (ys < ye)
        ys <- ys + 1
      else
        ys <- ys -1
      cur_matrix[xs, ys] <- 1  
    }
  } else if (xs < xe & ys < ye) {
    while (xs != xe & ys != ye) {
      xs <- xs + 1
      ys <- ys + 1
      cur_matrix[xs, ys] <- 1
    }
  } else if (xs > xe & ys < ye) {
    while (xs != xe & ys != ye) {
      xs <- xs - 1
      ys <- ys + 1
      cur_matrix[xs, ys] <- 1
    }
  } else if (xs < xe & ys > ye) {
    while (xs != xe & ys != ye) {
      xs <- xs + 1
      ys <- ys - 1
      cur_matrix[xs, ys] <- 1
    }
  } else if (xs > xe & ys > ye) {
    while (xs != xe & ys != ye) {
      xs <- xs - 1
      ys <- ys - 1
      cur_matrix[xs, ys] <- 1
    }
  }
  
  cur_matrix
}

# part 1
for (i in 1:length(value)) {
  temp <- value[i]
  temp <- endpoints(temp)
  
  if (temp$start[1] != temp$end[1] & temp$start[2] != temp$end[2])
    next
  temp <- xloop(temp)
  
  mymatrix <- mymatrix + temp
}

mymatrix
sum(mymatrix >=2)

# part 2
for (i in 1:length(value)) {
  temp <- value[i]
  temp <- endpoints(temp)
  
  temp <- xloop(temp)
  
  mymatrix <- mymatrix + temp
}

mymatrix
sum(mymatrix >=2)
