# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 09 --------------------------------------------------------------------------------------
value <- tibble( ints = c(
  2199943210,
  3987894921,
  9856789892,
  8767896789,
  9899965678)
)
value <- tibble(ints= readLines("Data/aoc21_09.txt"))

map <- as.matrix(value %>%
                   separate(ints, into = paste0("i",  1:nchar(value[1,])), sep = "(?<=[0-9])(?=[0-9])") %>%
                   mutate(across(everything(), as.numeric)))

sizex <- length(map)/nrow(map)
sizey <- nrow(map)
lowmap <- !map
for (i in 1:(sizex)) {
  for (j in 1:sizey) {
    if (i < sizex) right <- map[j,i] < map[j,i+1] else right <- TRUE
    if (i > 1) left <- map[j,i] < map[j,i-1] else left <- TRUE
    if (j < sizey) top <- map[j,i] < map[j+1,i] else top <- TRUE
    if (j > 1) bottom <-  map[j,i] < map[j-1,i] else bottom <- TRUE
    
    lowmap[j,i] <- right & left & top & bottom
  }
}

risk <- lowmap * map + lowmap * 1
sum(risk)

# part 2
basin <- map
basin[basin == 9] <-"T"
basin[lowmap] <- "L"
k <- 1
for (i in 1:length(basin)) {
  if (basin[i] == "L") {
    basin[i] <- paste0("L", as.character(k))
    k <- k + 1
  }
}

while (sum(grepl("^[1-8]", basin)) > 0) {
  for (i in 1:(sizex)) {
    for (j in 1:sizey) {
      if (basin[j, i] %in% as.character(1:8)) {
        if (i < sizex) if (grepl("L", basin[j,i+1])) basin[j, i] <- basin[j,i+1]
        if (i > 1) if (grepl("L", basin[j,i-1])) basin[j, i] <- basin[j,i-1]
        if (j < sizey) if (grepl("L", basin[j+1,i])) basin[j, i] <- basin[j+1,i]
        if (j > 1) if (grepl("L", basin[j-1,i])) basin[j, i] <- basin[j-1,i]
      }
    }
  }
}
basin_sizes <- tibble(k = NA, n = NA, .rows = 0)
for (i in 1:(k-1)) {
  basin_sizes <- basin_sizes %>%
    add_row(k = i, n = length(grep(paste0("L", k, "\\b"), basin)))
}

basin_sizes %>%
  arrange(desc(n)) %>%
  head(3) %>%
  summarize(prod = prod(n))
