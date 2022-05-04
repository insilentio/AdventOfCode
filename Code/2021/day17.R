# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 17 --------------------------------------------------------------------------------------

target <- expand.grid(x = seq(20, 30, 1), y = seq(-15, -5, 1))

decrease <- function(velo) {
  velo$x <- sign(velo$x) * (abs(velo$x) - 1)
  velo$y <- velo$y - 1
  return(velo)
}

outside <- function(new, old) {
  x <- y <- FALSE
  if (new$x < min(target$x) & old$x > max(target$x)) x <- TRUE
  if (new$x > max(target$x) & old$x < min(target$x)) x <- TRUE
  if (new$y < min(target$y) & old$y > max(target$y)) y <- TRUE
  if (new$y > max(target$y) & old$y < min(target$y)) y <- TRUE
  return(x | y)
}

traject <- function(speed) {
  pos <- data.frame(x = 0, y = 0)
  solution_found <- FALSE
  max_y <- pos$y
  
  test <- TRUE
  while (test) {
    pos_new <- pos + speed
    speed <- decrease(speed)
    
    if (pos_new$y > max_y) max_y <- pos_new$y
    
    if (pos_new$x %in% target$x & pos_new$y %in% target$y) {
      solution_found <- TRUE
      test <- FALSE
    }
    
    if (outside(pos_new, pos))
      test <- FALSE else 
        pos <- pos_new
  }
  return(list(found = solution_found, max_y = max_y))
}

# above ex.: x at least 6:15, max 30
# y at least -10, max 10-1
test_grid <- expand.grid(x = 6:30, y = -10:9)
for (i in 1:nrow(test_grid)) {
  test <- traject(test_grid[i,])
  if (test$found)
    print(paste0("max y for ", i, " is ", test$max_y))
}



valid_y <- Vectorize(function(y) {
  rg <-  cumsum(1:y)[y] + abs(unique(target$y))
  valids <- rg %in% cumsum(1:max(rg))
  return(sum(valids == TRUE) > 0)
})

