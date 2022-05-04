# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 10 --------------------------------------------------------------------------------------

value <- c("[({(<(())[]>[[{[]{<()<>>",
           "[(()[<>])]({[<{<<[]>>(",
           "{([(<{}[<>[]}>{[]{[(<()>",
           "(((({<>}<{<{<>}{[]{[]{}",
           "[[<[([]))<([[{}[[()]]]",
           "[{[{({}]{}}([{[{{{}}([]",
           "{<[[]]>}<{[{[{[]{()[[[]",
           "[<(<(<(<{}))><([]([]()",
           "<{([([[(<>()){}]>(<<{{",
           "<{([{{}}[<[[[<>{}]]]>[]]")
value <- readLines("Data/aoc21_10.txt")

remove_resp <- function(text) {
  temp <- gsub("(\\(\\))|(\\[\\])|(\\{\\})|(<>)", "", text)
  return(temp)
}

score <- function(text) {
  a <- regexpr(">", text)
  b <- regexpr("\\}", text)
  c <- regexpr("\\]", text)
  d <- regexpr("\\)", text)
  
  found <- c(a, b, c, d)
  found[found == -1] <- 99
  found <- sort(found)
  
  if (found[1] == a) score  <- 25137 else
    if (found[1] == b) score <- 1197 else
      if (found[1] == c) score <- 57 else
        if (found[1] == d) score <- 3 else
          score <- 0
  return(score)
}

get_score <- function(text) {
  while (nchar(text) > 1) {
    before <- nchar(text)
    text <- remove_resp(text)
    after <- nchar(text)
    if (before == after) {
      scores <- score(text)
      break
    }
  }
  return(scores)
}

# part 1
scores <- c()
for (i in 1:length(value)) {
  scores[i] <- get_score(value[i])
}
sum(scores)

# part 2
valid <- value[scores == 0]
get_reduced <- function(text) {
  while (nchar(text) > 1) {
    before <- nchar(text)
    text <- remove_resp(text)
    after <- nchar(text)
    if (before == after) {
      break
    }
  }
  return(text)
}
get_score2 <- function(text) {
  score <- 0
  text <- get_reduced(text)
  
  while (nchar(text) > 0) {
    len <- nchar(text)
    symb <- substr(text, len, len)
    if (symb == "(") score <- (score * 5 + 1) else
      if (symb == "[") score <- score * 5 + 2 else
        if (symb == "{") score <- score * 5 + 3 else
          if (symb == "<") score <- score * 5 + 4
    text <- substr(text, 1, len-1)
  }
  return(score)
}

scores <- c()
for (i in 1:length(valid)) {
  scores[i] <- get_score2(valid[i])
}
median(scores)
