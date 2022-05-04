# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

# day 01 --------------------------------------------------------------------------------------

# real dataset
depths <- as.tibble(read_lines("Data/aoc21_01_1.txt"))

# part 1
depths %>%
  mutate(value = as.numeric(value)) %>%
  mutate(diff = value - lag(value)) %>%
  mutate(incr = diff > 0) %>%
  summarise(n_incr = sum(incr, na.rm = TRUE))

# part 2
depths %>%
  mutate(value = as.numeric(value)) %>%
  mutate(slide1 =  lag(value, 1), slide2 = lag(value, 2)) %>%
  mutate(window = value + slide1 + slide2) %>%
  mutate(diff = window - lag(window)) %>%
  mutate(incr = diff > 0) %>%
  summarise(n_incr = sum(incr, na.rm = TRUE))  
  

# day 02 --------------------------------------------------------------------------------------
# real dataset
move <- as.tibble(read_lines("Data/aoc21_02.txt"))

# part 1
temp <- move %>%
  mutate(dir = substr(value, 1, 1)) %>%
  mutate(amount = as.numeric(word(value, 2, 2))) %>%
  mutate(x = ifelse(dir == "f", amount, 0)) %>%
  mutate(y = case_when(dir == "d" ~ amount,
                       dir == "u" ~ -amount,
                       TRUE ~ 0)) %>%
  summarise(x = sum(x), y = sum(y))

temp$x * temp$y  

# part 2
temp <- move %>%
  mutate(dir = substr(value, 1, 1)) %>%
  mutate(amount = as.numeric(word(value, 2, 2))) %>%
  mutate(x = ifelse(dir == "f", amount, 0)) %>%
  mutate(y_change = case_when(dir == "d" ~ amount,
                       dir == "u" ~ -amount,
                       TRUE ~ 0)) %>%
  mutate(aim = cumsum(y_change)) %>%
  mutate(y = x * (aim)) %>%
  summarise(x = sum(x), y = sum(y))

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


# day 08 --------------------------------------------------------------------------------------

value <- readLines("Data/aoc21_08.txt")

digits <- tibble(input = gsub(" \\|.+", "", value), output = gsub(".+\\| ", "", value))
outputs <- digits %>%
  select(output) %>%
  mutate(o1 = str_replace(output, " .+", "")) %>%
  mutate(o2 = str_replace(str_replace(output, paste0(o1, " "), ""), " .+", "")) %>%
  mutate(o3 = str_replace(str_replace(output, paste0(o1, " ", o2, " "), ""), " .+", "")) %>%
  mutate(o4 = str_replace(output, ".+ ", "")) %>%
  mutate(co1 = nchar(o1), co2 = nchar(o2), co3 = nchar(o3), co4 = nchar(o4))

known <- c(2,3,4,7)
mydata %>%
  summarize(known1 = sum(co1 %in% known),
            known2 = sum(co2 %in% known),
            known3 = sum(co3 %in% known),
            known4 = sum(co4 %in% known)) %>%
  sum()


# part 2
a <- str_split(digits$input, "( )|(?<=[a-z])(?=[a-z])")
dict <- data.frame(t(sapply(a, function(x) {table((factor(x, levels = letters[1:7])))})))
dict$AB <- str_extract(digits$input, "\\b[a-z]{2}\\b")
dict$FOUR <- str_extract(digits$input, "\\b[a-z]{4}\\b")
dict[dict == 4] <- "g"
dict[dict == 9] <- "b"
dict[dict == 6] <- "e"
dict <- dict %>%
  mutate(real_b = case_when(
    a == "b" ~ "a",
    b == "b" ~ "b",
    c == "b" ~ "c",
    d == "b" ~ "d",
    e == "b" ~ "e",
    f == "b" ~ "f",
    g == "b" ~ "g"
  )) %>%
  mutate(real_a = str_replace(AB, real_b, "")) %>%
  mutate(aCol = match(real_a, letters[1:7]))
for (i in 1:nrow(dict)) {
  j <- dict[i, "aCol"]
  dict[i, j] <- "a"
}
dict[dict == 8] <- "d"
dict <- dict %>%
  mutate(real_f = case_when(
    a == "7" & str_detect(FOUR, "a") ~ "a",
    b == "7" & str_detect(FOUR, "b") ~ "b",
    c == "7" & str_detect(FOUR, "c") ~ "c",
    d == "7" & str_detect(FOUR, "d") ~ "d",
    e == "7" & str_detect(FOUR, "e") ~ "e",
    f == "7" & str_detect(FOUR, "f") ~ "f",
    g == "7" & str_detect(FOUR, "g") ~ "g"
  )) %>%
  mutate(fCol = match(real_f, letters[1:7]))
for (i in 1:nrow(dict)) {
  j <- dict[i, "fCol"]
  dict[i, j] <- "f"
}
dict[dict == 7] <- "c"
dict <- dict %>%
  mutate(real_c = case_when(
    a == "c" ~ "a",
    b == "c" ~ "b",
    c == "c" ~ "c",
    d == "c" ~ "d",
    e == "c" ~ "e",
    f == "c" ~ "f",
    g == "c" ~ "g"
  )) %>%
  mutate(real_d = case_when(
    a == "d" ~ "a",
    b == "d" ~ "b",
    c == "d" ~ "c",
    d == "d" ~ "d",
    e == "d" ~ "e",
    f == "d" ~ "f",
    g == "d" ~ "g"
  )) %>%
  mutate(real_e = case_when(
    a == "e" ~ "a",
    b == "e" ~ "b",
    c == "e" ~ "c",
    d == "e" ~ "d",
    e == "e" ~ "e",
    f == "e" ~ "f",
    g == "e" ~ "g"
  )) %>%
  mutate(real_g = case_when(
    a == "g" ~ "a",
    b == "g" ~ "b",
    c == "g" ~ "c",
    d == "g" ~ "d",
    e == "g" ~ "e",
    f == "g" ~ "f",
    g == "g" ~ "g"
  )) %>%
  select(letters[1:7], paste0("real_", letters[1:7]))


sortl <- Vectorize(function(x) {
  splitted <- unlist(str_split(x, "( )|(?<=[a-z])(?=[a-z])"))
  paste(sort(splitted), collapse = "")
}
)

dictN <- dict %>%
  mutate(N1 = paste0(real_a, real_b)) %>%
  mutate(N2 = paste0(real_a, real_c, real_d, real_f, real_g)) %>%
  mutate(N3 = paste0(real_a, real_b, real_c, real_d, real_f)) %>%
  mutate(N4 = paste0(real_a, real_b, real_e, real_f)) %>%
  mutate(N5 = paste0(real_b, real_c, real_d, real_e, real_f)) %>%
  mutate(N6 = paste0(real_b, real_c, real_d, real_e, real_f, real_g)) %>%
  mutate(N7 = paste0(real_a, real_b, real_d)) %>%
  mutate(N8 = paste0(real_a, real_b, real_c, real_d, real_e, real_f, real_g)) %>%
  mutate(N9 = paste0(real_a, real_b, real_c, real_d, real_e, real_f)) %>%
  mutate(N0 = paste0(real_a, real_b, real_c, real_d, real_e, real_g)) %>%
  select(-c(1:14)) %>%
  bind_cols(select(outputs, 2:5)) %>%
  mutate(across(everything(), sortl))

for (i in 1:nrow(dictN)) {
  dictN[i, 11] = match(dictN[i,11], dictN[i,1:10])
  dictN[i, 12] = match(dictN[i,12], dictN[i,1:10])
  dictN[i, 13] = match(dictN[i,13], dictN[i,1:10])
  dictN[i, 14] = match(dictN[i,14], dictN[i,1:10])
}

dictN %>%
  mutate(across(c(o1, o2, o3, o4), function(x) {sub("10", "0", x)})) %>%
  mutate(out = as.numeric(paste0(o1, o2, o3, o4))) %>%
  select(out) %>%
  summarise(sum = sum(out))



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

