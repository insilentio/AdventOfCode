# https://adventofcode.com/2021
# 
# 
# 
library(tidyverse)

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
