library(tidyverse)
library(here)
library(stringi)

input <- tibble(x = readLines(here("inputs", "day1.txt")))

# part 1

input %>% 
  mutate(nums = str_extract_all(x, "\\d")) %>% 
  pull(nums) %>% 
  map_dbl(.f = function(x) {
    len <- length(x)
    first <- x[1]
    last <- x[len]
    as.numeric(paste0(first,last))
  }) %>% 
  sum()



# part 2
# rely on stringi

word <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
# create "or" regex to match spelled out numbers
search <- paste0(word, collapse = "|")

input %>% 
  mutate(
    first = stri_extract_first_regex(x, paste0("(\\d|", search, ")")),
    last = stri_extract_last_regex(x, paste0("(\\d|", search, ")"))
    ) %>% 
  rowwise() %>% 
  mutate(first = na.exclude(c(as.numeric(first), match(first, word))),
         last = na.exclude(c(as.numeric(last), match(last, word))),
         num = as.numeric(paste0(first, last))) %>% 
  pull(num) %>% 
  sum()

