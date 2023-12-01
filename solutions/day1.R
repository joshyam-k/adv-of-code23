library(tidyverse)
library(here)

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

singles <- data.frame(
  word = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"),
  num = 1:9
)

overlaps <- expand.grid(c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"),
            c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")) %>% 
  filter(str_sub(Var1, -1) == str_sub(Var2, 0, 1))


input %>% 
  mutate(x = str_replace_all(x, "eightwo", "82")) %>% 
  mutate(x = str_replace_all(x, "twone", "21")) %>% 
  mutate(x = str_replace_all(x, "oneight", "18")) %>% 
  mutate(x = str_replace_all(x, "eighthree", "83")) %>% 
  mutate(x = str_replace_all(x, "sevenine", "79")) %>% 
  mutate(x = str_replace_all(x, "threeight", "38")) %>%
  mutate(x = str_replace_all(x, "fiveight", "58")) %>%
  mutate(x = str_replace_all(x, "nineight", "98")) %>%
  mutate(x = str_replace_all(x, "one", "1")) %>% 
  mutate(x = str_replace_all(x, "two", "2")) %>% 
  mutate(x = str_replace_all(x, "three", "3")) %>% 
  mutate(x = str_replace_all(x, "four", "4")) %>% 
  mutate(x = str_replace_all(x, "five", "5")) %>% 
  mutate(x = str_replace_all(x, "six", "6")) %>% 
  mutate(x = str_replace_all(x, "seven", "7")) %>% 
  mutate(x = str_replace_all(x, "eight", "8")) %>% 
  mutate(x = str_replace_all(x, "nine", "9")) %>% 
  mutate(nums = str_extract_all(x, "\\d")) %>% 
  pull(nums) %>% 
  map_dbl(.f = function(x) {
    len <- length(x)
    first <- x[1]
    last <- x[len]
    as.numeric(paste0(first,last))
  }) %>% 
  sum()

