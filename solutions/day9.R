library(tidyverse)
library(here)

source(here("get_input.R"))

input_raw <- get_input(9)

input <- input_raw %>% 
  mutate(vals = str_extract_all(input, "-?\\d+")) %>% 
  mutate(vals = map(vals, .f = as.numeric)) %>% 
  select(-input)

recurse_diff <- function(x, ...) {
  diff_vec <- diff(x)
  if (all(diff_vec == 0)) {
    return(done(diff_vec))
  } else {
    return(diff_vec)
  }
}

get_each_last <- function(x) {
  accumulate(.x = 1:length(x), .f = recurse_diff, .simplify = FALSE, .init = x) %>% 
    map(.f = last) %>% 
    unlist() %>% 
    sum()
}


# part 1
input %>% 
  mutate(next_num = map_dbl(.x = vals, .f = get_each_last)) %>% 
  summarise(answer = sum(next_num))


# part 2

input %>% 
  mutate(vals = map(vals, .f = rev)) %>% 
  mutate(next_num = map_dbl(.x = vals, .f = get_each_last)) %>% 
  summarise(answer = sum(next_num))





