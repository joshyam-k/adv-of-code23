library(tidyverse)
library(here)

source(here("get_input.R"))

input_raw <- get_input(6)

time <- str_extract_all(input_raw[1, ], "\\d+")[[1]] %>% as.numeric()

distance <- str_extract_all(input_raw[2, ], "\\d+")[[1]] %>% as.numeric()

ways_to_win <- function(.time, .distance) {
  
  charge_opts <- 1:(.time - 1)
  time_post_charge <- (.time - charge_opts) * charge_opts
  sum(time_post_charge > .distance)
   
}

# part 1

map2_dbl(.x = time, .y = distance, .f = ways_to_win) %>% prod()


# part 2

time_collapse <- paste0(time, collapse = "") %>% as.numeric()
distance_collapse <- paste0(distance, collapse = "") %>% as.numeric()

ways_to_win(time_collapse, distance_collapse)
