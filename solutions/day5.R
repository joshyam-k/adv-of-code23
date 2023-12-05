library(tidyverse)
library(here)
library(janitor)

source(here("get_input.R"))

input_raw <- get_input(5)

seeds <- input_raw[1, ] %>% 
  mutate(seeds = str_extract_all(input, "\\d+")) %>% 
  select(seeds) %>% 
  unnest(seeds) %>% 
  pull()

# destination - source - range
flow <- c("seed", "soil", "fertilizer", "water", "light", "temperature", "humidity", "location")

input_clean <- tail(input_raw, -1) %>% 
  mutate(is_empty = input == "") %>% 
  mutate(batch = paste0("g.", cumsum(is_empty != lag(is_empty, default = FALSE)))) %>% 
  mutate(batch = ifelse(batch != lag(batch), str_remove(input, "\\smap:"), NA)) %>% 
  fill(batch, .direction = "down") %>% 
  filter(!is_empty, !str_detect(input, "map")) %>% 
  select(input, batch) %>% 
  nest(.by = batch) %>% 
  mutate(data = map2(.x = data,
                     .y = batch,
                     .f = function(.x, .y) {
                       src <- str_extract(.y, ".*(?=-to)")
                       dest <- str_extract(.y, "(?<=to-).*")
                       separate(.x,
                                col = "input",
                                into = c(dest, src, "range"),
                                sep = "\\s", convert = TRUE)
                       }
                     )) %>% 
  pull(data)

names(input_clean) <- flow[-length(flow)]

# part 1
# lowest location number?

# seed -> soil -> fertilizer -> water -> light -> temperature -> humidity -> location

locs <- c()
for (i in seq_along(seeds)) {
  seed_i <- as.numeric(seeds[i])
  curr_item <- seed_i
  
  for (j in seq_along(flow[-length(flow)])) {
    flow_j <- input_clean[[flow[j]]]
    link <- flow_j %>% 
      filter(between(rep(seed_i, nrow(flow_j)), flow_j[[2]], flow_j[[2]] + flow_j[["range"]] - 1 ))
    old <- curr_item
    if (nrow(link) == 0) {
      curr_item <- curr_item
    } else {
      curr_item <- link[[1]] + (curr_item - link[[2]])
      print(paste0(flow[j], ": ", old, "-> ", flow[j + 1], ": ", curr_item))
    }
    if (j == 7) {
      locs[i] <- curr_item
    }
    
    
  }
}





  

    
  
  

