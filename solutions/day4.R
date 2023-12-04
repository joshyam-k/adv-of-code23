library(tidyverse)
library(here)

source(here("get_input.R"))

input <- get_input(4)

# part 1

input_cl <- input %>% 
  mutate(input = str_remove(input, "Card\\s+\\d+:\\s")) %>% 
  separate(input, into = c("winning", "ours"), sep = "\\|") %>% 
  mutate(winning = str_extract_all(winning, "\\d+"),
         ours = str_extract_all(ours, "\\d+")) 

overlaps <- input_cl %>% 
  mutate(n_overlap = map2_dbl(.x = ours, .y = winning, .f =  ~ sum(.x %in% .y))) 

overlaps %>% 
  filter(n_overlap > 0) %>% 
  summarise(points = sum(2^(n_overlap - 1)))

# part 2


n_games <- nrow(overlaps)
m <- overlaps$n_overlap
copies <- rep(1, n_games)


for (i in 1:n_games) {
  if (m[i] > 0) {
    range <- seq(from = i + 1, to = i + m[i])
    # can't overlap over the length of the input
    range[range <= n_games]
    copies[range] <- copies[range] + copies[i]
  }
}
