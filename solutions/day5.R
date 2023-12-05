library(tidyverse)
library(here)
library(janitor)
library(furrr)
library(data.table)

source(here("get_input.R"))

input_raw <- get_input(5)

seeds <- input_raw[1, ] %>% 
  mutate(seeds = str_extract_all(input, "\\d+")) %>% 
  select(seeds) %>% 
  unnest(seeds) %>% 
  mutate(seeds = as.numeric(seeds)) %>% 
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

find_min_loc <- function(seeds) {
  locs <- rep(1, length(seeds))
  for (i in seq_along(seeds)) {
    
    seed_i <- seeds[i]
    curr_item <- seed_i
    
    for (j in seq_along(flow[-length(flow)])) {
      flow_j <- input_clean[[flow[j]]]
      flow_j_dt <- as.data.table(flow_j)
      
      link <- flow_j_dt[data.table::between(curr_item, flow_j_dt[[2]], flow_j_dt[[2]] + flow_j_dt[["range"]] - 1), ]
    
      if (nrow(link) == 0) {
        curr_item <- curr_item
      } else {
        curr_item <- link[[1]] + (curr_item - link[[2]])
      }
      if (j == 7) {
        locs[i] <- curr_item
      }
      
    }
  }
  
  min(locs)
  
}

find_min_loc(seeds)

# part 2

ids <- seq(from = 1, to = 20, by = 2)
new_seeds <- list()
df <- data.frame()
for (i in seq_along(ids)) {
  t <- data.frame(start = seeds[i], end = seeds[i] + seeds[i + 1])
  df <- rbind(df, t)
  seeds_i <- seeds[i]:(seeds[i] + seeds[i + 1 - 1])
  new_seeds[[i]] <- seeds_i
}


# don't do each one
df %>% 
  mutate(id = row_number()) %>% 
  ggplot() +
  geom_linerange(aes(y = factor(id), xmin = start, xmax = end)) 


test <- rep(seeds, 500)
t1 <- Sys.time()
find_min_loc(test)
t2 <- Sys.time()


new_seed_range <- range(c(as.numeric(df[1, ]), as.numeric(df[8, ]), as.numeric(df[9, ])))
new_seeds_vec <- new_seed_range[1]:new_seed_range[2]

# do work
ncores <- parallel::detectCores()
cl <- parallel::makeCluster(ncores)
parallel::clusterExport(cl, "flow")
parallel::clusterExport(cl, "input_clean")
parallel::clusterEvalQ(cl, library(data.table))

n <- length(new_seeds_vec)
sections <- rep(1:ncores, each = ceiling(n/ncores), length.out = n)
seed_lst <- split(data.frame(new_seeds_vec), f = sections)


res <- parallel::parLapply(cl,
                           X = new_seeds_vec[1:1e8],
                           fun = find_min_loc)

res <- new_seeds %>% 
  future_map_dbl(find_min_loc) 

min(res)







  

    
  
  

