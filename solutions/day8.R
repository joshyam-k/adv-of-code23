library(tidyverse)
library(here)

source(here("get_input.R"))

input <- get_input(8)

steps <- head(input, 1) %>% 
  mutate(sps = str_extract_all(input, "L|R")) %>% 
  pull(sps) %>% 
  unlist()

nodes <- tail(input, -2) %>% 
  separate(input, into = c("node", "vals"), sep = " = ") %>% 
  mutate(vals = str_remove_all(vals, "\\(|\\)")) %>% 
  separate(vals, into = c("L", "R"), sep = ", ")

num_steps <- function(strt, pt) {
  curr <- strt
  done <- FALSE
  n_steps <- 0
  while(!done) {
    for (i in 1:length(steps)) {
      dir <- steps[i]
      next_step <- curr[[dir]]
      curr <- nodes[nodes$node == next_step, ]
      n_steps <- n_steps + 1
      if (pt == 1) {
        if (curr$node == "ZZZ"){
          done <- TRUE
          break
        }
      } else if (pt == 2) {
        if (str_ends(curr$node, "Z")) {
          done <- TRUE
          break
        }
      }
      
    }
  }
  return(n_steps)
}

# part 1

curr <- nodes[nodes$node == "AAA", ]

num_steps(curr, 1)



# part 2

# instead of brute forcing it, notice that each starting node gets to a node that ends with z 
# cyclically, so find how many steps it takes each individual starting node to get to a node ending with z
# and then find the LCM of those numbers of steps

curr <- nodes %>% filter(str_ends(node, "A"))

out <- c()
for(i in 1:nrow(curr)) {
  indiv <- curr[i, , drop = FALSE]
  num <- num_steps(indiv, 2)
  out <- c(out, num)
}


Lcm_vector <- function(x) Reduce(pracma::Lcm, x)
answer <- Lcm_vector(out)
format(answer, scientific = FALSE)






