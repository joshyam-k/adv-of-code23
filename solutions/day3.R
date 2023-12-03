library(tidyverse)
library(here)

source(here("get_input.R"))

input <- get_input(3)

input_ref <- input %>% 
  mutate(row = row_number(),
         items = str_split(input, "")) %>% 
  select(-input) %>% 
  unnest(items) %>% 
  group_by(row) %>% 
  mutate(col = row_number()) %>% 
  ungroup() %>% 
  select(row, col, items) %>% 
  mutate(is_digit = str_detect(items, "\\d")) %>% 
  group_by(row) %>% 
  mutate(number_id = paste0(row, ".", cumsum(is_digit != lag(is_digit, default = FALSE)))) %>% 
  group_by(number_id) %>% 
  mutate(part_number = as.numeric(paste0(items, collapse = ""))) %>% 
  ungroup() 


# part 1

adj <- tibble(row_delta = c(-1, 1, 0, 0, -1, -1, 1, 1),
              col_delta = c(0, 0, -1, 1, -1, 1, -1, 1))

input_ref %>%
  filter(!is.na(part_number)) %>% 
  expand_grid(adj) %>%
  mutate(row2 = row + row_delta,
         col2 = col + col_delta) %>%
  inner_join(input_ref, by = c(row2 = "row", col2 = "col"), suffix = c("", "2")) %>%
  select(-row_delta, -col_delta) %>% 
  filter(items2 != ".", !is_digit2) %>% 
  arrange(row, col) %>% 
  distinct(number_id, .keep_all = TRUE) %>% 
  summarise(sum(part_number))


# part 2

input_ref %>% 
  filter(items == "*") %>% 
  expand_grid(adj) %>%
  mutate(row2 = row + row_delta,
         col2 = col + col_delta) %>%
  inner_join(input_ref, by = c(row2 = "row", col2 = "col"), suffix = c("", "2")) %>%
  select(-row_delta, -col_delta) %>% 
  filter(!is.na(part_number2)) %>% 
  distinct(row, col, number_id2, .keep_all = TRUE) %>% 
  group_by(row, col) %>% 
  summarise(n_adj_num = n(),
            gear_ratio = prod(part_number2),
            .groups = "drop") %>% 
  filter(n_adj_num == 2) %>% 
  summarise(sum(gear_ratio))
