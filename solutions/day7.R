library(tidyverse)
library(here)

source(here("get_input.R"))

input <- get_input(7)

ord <- str_extract_all("A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2", "[A-Z|\\d]")[[1]]
ord2 <- str_extract_all("A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J", "[A-Z|\\d]")[[1]]
type_levels <- factor(c("five_of", "four_of", "full_house", "three_of", "two_pair", "one_pair", "high_card"))

get_type <- function(hand) {
  tabt <- table(hand)
  if (length(unique(hand)) == 1) {
    type <- "five_of"
  } else if (length(unique(hand)) == 5) {
    type <- "high_card"
  } else if (sum(tabt == 4) == 1) {
    type <- "four_of"
  } else if ((sum(tabt == 3) == 1) && (sum(tabt == 2) == 1)) {
    type <- "full_house"
  } else if ((sum(tabt == 3) == 1) && (sum(tabt == 1) == 2)) {
    type <- "three_of"
  } else if ((sum(tabt == 2) == 2) && (sum(tabt == 1) == 1)) {
    type <- "two_pair"
  } else if ((sum(tabt == 2) == 1) && (sum(tabt == 1) == 3)) {
    type <- "one_pair"
  }
  return(type)
}

# part 1

inp <- input %>% 
  separate(input, into = c("hand", "bid"), sep = "\\s", convert = TRUE) %>% 
  mutate(hand_splt = str_split(hand, "")) %>% 
  mutate(type = map_chr(.x = hand_splt, .f = get_type)) 

inp %>% 
  mutate(rank_str = map_chr(hand_splt,  ~ paste0(LETTERS[match(., ord)], collapse = ""))) %>% 
  arrange(factor(type, levels = type_levels), rank_str) %>% 
  mutate(final_rank = rev(row_number())) %>% 
  summarise(answer = sum(final_rank * bid))
  
# part 2

joker_upgrade <- function(hand, vals) {
  
  tabt <- table(vals)
  n_J <- str_count(hand, "J")
  non_J <- tabt[which(names(tabt) != "J")]
  
  if (n_J > 0 && (get_type(vals) %in% c("four_of", "five_of"))){
    upgrade <- "five_of"
  } else if (n_J == 4) {
    upgrade <- "five_of"
  } else if (n_J == 3 && sum(non_J == 2) == 1) {
    upgrade <- "five_of"
  } else if (n_J == 3 && sum(non_J == 1) == 2){
    upgrade <- "four_of"
  } else if (n_J == 2) {
    
    if (sum(non_J == 3) == 1){
      upgrade <- "five_of"
    } else if (sum(non_J == 2) == 1) {
      upgrade <- "four_of"
    } else if (sum(non_J == 1) == 3) {
      upgrade <- "three_of"
    }
    
    # if 2 of a kind and 1 of other: four_of
    # if all distinct : three_of
    
  } else if (n_J == 1) {
    
    if (sum(non_J == 1) == 4) {
      upgrade <- "one_pair"
    } else if (sum(non_J == 2) == 2) {
      upgrade <- "full_house"
    } else if (sum(non_J == 2) == 1) {
      upgrade <- "three_of"
    } else if (sum(non_J == 3) == 1) {
      upgrade <- "four_of"
    } else if (sum(non_J == 4) == 1) {
      upgrade <- "five_of"
    }
    
    # if all distinct: two_pair
    # if 2 of a kind and 2 distinct: three_of
    # if 2 of a kind and 2 of a kind: full house
    # if 3 of a kind and 1 distinct: four_of
    # if 4 of a kind: five_of

    
  } else {
    upgrade <- "missing"
  }
  
  return(upgrade)
  
} 


inp %>% 
  mutate(type2 = map2_chr(.x = hand, .y = hand_splt, .f = joker_upgrade)) %>% 
  mutate(rank_str = map_chr(hand_splt,  ~ paste0(LETTERS[match(., ord2)], collapse = ""))) %>% 
  mutate(final_type = ifelse(type2 == "missing", type, type2)) %>% 
  arrange(factor(final_type, levels = type_levels), rank_str) %>% 
  mutate(final_rank = rev(row_number())) %>% 
  #View()
  summarise(answer = sum(final_rank * bid))
