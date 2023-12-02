library(tidyverse)
library(here)

input <- tibble(x = readLines(here("inputs", "day2.txt")))

# part 1
# 12 red, 13 green, 14 blue

input %>% 
  mutate(fail_red = str_detect(x, "(1[3-9]|[2-9]\\d|\\d{3,100})\\sred"),
         fail_green = str_detect(x, "(1[4-9]|[2-9]\\d|\\d{3,100})\\sgreen"),
         fail_blue = str_detect(x, "(1[5-9]|[2-9]\\d|\\d{3,100})\\sblue")) %>% 
  mutate(pass = fail_red + fail_green + fail_blue) %>% 
  filter(pass == 0) %>% 
  mutate(id = str_extract(x, "(?<=Game\\s)\\d+")) %>% 
  summarise(sum(as.numeric(id)))

# part 2
# different approach here...

input %>% 
  mutate(id = row_number()) %>% 
  mutate(items = str_extract_all(x, "\\d+\\s[a-z]+")) %>% 
  unnest(items) %>% 
  separate(items, into = c("number", "color"), sep = " ", convert = TRUE) %>% 
  group_by(id, color) %>% 
  summarise(number = max(number)) %>% 
  summarise(power = prod(number)) %>% 
  summarise(res = sum(power))

