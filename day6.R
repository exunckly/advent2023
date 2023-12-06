# Advent of code 2023 - Day 6

library(tidyverse)

# Part 1

# Could compute all values but it feels more straightforward/faster to write an equation and find intercepts
# (especially with part 2 looming)

# t = race time
# s = displacement
# h = hold time
# r = record

# s = h(t-h)
# s = -h^2 + th

# Line intercepts with record, i.e. EQUALS record when: 
# s = -h^2 + th - r = 0

# Roots of this equation are -t + (t^2 - 4*r)^0.5)/(-2) and -t - (t^2 - 4*r)^0.5)/(-2)
# Need to remember to check for rounding etc when calculating the number of integers between them (inclusive)

# Maximum displacement (in case it's needed later) is halfway along the parabola
# i.e. at t/2
# smax = 0.25 t^2
# This may not be an integer value but can find the nearest integer later (remember we may need to deal with 0.5)

# Read in data
my_data <- read_lines("Data/day6_input.txt") %>%
  str_split("\\s+") %>%
  as_tibble(.name_repair = "unique") %>%
  slice (-1) %>%
  mutate(across(everything(), as.numeric)) %>%
  set_names (c("t", "r")) %>%
  mutate (r_test = 0) %>%
  mutate (source = "part1")
  
# Part 2 - oh no, there is one race only and we should ignore the spaces between the numbers
additional_data <- read_lines("Data/day6_input.txt") %>%
  str_replace_all("\\s+", "") %>%
  str_replace_all("Time:", "") %>%
  str_replace_all("Distance:", "") %>%
  as.numeric()

my_data <- my_data %>%
  add_row(t = additional_data[1],
          r = additional_data[2], 
          source = "part2")

# Answer involves number of ways in which we can beat the record, i.e. the number of integers between the intercepts
# NOT including the intercepts if they are integers because these are when we EQUAL the record

my_data <- my_data %>%
  # Intercepts when we EQUAL the record
  mutate (i_1 = (-t + (t^2 - 4*r)^0.5)/(-2)) %>%
  mutate (i_2 = (-t - (t^2 - 4*r)^0.5)/(-2)) %>%
  # Ceiling/floor to nearest integer unless we equalled the record on an integer
  ### Reminder to come back and check for tolerance (i.e. number of decimal places the check goes to) if I have issues
  mutate (i_1_int = ifelse (i_1 == round(i_1, 0), ceiling(i_1) + 1, ceiling(i_1))) %>%
  mutate (i_2_int = ifelse (i_2 == round(i_2, 0), floor(i_2) - 1, floor(i_2))) %>%
  mutate (num_ways = abs(i_2_int - i_1_int) + 1) %>%
  # In case they are needed in part 2 :-)
  mutate (max_hold = t/2) %>%
  mutate (max_hold_int = round(max_hold, 0)) %>%
  mutate (max_dist = 0.25*t^2)

part1 <- my_data %>%
  filter(source == "part1") %>%
  summarise (prod(num_ways))

print(part1)

part2 <- my_data %>%
  filter(source == "part2") %>%
  summarise (sum(num_ways))

print (part2)