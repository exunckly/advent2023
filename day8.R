# Advent of code 2023 - Day 8

library(tidyverse)

# Read in data
my_data <- as_tibble(read_lines("Data/day8_test1.txt"))

# Directions to follow
dirn <- my_data %>%
  slice_head() %>%
  separate_longer_position(value,1) %>%
  rename(dirn = value) %>%
  as.matrix()

# Locations
locs <- my_data %>%
  slice(3:n()) %>%
  separate_wider_regex(value,
                       patterns = c(start = ".*", " = \\(",
                       L = ".*",  ", ",
                       R = ".*", "\\)"))


# Part 1
curr_id <- match("AAA", locs$start)
i <- 0

while (locs$start[curr_id] != "ZZZ"){
  i <- i + 1
  # Find the next location
  curr_dirn_index <- ifelse(i%%length(dirn) == 0, length(dirn), i%%length(dirn))
  curr_dirn <- dirn[curr_dirn_index]
  curr_dirn <- locs %>%
    filter(start == start[curr_id]) %>%
    select({{curr_dirn}})
  curr_id <- match(curr_dirn, locs$start)
}

part1 <- i
print(part1)

# Part 2
# Perhaps I could find the 'period' of each loop,
# the indices when the location ends in Z and then %% the heck out of it

loop_T <- function (loc, locs){
  if (str_sub (loc, -1) != A){
    return (NA)
  } else {
    
  }
  
}
