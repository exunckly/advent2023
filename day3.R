# Advent of code 2023 - Day 2

library(tidyverse)

# We are given a matrix containing digits, full stops (blanks) and symbols

# Part 1
# Need to find all numbers containing at least one digit that is adjacent (one step) to non-full stop symbols
# Adjacency is horizontal, vertical or diagonal
# Numbers are when you have horizontally adjacent digits, e.g. 573 is five hundred and seventy-three
# Final answer is the sum of the numbers adjacent to symbols

find_adjacent <- function(my_row, my_col, no_adj = 1){
# Find locations of adjacent cells
# no_adj lets us specify how far adjacent we want to look in case part 2 expands this (note: it didn't)
# Returns a matrix that does not contain the original location
  row_span <- (my_row-no_adj):(my_row+no_adj)
  col_span <- (my_col-no_adj):(my_col+no_adj)
  locs <- expand.grid(row_span, col_span)
  colnames(locs) <- c("row", "column")
  locs2 = locs %>%
    filter(row != my_row | column != my_col)
  return(data.matrix(locs2))
}


#my_file <- "Data/day3_test.txt"
my_file <- "Data/day3_input.txt"

# General idea is to:
# extract the locations adjacent to special characters as row, column
# extract the locations of digits as row, column and have additional columns that:
#   (i) say what number the digit is part of
#   (ii) uniquely identify the number
# find the intersecting locations between these two datasets

# Don't need to worry about some adjacent locations being 'outside the grid' (e.g. column 0)
# because they will not intersect with any of the digits

# Read in data
my_data <- as_tibble(read_lines(my_file)) %>%
  mutate(row = row_number())

# Extract locations of special characters that are not . or digits
my_specials <- my_data %>%
  mutate(value = str_replace_all(value, "\\d", ".")) %>% # replace digits with .
  mutate(value = str_replace_all(value, "\\.", " ")) %>% # replace . with space
  mutate(specials = str_locate_all(value, "\\S")) %>% # find individual characters that are not whitespace
  unnest_longer(specials) %>%
  mutate(column = specials[,1]) %>% # Only need one value as the start and end are always the same
  select (-specials, -value)

# Find all adjacent locations to look for digits in
my_adjacents <- my_specials %>%
  rowwise() %>%
  mutate(adjacents = list(find_adjacent(row, column))) %>%
  unnest_longer(adjacents) %>% # would be nice to work out how to avoid the clumsy renaming
  select (-row, -column) %>%
  mutate(row = adjacents[,1]) %>%
  mutate(column = adjacents[,2]) %>%
  select (-adjacents) %>%
  distinct() # dedupe as we have some locations > once

# Extract 'spans' of numbers so that we can associate each digit location with the number it is part of
# Dataframe has row, start, end and number; where start and end are the columns that the number starts and ends in
my_spans <- my_data %>%
  mutate(locs = str_locate_all(value, "\\d+")) %>%
  unnest_longer(locs) %>%
  mutate(start = locs[,1]) %>%
  mutate(end = locs[,2]) %>%
  select(-locs) %>%
  mutate(number = as.numeric(str_sub(value, start, end)))

# Extract locations of digits
# Each entry is a row and column in which a digit is found, plus the start and end of the number containing the digit
# row + start + end = unique identifier for an individual number
my_digit_locs <- my_spans %>%
  rowwise() %>%
  mutate(column = list(start:end)) %>%
  unnest_longer(column)

# Intersect the adjacent locations with the digit locations
# The distinct line is because we may have identified the same part number in the same location more than once

my_intersect <- left_join(my_adjacents, my_digit_locs, by = c("row", "column")) %>%
  filter(!is.na(number)) %>%
  distinct(row, start, end, .keep_all = TRUE)

# Spent ages wondering what I'd done wrong before realising that it didn't want unique part numbers :-(
# i.e. if 123 appears in two different places on the grid we include it twice in the sum
part1 <- sum(my_intersect$number)

# Part 2 - now we want to use only * and only when the * is adjacent to exactly two part numbers
# We multiply together the two part numbers and sum those to get the second answer

# Extract locations of stars
my_stars <- my_data %>%
  mutate(stars = str_locate_all(value, "\\*")) %>%
  unnest_longer(stars) %>%
  mutate(column = stars[,1]) %>% # Only need one value as the start and end are always the same
  select (-stars, -value) %>%
  mutate(star_id = row_number())

# Find all adjacent locations to look for digits in
my_adjacent_to_stars <- my_stars %>%
  rowwise() %>%
  mutate(adjacents = list(find_adjacent(row, column))) %>%
  unnest_longer(adjacents) %>% # would be nice to rename things less clumsily than this
  select (-row, -column) %>%
  mutate(row = adjacents[,1]) %>%
  mutate(column = adjacents[,2]) %>%
  select (-adjacents) %>%
  distinct() # dedupe as we have some locations > once

# Intersect the locations adjacent to stars with the digit locations
my_star_intersect <- left_join(my_adjacent_to_stars, my_digit_locs, by = c("row", "column")) %>%
  filter(!is.na(number)) %>%
  distinct(row, start, end, .keep_all = TRUE) %>%
  # Include only stars adjacent to exactly two numbers
  summarise(product = prod(number), n = n(), .by = star_id) %>%
  filter(n == 2)

part2 <- sum(my_star_intersect$product)