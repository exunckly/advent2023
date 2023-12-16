# Advent of code day 11

# Read in a grid
# Insert a row or column any time we see ..... in an entire row or column (or do some maths?)

# Calculate shortest differences between pairs of locations

# my_data <- as_tibble(read_lines("Data/day11_test.txt")) %>%
#   # Grid the data
#   mutate (r = row_number()) %>%
#   mutate (val = str_split(value, "")) %>%
#   unnest (val) %>%
#   group_by(r) %>%
#   mutate (c = row_number()) %>%
#   ungroup()

library(tidyverse)


# Function to turn an actual matrix (i.e. an 'overhad view' of an area) into a dataframe containing the row and column numbers of each #
# -- giving each # a unique ID
# -- adding the 'gaps' to the row and column numbers
grid_data <- function(x, insert_no){
  grid_data <- x %>%
    as_tibble() %>%
    mutate (r = row_number()) %>%
    pivot_longer(!r, names_to = "c", values_to = "x") %>%
    mutate (c = as.numeric(str_extract(c, "\\d+"))) %>%
    # Allocate IDs
    group_by(x) %>%
    mutate (id = row_number()) %>%
    filter (x == "#") %>%
    ungroup () %>%
    select (-x) %>%
    # Add the gaps to the row and column numbers
    rowwise() %>%
    mutate(r = r + sum(r > empty_rows)*insert_no) %>%
    mutate(c = c + sum(c > empty_cols)*insert_no) #%>%
}

# Function to calculate shortest paths from a dataframe of one row per item: id, r (row number), c (column number)
shortest_paths <- function (grid_data){
  expanded_data <- expand_grid(from = grid_data$id, to = grid_data$id) %>%
    left_join(grid_data, by = join_by(from == id)) %>%
    rename (r1 = r, c1 = c) %>%
    left_join(grid_data, by = join_by (to == id)) %>%
    rename (r2 = r, c2 = c) %>%
    mutate (dist = abs(c2-c1) + abs(r2-r1)) %>%
    # Remove duplicates
    filter (from < to)
  return(expanded_data)
}


# Read in data
my_list <- read_lines("Data/day11_input.txt")
my_split_list2 <- str_split(my_list, "")
my_data <- t(matrix(unlist(my_split_list2), ncol = length(my_split_list2)))

# What do empty rows and columns look like
match_row <- strrep(".", str_length(my_list[[1]]))
match_col <- rep(".", length(my_data[,1]))

# ID numbers of empty rows
empty_rows <- which(my_list == match_row)

# ID numbers of empty columns
empty_cols <- c()
for (i in seq_along(my_data[1,])){
  if (all(my_data[,i] == match_col)){
    empty_cols <- append( empty_cols, i)
  }
}

# Part 1
# Insert 1 row/column when we find a gap
insert_no <- 1

# Shortest paths
my_grid_data1 <- grid_data (my_data, insert_no)
expanded_data1 <- shortest_paths(my_grid_data1)

part1 <- sum(expanded_data1$dist)

print(part1)

# Part 2
# Insert 999999 rows/columns when we find a gap
insert_no <- 999999

# Shortest paths
my_grid_data2 <- grid_data (my_data, insert_no)
expanded_data2 <- shortest_paths(my_grid_data2)

part2 <- sum(expanded_data2$dist)

print(part2)