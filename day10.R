# Advent of code 2023 - Day 10
library(tidyverse)
#library(igraph)
#library(tidygraph)

# potential gotchas with igraph
# The following objects are masked from ‘package:lubridate’:
#   %--%, union
# 
# The following objects are masked from ‘package:dplyr’:
#   as_data_frame, groups, union
# 
# The following objects are masked from ‘package:purrr’:
#   compose, simplify
# 
# The following object is masked from ‘package:tidyr’:
#   crossing
# 
# The following object is masked from ‘package:tibble’:
#   as_data_frame
# 
# The following objects are masked from ‘package:stats’:
#   decompose, spectrum
# 
# The following object is masked from ‘package:base’:
#   union



# From the AoC spec: 
#   The pipes are arranged in a two-dimensional grid of tiles:
#   
# | is a vertical pipe connecting north and south.
# - is a horizontal pipe connecting east and west.
# L is a 90-degree bend connecting north and east.
# J is a 90-degree bend connecting north and west.
# 7 is a 90-degree bend connecting south and west.
# F is a 90-degree bend connecting south and east.
# . is ground; there is no pipe in this tile.
# S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

# Note: each type of pipe connects to two other locations (but this may change in part 2?)

# Establish co-ordinate system
# Let's have [1,1] at the top left as R matrices do



# Chuck it into igraph and work out how to find a loop
# igraph seems to call them cycles

# Test the options for S - presumably only one of them will generate a loop?


# test1 = simple loop with no S in it (the only one with no S)
# test2 = simple loop only
# test3 = simple loop with extra garbage around it
# test4 = complex loop only
# test5 = complex loop with extra garbage around it


# Answer for part 1 is the furthest point away from the start that's in the loop,
# in terms of number of steps around the loop (rather than x,y co-ordinates)


# Make list of edges based on symbols using a custom function
# tile ids are of the form rxcy
get_connections <- function (r, c, val){
  case_when(
    val == "|" ~ c(paste0("r", r-1, "c", c), paste0("r", r+1, "c", c)),
    val == "-" ~ c(paste0("r", r, "c", c-1), paste0("r", r, "c", c+1)),
    val == "L" ~ c(paste0("r", r-1, "c", c), paste0("r", r, "c", c+1)),
    val == "J" ~ c(paste0("r", r-1, "c", c), paste0("r", r, "c", c-1)),
    val == "7" ~ c(paste0("r", r+1, "c", c), paste0("r", r, "c", c-1)),
    val == "F" ~ c(paste0("r", r+1, "c", c), paste0("r", r, "c", c+1)),
    val == "." ~ NA,
    val == "S" ~ NA,
    .default = NA
  )
}

# Function to find unique edges
# This removes duplicates
# It also means that we don't need to do anything special with the S location
# as it already has pipes going to it
unique_edges <- function (in_edges){
  out_edges <- in_edges %>%
    mutate(forwards = paste0(id, to)) %>%
    mutate(backwards = paste0(to, id))
  
  i <- 0
  while (i < length(out_edges$id)){
    i = i + 1
    if (i %% 1000 == 0) {print (i)}
    # Remove the matching row before we reach it in the loop
    # Account for the matching id being a moving target as we remove rows, i.e. filter on content not row number
    out_edges <- out_edges %>%
      filter(backwards != out_edges$forwards[i])
  }
  out_edges <- out_edges %>%
    select(id, to)
  return(out_edges)
}


# Read in data
my_data <- as_tibble(read_lines("Data/day10_input.txt")) %>%
  # Grid the data
  mutate (r = row_number()) %>%
  mutate (val = str_split(value, "")) %>%
  unnest (val) %>%
  group_by(r) %>%
  mutate (c = row_number()) %>%
  ungroup() %>%
  # Generate IDs for use in igraph
  mutate (id = paste0("r", r, "c", c)) %>%
  select(id, r, c, val, -value) %>%
  rowwise() #%>%

# If we have a situation like -| they don't actually connect
# need to stick with finding the loop and ignore the other pipes

start_loc <- my_data$id[match("S", my_data$val)]

# Get putative connections and neighbours to S
my_connections <- my_data %>%
  mutate(to = list(get_connections(r, c, val))) %>%
  unnest(to) %>%
  ungroup()

# Need to 'look backwards' to find the first neighbour
# Choose one at random
curr_loc <- my_connections$id[which(my_connections$to %in% start_loc)][1]
loc_vec <- c(start_loc, curr_loc)

while(!is_empty(curr_loc)){
  # Now we will have two matches per position and we need to use the one that we don't already have in loc_vec
  curr_loc_neighbours <- my_connections$to[which(my_connections$id %in% curr_loc)]
  curr_loc <- setdiff(curr_loc_neighbours,loc_vec)
  loc_vec <- append(loc_vec, curr_loc)
}

part1 <- length(loc_vec)/2
print(part1)

# Part 2 - what is in the loop?

# Scan across left to right to see if locations are inside or outside the loop
# Start with state = FALSE (i.e. outside pipes)
# Keep track of whether we have encountered  L or F, and whether we have 'gone outside' the L or F by encountering 7 or J
# If we cross |, L7 (with any amount of stuff between the L and 7) or FJ (ditto) we flip the state to 1
# # If we cross . add state * 1 to the area count

# Function to work out what to replace the S with
get_start_symbol <- function(start_loc, s_neigh){
  s_loc_vals <- as.numeric(unlist(str_extract_all(start_loc, "\\d+")))
  s_neigh_vals <- as.numeric(unlist(str_extract_all(s_neigh, "\\d+"))) # row1, column1, row2 column2
  if (s_neigh_vals[1] == s_neigh_vals[3]) { # same row
    return ("-")
  } else if (s_neigh_vals[2] == s_neigh_vals[4]) { # same column
    return ("|")
  } else if ((s_neigh_vals[2] + s_neigh_vals[4])/s_loc_vals[2] < 1){ #  J or 7 - column to the left
    if ((s_neigh_vals[1] + s_neigh_vals[3])/s_loc_vals[1] < 1) {return("J")} # row above
    if ((s_neigh_vals[1] + s_neigh_vals[3])/s_loc_vals[1] > 1) {return("7")} # row below
  } else if ((s_neigh_vals[2] + s_neigh_vals[4])/s_loc_vals[2] > 1){ #  F or L - column to the right
    if ((s_neigh_vals[1] + s_neigh_vals[3])/s_loc_vals[1] < 1) {return("L")} # row above
    if ((s_neigh_vals[1] + s_neigh_vals[3])/s_loc_vals[1] > 1) {return("F")} #row below
  } else {return("oops")}
}

# Remember to fix the S!
s_neigh <- my_connections$id[which(my_connections$to %in% start_loc)]

# Replace the S
my_data <- my_data %>%
  mutate (val = ifelse(val == "S", get_start_symbol(start_loc, s_neigh), val))

# Make a version of the grid that contains only the loop - wipe away all messy bits
my_loop <- as_tibble(loc_vec) %>%
  rename (id = value) %>%
  mutate (in_loop = TRUE)

my_grid <- my_data %>%
  left_join(my_loop, by = "id") %>%
  mutate(in_loop = ifelse(is.na(in_loop), FALSE, TRUE)) %>%
  mutate(x = ifelse(in_loop == TRUE, val, ".")) %>%
  mutate (a = NA)

my_matrix <- my_grid %>%
  select(r, c, x) %>%
  pivot_wider(names_from = c, values_from = x) %>%
  select (-r) %>%
  as.matrix()

area <- 0
myF <- 0
myL <- 0

my_plan <- matrix(NA, nrow = max(my_grid$r), ncol = max(my_grid$c))
my_plan2 <- matrix(NA, nrow = max(my_grid$r), ncol = max(my_grid$c))


for(i in 1:max(my_grid$r)){
  state <- FALSE
  for (j in 1:max(my_grid$c)){
    x <- my_matrix[i,j]
    my_plan2[i,j] <- x
    if (x == "."){
      area <- area + as.numeric(state)
      my_plan[i,j] <- state
    } else if (x == "|"){
        state <- !state
        pipe <- "on"

    } else if (x == "F"){
        myF <- 1
    } else if (x == "L"){
        myL <- 1
    } else if (x == "J"){
        if (myF == 1){
          state <- !state
          myF <- 0
        }
        if (myL == 1){
          myL <- 0
        }
    } else if (x == "7"){
        if (myL == 1){
          state <- !state
          myL <- 0
        }
       if (myF == 1){
         myF <- 0
       }
    } else {
      # do nothing
    }
  }
}

part2 <- area
print(part2)
