# Advent of code 2023 - Day 9
library(tidyverse)

# Recursive function to gfind next and zeroth in sequence
# In part 1 the next value in the sequence is sum(keep_track_f) from this recursive function
# Iterations is for debugging
#   Post hoc note: purr::accumulate() would have been helpful
my_extrapolate <- function(x, keep_track_f = c(), keep_track_b = c(), iterations = 0){
  # sum(x) != 0 doesn't work when we have a mix of positive and negative numbers
  if(!all(x == 0)){
    #print(x)
    keep_track_f <- append(keep_track_f, x[length(x)]) # faster than tail()
    keep_track_b <- append(keep_track_b, x[1])
    iterations = iterations + 1
    x <- my_extrapolate(diff(x), keep_track_f, keep_track_b, iterations)
  } else {
    return (list(x = cumsum(na.omit(keep_track_f)),
                 keep_track_f = keep_track_f,
                 keep_track_b = keep_track_b,
                 iterations = iterations))
  }
}

# In part 2 the zeroth value in the sequence is slightly clunkier
#   Post hoc note: turns out that I could just have reversed the sequences and used the same method as in part 1
my_zeroth_val <- function(first_vals, target = 0){
  zeroth_val <- rep(NA, length(first_vals))
  for(i in rev(seq_along(first_vals))){
      if (i == length(first_vals)){
        zeroth_val[i] <- first_vals[i] - target
      } else {
        zeroth_val[i] <- first_vals[i] - zeroth_val[i+1]
      }
  }
  return(zeroth_val[1])
}

# Read in data
my_data <- as_tibble(read_lines("Data/day9_input.txt")) %>%
  rowwise() %>%
  mutate(seq_vec = list(as.numeric(unlist(str_split(value, " "))))) %>%
  mutate(seq_len = length(seq_vec)) %>%
  # Keep track of first and last values in the sequences as they reduce
  mutate(extrapolation = list(my_extrapolate(unlist(seq_vec)))) %>%
  # Next value in each sequence
  mutate(next_val = sum(extrapolation$keep_track_f)) %>%
  mutate(iterations = extrapolation$iterations) %>%
  # Zeroth value in each sequence
  mutate(zeroth_val = my_zeroth_val(extrapolation$keep_track_b))

part1 <- sum(my_data$next_val)
print(part1)

part2 <- sum(my_data$zeroth_val)
print(part2)