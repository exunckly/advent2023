# Advent of code 2023 - Day 1

library(tidyverse)
library(janitor)

# Note that data is fixed width but this time the values ar left aligned.
# Yesterday's approach of splitting on "\\s+" should be fine here too

# Part 1
# Text suggests expanding and multiple left joins but the values in the input would lead to very large objects

# Instead:
#  find the maximum value in the 'source' column that is lower than the value we are looking up
#  check to see if the value we're looking up is within the range in that row (if it 'matches')
#  set the returned value:
#     if no match, same as source value
#     if it has matched apply the offset between 'dest' and 'source' for the matched row
#  Take care with signs and the order of the values (in the inout dest is first, then source then range)

# Read in data - we have two different data formats in the same textfile, for seeds and lookups

my_data <- as_tibble(read_lines("Data/day5_input.txt"))

# Function to return a destination value when given a lookup table, source value, and source and destination names
# The join stuff is a bit overkill perhaps
return_dest <- function(in_source_val, in_source_name, in_dest_name, t_lookup){
  # See if a match exists
  to_join <- tibble(source_name = in_source_name,
                    dest_name = in_dest_name,
                    in_source_val = in_source_val)
  by <- join_by(source_name, dest_name, between(in_source_val, source, source_upper))
  # There should be one or zero matches
  best_match <- left_join(to_join, t_lookup, by)
  if (is.na(best_match$dest[1])){
    return (in_source_val)
  } else {
    return(in_source_val + best_match$delta[1])
  }
}


# Items in the order that we operate on them
items <- c("seed", "soil", "fertilizer", "water", "light", "temperature", "humidity", "location")

# Make the seeds dataset
seeds <- my_data %>%
  slice(1) %>%
  separate_longer_delim(value, " ") %>%
  slice (-1) %>%
  mutate(value = as.numeric(value)) %>%
  rename(seed = value)

  
# Make the lookup table
#  dest_name and source_name are text that describes the values in that row e.g. dest_name: soil source_name: seed 
#  dest and source are the corresponding values
#  range is the range above the starting dest and source values that the mapping applies to

t_lookup <- my_data %>%
  filter(!value == "") %>%
  slice (-1) %>%
  # Do the x-to-y bit
  mutate (mapping = ifelse(str_detect(value, "map"), value, NA)) %>%
  fill(mapping) %>%
  filter(!str_detect(value, "map")) %>%
  separate_wider_regex(mapping, patterns = c(source_name = ".*", "-to-", dest_name = ".*", " map:")) %>%
  # Do the values
  separate_wider_delim(value, delim = " ", names = c("dest", "source", "range")) %>%
  mutate(across(dest:range, as.numeric)) %>%
  # Precompute new column to use in dplyr overlap joins
  mutate(source_upper = source + range-1) %>%
  mutate(dest_upper = dest + range-1) %>%
  mutate (delta = dest - source)

# Store the outputs from various steps in case we need them for part 2 (spolier: we didn't)

# Would like to have written a for loop that uses the column names and items array but got stuck on the syntax
# Could just have written a normal loop on an array but wanted to preserve intermediate steps in case they are needed for part 2

seeds1 <- seeds
seeds1[items[2:length(items)]] <- NA
seeds1 <- seeds1 %>%  
  rowwise() %>%
  mutate(soil = return_dest(seed, "seed", "soil", t_lookup)) %>%
  mutate(fertilizer = return_dest(soil, "soil", "fertilizer", t_lookup)) %>%
  mutate(water = return_dest(fertilizer, "fertilizer", "water", t_lookup))%>%
  mutate(light = return_dest(water, "water", "light", t_lookup)) %>%
  mutate(temperature = return_dest(light, "light", "temperature", t_lookup)) %>%
  mutate(humidity = return_dest(temperature, "temperature", "humidity", t_lookup)) %>%
  mutate(location = return_dest(humidity, "humidity", "location", t_lookup))

part1 <- min(seeds1$location)
print(part1)


# Part 2 - the initial seeds line is actually pairs of ranges
# And there is too much to process into large data structures
# Work with ranges and split the ranges 

# We map onto very few output deltas (difference between input value and output value) so can we split the ranges at each point delta changes?
# The smallest output value will always be at the bottom of a range

start.time <- Sys.time()

# Function that takes a set of ranges of one variable and finds overlaps between these ranges and the lookup tables between that variable and the next one
# Sometimes the original range overlaps one or more looked up ranges but this is accounted for also
return_splits <- function(in_source_lower, in_source_upper, in_source_name, in_dest_name, t_lookup){
  # Extract all overlapping ranges
  # Note that t_lookup is sorted by source value and group number
  to_join <- tibble(source_name = in_source_name,
                    dest_name = in_dest_name,
                    in_source_lower = in_source_lower,
                    in_source_upper = in_source_upper)
  by_lower <- join_by(source_name, dest_name, between(in_source_lower, source, source_upper))
  by_upper <- join_by(source_name, dest_name, between(in_source_upper, source, source_upper))
  # There should be one or zero matches for each
  lower_match <- left_join(to_join, t_lookup, by_lower)
  upper_match <- left_join(to_join, t_lookup, by_upper)
  
  # There may also be ranges that matched in between so grab these too
  to_manipulate <- lower_match %>%
    bind_rows(upper_match) %>%
    unique()
  
  to_use <- t_lookup %>%
    mutate(in_source_lower = in_source_lower) %>%
    mutate(in_source_upper = in_source_upper) %>%
    filter(source_name == in_source_name &
           dest_name == in_dest_name &
           group_order >= min(to_manipulate$group_order) &
           group_order <= max(to_manipulate$group_order))

  to_use <- to_use %>%
    mutate (use_source_lower = ifelse(in_source_lower > source, in_source_lower, source)) %>% # set the ends of the new split ranges
    mutate (use_source_upper = ifelse(in_source_upper < source_upper, in_source_upper, source_upper)) %>% # set the ends of the new split ranges
    mutate (dest_lower = use_source_lower + delta) %>%
    mutate (dest_upper = use_source_upper + delta)
  
  to_return <- to_use %>%
    select(dest_lower, dest_upper) %>%
    rename(lower = dest_lower, upper = dest_upper)
  
  return(to_return)
}


# Function that takes a complete lookup table and looks up the next set of ranges, e.g. you put in the seeds ranges and get the soil ranges
# Uses bind_rows because you sometimes get multiple output ranges for a single input range
compute_next <- function(data_in, in_source_name, in_dest_name, t_lookup){
  for (i in seq_along(data_in$lower)){
    if (i == 1){
      data_out <- return_splits(data_in$lower[i], data_in$upper[i], in_source_name, in_dest_name, t_lookup)
    } else{
      data_out <- data_out %>%
        bind_rows(data_out, return_splits(data_in$lower[i], data_in$upper[i], in_source_name, in_dest_name, t_lookup)) %>%
        unique()
    }
  }
  return(data_out)
}

# Manipulate input data
 seeds2 <- seeds %>%
   mutate (pair = ifelse(row_number() %% 2 == 1, row_number(), row_number()-1)) %>%
   mutate (type = ifelse(row_number() %% 2 == 1, "lower", "range")) %>%
   pivot_wider(names_from = type, values_from = seed) %>%
   mutate(upper = lower + range - 1) %>%
   select (-range, -pair)

# Manipulate the lookup table by adding rows so I can see what is going on
t_lookup_sorted <- t_lookup %>%
  group_by(source_name, dest_name) %>%
  arrange(source, .by_group = TRUE) %>%
  select(source_name, dest_name, source, source_upper, delta, everything()) %>%
  mutate(lowest_source = min(source)) %>%
  mutate(highest_source = max(source_upper)) %>%
  mutate(group_order = row_number()) %>%
  ungroup()

# Looking at the mappings, the range of each mapping input seemed to go from 0 to 2^32 and that most (but not all) of this range was covered by the provided data
# Fill in any gaps in the lookup table to complete it

# We will add any rows to t_lookup_additional by looping through t_lookup_sorted and looking for gaps
t_lookup_additional <- t_lookup_sorted %>%
  ungroup() %>%
  select (-lowest_source, -highest_source)

for(i in seq_along(t_lookup_sorted$source_name)){
  # First row in each group (e.g. seed to soil) is a special case as the row before it is not from the same group
  # and it might not start at zero currently
  if (t_lookup_sorted$group_order[i] == 1){
    if (t_lookup_sorted$source[i] != 0){
      #print("add 0")
      # Check if mapping starts at zero and fill the gap if not
      t_lookup_additional <- t_lookup_additional %>%
        add_row(source_name = t_lookup_sorted$source_name[i],
              dest_name = t_lookup_sorted$dest_name[i],
              source = 0,
              source_upper = t_lookup_sorted$source[i] - 1,
              delta = 0,
              dest = 0,
              dest_upper = t_lookup_sorted$source[i] - 1,
              group_order = 0) # placeholder for debugging - is properly renumbered later
    }
  } else if ((i < length(t_lookup_sorted$source_name) & t_lookup_sorted$group_order[i+1] == 1) |
             (i == length(t_lookup_sorted$source_name))){
    # Last row in the group
    if (t_lookup_sorted$source_upper[i] != 4294967295){
      #print(" add 2^32")
      # Check if mapping starts at ends at 2^32 and fill the gap if not 
      t_lookup_additional <- t_lookup_additional %>%
        add_row(source_name = t_lookup_sorted$source_name[i],
                dest_name = t_lookup_sorted$dest_name[i],
                source = t_lookup_sorted$source_upper[i] + 1,
                source_upper = 4294967295,
                delta = 0,
                dest = t_lookup_sorted$source_upper[i] + 1,
                dest_upper = 4294967295,
                group_order = t_lookup_sorted$group_order[i] + 1) # placeholder for debugging - is properly renumbered later
    }
    
  } else {# if we are not examining the first or last row in the group
        # Check to see if the ranges are contiguous (most are) and if not add a new range
        if ((t_lookup_sorted$source[i] != t_lookup_sorted$source_upper[i-1] + 1) &
             (t_lookup_sorted$source_name[i] == t_lookup_sorted$source_name[i-1])){
          #print ("add interim")
          t_lookup_additional <- t_lookup_additional %>%
          add_row(source_name = t_lookup_sorted$source_name[i],
                  dest_name = t_lookup_sorted$dest_name[i],
                  source = t_lookup_sorted$source_upper[i-1] + 1,
                  source_upper = t_lookup_sorted$source[i] - 1,
                  delta = 0,
                  dest = t_lookup_sorted$source_upper[i-1] + 1,
                  dest_upper = t_lookup_sorted$source[i] - 1,
                  group_order = t_lookup_sorted$group_order[i] + 0.5) # placeholder for debugging - is properly renumbered later
        }
    }
  
}

# Tidy up the complete lookup table and re-sort then renumber the groups (used in return_splits function)
t_lookup_additional<- t_lookup_additional %>%
  select(-group_order) %>%
  group_by(source_name, dest_name) %>%
  arrange(source, .by_group = TRUE) %>%
  mutate(group_order = row_number()) %>%
  ungroup()

# Walk through the mappings to get locations
soil2 <- compute_next(seeds2, "seed", "soil", t_lookup_additional)
fertilizer2 <- compute_next(soil2, "soil", "fertilizer", t_lookup_additional)
water2 <- compute_next(fertilizer2, "fertilizer", "water", t_lookup_additional)
light2 <- compute_next(water2, "water", "light", t_lookup_additional)
temperature2 <- compute_next(light2, "light", "temperature", t_lookup_additional)
humidity2 <- compute_next(temperature2, "temperature", "humidity", t_lookup_additional)
location2 <- compute_next(humidity2, "humidity", "location", t_lookup_additional)

# As we've kept track of the lower end of all ranges and split them as we went this will have pulled through the lowest value
part2 <- min(location2$lower)
print(part2)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,1)
print(time.taken)
