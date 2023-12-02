# Advent of code 2023 - Day 2

library(tidyverse)

# In the input each row is a 'game'
# Each 'turn' involves extracting cubes from a bag. Turns are semicolon separated. The number of turns is not always the same
# Within each turn we have comma separate cube colours separated from the number of cubes by a space, e.g. 4 red
# Game number is colon separated from the rest of the data

#Sample input:
  #Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  #Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue

# Part 1
# "The Elf would first like to know which games would have been possible
# if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?"
# The final answer should be the sum of the IDs of the possible games

# Read in and wrangle data
# Let's guess that we will encounter more colours in a later version of the game
# so go for a long data format with a colour column and IDs for each game and trial within the game

my_data <- as_tibble(read_lines("Data/day2_input.txt")) %>%
  separate_wider_regex(value, c("Game ", game = "\\d+", ": ", trials = ".*"), cols_remove = TRUE) %>%
  mutate(game = as.numeric(game)) %>%
  # separate_rows has been superseded by separate_longer_delim
  separate_longer_delim(trials, delim = "; ") %>%
  # Generate a trial ID within each game - note: it turned out that this was not needed in the end
  group_by(game) %>%
  mutate(trial_id = row_number()) %>%
  ungroup() %>%
  separate_longer_delim(trials, delim = ", ") %>%
  #separate superseded by separate_wider_delim
  separate_wider_delim (trials, names = c("n", "colour"), delim = " ") %>%
  mutate (n = as.numeric(n))

# With my data structure it seems easiest to identify the impossible games and subtract from all games

sum_all_game_ids <- sum(unique(my_data$game))

impossible_games <- my_data %>%
  filter(colour == "red" & n > 12 |
           colour == "green" & n > 13 |
           colour == "blue" & n > 14)

sum_possible_game_ids <- sum_all_game_ids - sum(unique(impossible_games$game))

part1 <- sum_possible_game_ids

# Part 2
# "In each game you played, what is the [smallest] number of cubes of each color
# that could have been in the bag to make the game possible?"
# Note: we are now assuming that all games are possible

# The answer should be max_red * max_blue * max_green in each game, summed

my_data2 <- my_data %>%
  group_by(game, colour) %>%
  summarise(max = max(n)) %>%
  ungroup() %>%
  # Allow for having zero cubes of a colour drawn in a game (which would be a nonexistent row in my long dataset)
  # so pivot wider and replace NA with zero
  pivot_wider(names_from = colour, values_from = max) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(part2 = blue*red*green)

part2 = sum(my_data2$part2)