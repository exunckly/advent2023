# Advent of code 2023 - Day 4

library(tidyverse)

# We have a series of 'scratchcards'
# Data line is "Card" card number : a series of 'winning numbers' | a series of numbers on the scratchcard
# Data format is fixed width (right aligned)

# Part 1

# We want to know, line by line, how many winning numbers the scratchcard contains
# Then manipulate these answers to get a single number (details below)

# Read in data
my_data <- as_tibble(read_lines("Data/day4_input.txt")) %>%
  separate_wider_regex(value, patterns = c(
    "Card\\s+", id = "\\d+", ":\\s+", win = ".*", " \\|\\s+", have = ".*")) %>%
  rowwise() %>%
  # Make them numeric in case we need this in part 2 (note: wasn't needed)
  mutate(win_list = list(as.numeric(unlist(str_split(win, "\\s+"))))) %>%
  mutate(have_list = list(as.numeric(unlist(str_split(have, "\\s+"))))) %>%
  mutate(matches_list = NA, matches = NA)

# Work out how many winning numbers are %in% the scratchcard
# Some time, some year I will practice how to do this without using a loop
for (i in seq_along(my_data$id)){
  my_data$matches_list[i] <- list(my_data$win_list[i][[1]] %in% my_data$have_list[i][[1]])
  my_data$matches[i] = sum(my_data$matches_list[i][[1]])
}

# Answer for part 1 summarises the number of matches in this way
my_data <- my_data %>%
  mutate(score = ifelse(matches == 0, 0, 2^(matches-1)))

part1 <- sum(my_data$score)



# Part 2

# "you win copies of the scratchcards below the winning card equal to the number of matches.
# So, if card 10 were to have 5 matching numbers, you would win one copy each of cards 11, 12, 13, 14, and 15."

#"Copies of scratchcards are scored like normal scratchcards and have the same card number as the card they copied.
# So, if you win a copy of card 10 and it has 5 matching numbers, it would then win a copy of the same cards that
# the original card 10 won: cards 11, 12, 13, 14, and 15. This process repeats until none of the copies cause you
# to win any more cards. (Cards will never make you copy a card past the end of the table.)"

my_data <- my_data %>%
  mutate(no_to_process = 1)

# I think the for loop here is appropriate because it seems most efficient to do the rows in order?
for (i in seq_along(my_data$id)){
  # n matches means that you win a copy of the next n cards (accounting for overspill)
  if(my_data$matches[i] > 0){
    lower_range <- min(c(i+1, length(my_data$no_to_process)))
    upper_range <- min(c(i+my_data$matches[i], length(my_data$no_to_process)))
    my_data$no_to_process[lower_range:upper_range] =  my_data$no_to_process[lower_range:upper_range] + my_data$no_to_process[i]
  }
}

part2 <- sum(my_data$no_to_process)


