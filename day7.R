# Advent of code 2023 - Day 7

library(tidyverse)
library(stringi)

# Aces high, five card hands, A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2. 

# Every hand is exactly one type. From strongest to weakest, they are:
# 
# Five of a kind, where all five cards have the same label: AAAAA
# Four of a kind, where four cards have the same label and one card has a different label: AA8AA
# Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
# Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
# Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
# One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
# High card, where all cards' labels are distinct: 23456

# Break ties by looking at the (unsorted) cards and comparing first card, then second card if a tie and so on
# Doesn't matter what we have 'three of' e.g. AAA is the same as 222 (I wonder if this will change in part 2?!)

# Looked up poker hands in advance of part 2 - the only other thing we may need to account for is a flush, i.e. 5 consecutive cards


# Read in data

my_data <- as_tibble(read_lines("Data/day7_input.txt")) %>%
  separate_wider_delim(value, delim = " ", names = c("hand", "bid")) %>%
  dplyr::mutate(bid = as.numeric(bid))

# Represent hands using a string that says how many of a kind we have
# Post hoc note - hand_rep essentially re-invents rle() from base R, which I didn't know about
hand_ranks <- tibble(hand_type = c("five of a kind",
                                  "four of a kind",
                                  "full house",
                                  "three of a kind",
                                  "two pair",
                                  "one pair",
                                  "high card"),
                   hand_rep = c("5", "41", "32", "311", "221", "2111", "11111"),
                   hand_rank = c(1, 2, 3, 4, 5, 6, 7))

# Know how the individual cards rank for tie breaking
# Ranks of card is its position in the vector - can use match() to get that
# Can also use the vector to construct our data structure
# If we need to do a flush in part 2, can return all five indices of 11111 hands and get diff(range())
card_ranks <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")



represent_hands <- function (my_data, card_ranks){
# Make the hand representations
# I was wondering if part 2 might change the tiebreak rules, e.g. so that QQ beat 55, so preserved more information than needed in the end
my_data <- my_data %>%
  mutate(!!!setNames(rep(NA, length(card_ranks)), card_ranks))

  for (i in seq_along(card_ranks)){
    var_name <- card_ranks[i]
    my_data <- my_data %>%
      rowwise() %>%
      mutate("{var_name}" := ifelse(str_detect(hand, var_name), length(str_extract_all(hand,var_name)[[1]]), 0))
  }
  return(my_data)
}

rank_hands <- function (my_data, hand_ranks, card_ranks){
  # Rank the hands
  my_data <- my_data %>%
    unite("hand_rep", A:`2`, sep = "", remove = FALSE) %>%
    mutate(hand_rep = str_replace_all(hand_rep,"0", "")) %>%
    rowwise() %>%
    mutate(hand_rep = paste(sort(unlist(strsplit(hand_rep, "")), decreasing = TRUE), collapse="")) %>%
    # Make the hand rankings
    # smaller numbers are better ranks
    left_join(hand_ranks, by = "hand_rep") %>%
    # Make the tiebreaks by returning the positions of each card in the card_rank as a hex number
    # then representing this as one hex number by smashing them together
    # Smaller numbers are better ranks
    mutate(tiebreak = as.hexmode(paste(as.hexmode(match(unlist(strsplit(hand, "")), card_ranks)), collapse = ""))) %>%
    # sort the data by hand_rank then by tiebreak
    ungroup() %>%
    arrange(desc(hand_rank), desc(tiebreak)) %>%
    # Calculate ranks and winnings
    mutate (overall_rank = row_number()) %>%
    mutate (winnings = bid * overall_rank)
  return(my_data)
}

my_data1 <- my_data %>%
  represent_hands(card_ranks) %>%
  rank_hands(hand_ranks, card_ranks)

# Total winnings is the bid * the rank
 part1 <- sum(my_data1$winnings)
 print(part1)


# Part 2 - J cards are now jokers not jacks
 

card_ranks2 <- c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J")

# Represent the hands but with this different order
my_data2 <- my_data %>%
  represent_hands(card_ranks2) %>%
  rowwise() %>%
  # This seems to be where 'thinking like it's Excel' ties me in knots a bit
  # Maximum number of cards that are the same
  mutate(max_same = max(c_across(A:`2`))) %>%
  # Leftmost position with max_same cards
  mutate(pos_same = min(which(max_same == (c_across(A:`2`)))))
  # Add the number of jokers to the highest ranked card that has the highest number of copies 
  # The next line didn't work because I couldn't work out how to refer to the value in the column named J
  # mutate(across(A:`2`, ~ifelse(cur_column() == pos_same, .x + J, .x)))

 for (i in seq_along(my_data2$hand)){
   my_data2[i, (my_data2$pos_same[i]+2)] = my_data2[i, (my_data2$pos_same[i]+2)] + my_data2$J[i]
 } 

# Rank the hands
my_data2 <- my_data2 %>%
  rank_hands(hand_ranks, card_ranks2)
  
part2 <- sum(my_data2$winnings)
print(part2)
