# Advent of code 2023 - Day 1

library(tidyverse)

# Part 1

# Identify the first and last numerical digits in individual lines of text
# If there is one digit only, it is both the first and last

# Finally, sum the values of these two digit numbers to generate the answer

# Read in data
my_data <- read_csv("Data/day1_input.txt", col_names = FALSE)

# Extract digits
my_data <- my_data %>%
  mutate (digits = str_extract_all(X1, "\\d")) %>%
  rowwise() %>%
  mutate(value_part1 = as.numeric(first(digits))*10 + as.numeric(last(digits)))

# Sum values
part1 = sum(my_data$value_part1)


# Part 2
# Sane rules as part 1 except that some of the digits are spelled out with letters, e.g. one two
# We need the first digit and last digit, regardless of whether it is a number or a word
# Tried str_extract_all first but this 'consumes' the string as it goes -
# so it can't find the 'one' in 'twone' which has implications for the last digit

my_regex <- "\\d|one|two|three|four|five|six|seven|eight|nine"
replace_vec <- c("one" = "1", "two" = "2", "three" = "3", "four" = "4",
                "five" = "5", "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9")

my_data <- my_data %>%
  # Learning some new syntax
  # separate_wider_regex lets you match some bits (which you assign to new columns) and throw out other bits
  # It expects the whole string to match the whole regex
  separate_wider_regex(X1, c(".*?", d1 = my_regex, ".*?"), cols_remove = FALSE) %>%
  separate_wider_regex(X1, c(".*", d2 = my_regex, ".*?"), cols_remove = FALSE) %>%
  # Notes about the regular expressions
  # For d1 (digit 1) the .*? matches zero or one occurrences so that d1 can be either at the start of the string or after some gobbledegook
  # For d2 the .* is greedy so d2 matches the last occurrence
  # separate_wider_regex expects the whole input to match the pattern, hence the third term in each call
  mutate(across(d1:d2, ~str_replace_all(., replace_vec))) %>%
  mutate(value_part2 = as.numeric(d1)*10 + as.numeric(d2))

# Sum values
part2 = sum(my_data$value_part2)



# Part 2 - the bonkers version that first sprung to mind and gave me the star
# before looking into how I might find the last match

# Renamed some of the variables so as not to overlap with actual part 2

my_regex_text = "one|two|three|four|five|six|seven|eight|nine"
my_regex_fwd <- paste0(my_regex_text, "|\\d")
my_regex_rev <- paste0("\\d|", paste(rev(strsplit(my_regex_text, "")[[1]]), collapse="")) # bonkers :-)

replace_vec_long = c("one" = "1", "two" = "2", "three" = "3", "four" = "4",
                "five" = "5", "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9",
                "eno" = "1", "owt" = "2", "eerht" = "3", "ruof" = "4",
                "evif" = "5", "xis" = "6", "neves" = "7", "thgie" = "8", "enin" = "9")

my_data2 <- my_data %>%
  mutate (d1_text = str_extract(X1, my_regex_fwd)) %>%
  rowwise() %>%
  mutate (d2_text = str_extract(paste(rev(strsplit(X1, "")[[1]]), collapse=""), my_regex_rev)) %>%
  ungroup () %>%
  mutate (d1 = as.numeric(str_replace_all(d1_text, replace_vec_long))) %>%
  mutate (d2 = as.numeric(str_replace_all(d2_text, replace_vec_long))) %>%
  mutate (value_part2 = as.numeric(d1)*10 + as.numeric(d2))
  
# Sum values
part2_bonkers = sum(my_data2$value_part2)