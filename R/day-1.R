
# Part 1 ------------------------------------------------------------------

# The newly-improved calibration document consists of lines of text; each line 
# originally contained a specific calibration value that the Elves now need to 
# recover. On each line, the calibration value can be found by combining the 
# first digit and the last digit (in that order) to form a single two-digit 
# number.
# 
# For example:
#   
# ```
# 1abc2
# pqr3stu8vwx
# a1b2c3d4e5f
# treb7uchet
# ```
# 
# In this example, the calibration values of these four lines are 12, 38, 15, 
# and 77. Adding these together produces 142.
# 
# Consider your entire calibration document. What is the sum of all of the 
# calibration values?

# We need to manipulate the given strings and extract the numeric values from
# them. For this we'll use the `{stringr}` package for this, part of the 
# {tidyverse}. You could also use the base regular expression functions.
library(stringr)

# read in input data to object. Creates character vector
data <- readLines("inputs/day-1.txt")

# use {stringr} to extract all numeric values from each element of the vector.
# returns a list of character vectors
num_list <- stringr::str_extract_all(data, "\\d")

# get the first and last element of each vector. we'll use base subsetting.
# initiate a vector to store our results with same length of `num_list`
cal_vals_vec <- vector(length = length(num_list))

# we'll need to use a loop to access each element
for (i in seq_along(num_list)){
  
  # get first element
  el_first <- num_list[[i]][[1]]
  
  # get last element
  el_last <- num_list[[i]][length(num_list[[i]])]
  
  # paste them together to make a single double digit number and convert
  cal_val <- as.numeric(paste0(el_first, el_last))
  
  # add to vector
  cal_vals_vec[[i]] <- cal_val
  
}

# now let's sum our vector to get an answer
sum(cal_vals_vec)

# Part 2 ------------------------------------------------------------------

# Your calculation isn't quite right. It looks like some of the digits are 
# actually spelled out with letters: one, two, three, four, five, six, seven, 
# eight, and nine also count as valid "digits".
# 
# Equipped with this new information, you now need to find the real first and 
# last digit on each line. For example:
# 
# ```
# two1nine
# eightwothree
# abcone2threexyz
# xtwone3four
# 4nineeightseven2
# zoneight234
# 7pqrstsixteen
# ```
# 
# In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. 
# Adding these together produces 281.
# 
# What is the sum of all of the calibration values?

# we now need to identify the words of numbers within our text. We can do this
# with pattern matching. We will also have to convert these words to numbers.
# There may be a package out these to do this but easy enough to do ourselves

# first lets create a lookup for our numbers. This can be a named vector
numbers <- c("zero" = 0, "one" = 1, "two" = 2, "three" = 3, "four" = 4, 
             "five" = 5, "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9)

# we need to use {stringr} again to extract our values into a list of vectors.
# let's build our regexp pattern.
pattern <- paste0(c("\\d", names(numbers)), collapse = "|")

# and pass it to the regexp function
num_list_words <- stringr::str_extract_all(data, pattern)

# initiate vector
cal_vals_words_vec <- vector(length = length(num_list_words))

# loop through each vector
for (j in seq_along(num_list_words)) {
  
  # get first element
  el_words_first <- num_list_words[[j]][[1]]
  
  # convert if a word
  if(nchar(el_words_first) > 1) {
    el_words_first <- numbers[[el_words_first]]
  }
  
  # get last element
  el_words_last <- num_list_words[[j]][length(num_list_words[[j]])] 
  
  # convert if a word
  if(nchar(el_words_last) > 1) {
    el_words_last <- numbers[[el_words_last]]
  }
  
  # paste them together to make a single double digit number and convert
  cal_val_words <- as.numeric(paste0(el_words_first, el_words_last))
  
  # add to vector
  cal_vals_words_vec[[j]] <- cal_val_words
}

# now let's sum our vector to get an answer
sum(cal_vals_words_vec)


# Attempt number 2 --------------------------------------------------------

# the above solution doesn't quite work and returns the incorrect final sum.
# This is because there are some numbers that appear in the input file like:
# -'sevenine', 'threeight', 'twone', etc.
# these numbers should be parsed individually with letters reused, such as 79, 
# 38, and 21.
# the current solution doesn't allow this due to a limitation of the regex used,
# which doesn't allow overlapping use of letters. the above example would be
# parsed as '7ine', '3ight', and '2ne'.
# we can get around this by replacing the number words in our strings with 
# something a little easier to handle

r_vec <- c("zero" = "z0o", "one" = "o1e", "two" = "t2o", "three" = "t3e",
           "four" = "f4r", "five" = "f5e", "six" = "s6x", "seven" = "s7n",
           "eight" = "e8t", "nine" = "n9e")

# let's add a step to replace the strings before extracting the numbers
r_data <- stringr::str_replace_all(data, pattern = r_vec)

# this replacement means we can now go back to the first pattern of extracting
# numbers
num_list_r_words <- stringr::str_extract_all(r_data, "\\d")

# initiate vector
cal_vals_r_words_vec <- vector(length = length(num_list_r_words))

# we'll need to use a loop to access each element
for (k in seq_along(num_list_r_words)){
  
  # get first element
  el_r_words_first <- num_list_r_words[[k]][[1]]
  
  # get last element
  el_r_words_last <- num_list_r_words[[k]][length(num_list_r_words[[k]])]
  
  # paste them together to make a single double digit number and convert
  cal_val_r_words <- as.numeric(paste0(el_r_words_first, el_r_words_last))
  
  # add to vector
  cal_vals_r_words_vec[[k]] <- cal_val_r_words
  
}

# add up all the calibration values
sum(cal_vals_r_words_vec)

# df <- data.frame(
#   raw_in  = data,
#   replaced_data = r_data,
#   part_1 = cal_vals_vec,
#   part_2a = cal_vals_words_vec,
#   part_2b = cal_vals_r_words_vec
# )
# 
# View(df)
