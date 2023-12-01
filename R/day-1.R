
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


