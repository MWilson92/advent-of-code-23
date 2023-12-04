
# Part 1 ------------------------------------------------------------------

# As you walk, the Elf shows you a small bag and some cubes which are either 
# red, green, or blue. Each time you play this game, he will hide a secret 
# number of cubes of each color in the bag, and your goal is to figure out
# information about the number of cubes.
# 
# To get information, once a bag has been loaded with cubes, the Elf will reach 
# into the bag, grab a handful of random cubes, show them to you, and then put
# them back in the bag. He'll do this a few times per game.
# 
# You play several games and record the information from each game (your puzzle 
# input). Each game is listed with its ID number (like the 11 in Game 11: ...) 
# followed by a semicolon-separated list of subsets of cubes that were revealed 
# from the bag (like 3 red, 5 green, 4 blue).
# 
# For example, the record of a few games might look like this:
# 
# Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
# 
# In game 1, three sets of cubes are revealed from the bag (and then put back
# again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red 
# cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.
# 
# The Elf would first like to know which games would have been possible if the 
# bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?
# 
# In the example above, games 1, 2, and 5 would have been possible if the bag 
# had been loaded with that configuration. However, game 3 would have been 
# impossible because at one point the Elf showed you 20 red cubes at once;
# similarly, game 4 would also have been impossible because the Elf showed you 
# 15 blue cubes at once. If you add up the IDs of the games that would have been
# possible, you get 8.
# 
# Determine which games would have been possible if the bag had been loaded with
# only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the 
# IDs of those games?

# like day one we'll have to manipulate strings to extract our data with 
# {stringr}
library(stringr)

# read in data
raw_in <- readLines("inputs/day-2.txt")

# let's split our data to to give us the game number and then each turn
raw_ls <- stringr::str_split(raw_in, ":|;")

# initialise a list object to hold our data (internally lists in R are 
# generalised vectors)
data_ls <- vector(mode = "list", length = length(raw_ls))

# lets try and create a flag for each that is impossible.
# loop through each game and pull out values
for (i in seq_along(raw_ls)) {

  # pass the name of the game the data relates to
  names(data_ls)[i] <- raw_ls[[i]][[1]]
  
  # create flag green to see if game possible due to too many greens
  # I feel like this codes is very ugly but does the job
  data_ls[[i]][["green"]] <- sum( # can't pipe to sum with native pipe
    str_extract_all(              # extract values for each game for greens
    raw_ls[[i]][-1],              # remove game number
    "(\\d+)(?=\\s+green)"         # regex to extract numbers with lookahead
  ) |> 
    unlist() |>                   # str_extract_all returns list, need vector
    as.numeric() > 13)            # convert to numeric and assert logical
  
  # repeat above for blue and red
  data_ls[[i]][["blue"]] <- sum(
    str_extract_all(
    raw_ls[[i]][-1],
    "(\\d+)(?=\\s+blue)"
  ) |> 
    unlist() |> 
    as.numeric() > 14)
  
  data_ls[[i]][["red"]] <- sum(
    str_extract_all(
    raw_ls[[i]][-1],
    "(\\d+)(?=\\s+red)"
  ) |> 
    unlist() |> 
    as.numeric() > 12)
  
  # now unlist colour flags to give us a named vector for each game
  # sum flags
  data_ls[[i]] <- sum(unlist(data_ls[[i]]))
  
}

# now we know what games had 1 or more impossible draws (and how many)
# let's get the index and sum them up
sum(which(data_ls == 0))


# Part 2 ------------------------------------------------------------------

# As you continue your walk, the Elf poses a second question: in each game you
# played, what is the fewest number of cubes of each color that could have been
# in the bag to make the game possible?
# 
# Again consider the example games from earlier:
# 
# Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
# 
# In game 1, the game could have been played with as few as 4 red, 2 green, and
# 6 blue cubes. If any color had even one fewer cube, the game would have been 
# impossible.
# Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue 
# cubes.
# Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
# Game 4 required at least 14 red, 3 green, and 15 blue cubes.
# Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.
# 
# The power of a set of cubes is equal to the numbers of red, green, and blue
# cubes multiplied together. The power of the minimum set of cubes in game 1 is
# 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these 
# five powers produces the sum 2286.

# For each game, find the minimum set of cubes that must have been present. 
# What is the sum of the power of these sets?

# let's amend what we did previously to give us the max value for each colour in
# a game

# initialise list
power_ls <- vector(mode = "list", length = length(raw_ls))

# now lets loop through again
for (j in seq_along(raw_ls)) {
  
  # pass the name of the game the data relates to
  names(power_ls)[j] <- raw_ls[[j]][[1]]
  
  # create vector of all green values and find max value
  power_ls[[j]][["green"]] <- str_extract_all(
      raw_ls[[j]][-1],              # remove game number
      "(\\d+)(?=\\s+green)"         # regex to extract numbers with lookahead
    ) |>
    unlist() |>                     # str_extract_all returns list, need vector
    as.numeric() |>                 # coerce to numeric
    max()                           # get maximum value
  
  # repeat above for blue and red
  power_ls[[j]][["blue"]] <- str_extract_all(
      raw_ls[[j]][-1],
      "(\\d+)(?=\\s+blue)"
    ) |> 
    unlist() |> 
    as.numeric() |> 
    max()
  
  power_ls[[j]][["red"]] <- str_extract_all(
      raw_ls[[j]][-1],
      "(\\d+)(?=\\s+red)"
    ) |> 
    unlist() |> 
    as.numeric() |> 
    max()
  
  # create vector and get product of values
  power_ls[[j]] <- prod(unlist(power_ls[[j]]))
  
}

# sum doesn't work on lists, coerce to vector
sum(unlist(power_ls))
