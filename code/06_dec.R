# December 6 - Advent of Code 2022 ----------------------------------------

# Initiate
library(dplyr)
library(tidyr)
library(stringr)

# Part 1 ------------------------------------------------------------------

# Read data stream data
stream <- read.delim("data/06_dec_data.txt", header = FALSE)

# Separate string into individual columns
stream <- data.frame(str_split_fixed(stream, "", max(nchar(stream$V1))))

# Find first string with four unique values 
for(i in 1:ncol(stream)) {
  y <- i + 3
  
  distinct <- stream[i:y] %>% 
    pivot_longer(cols = starts_with("X")) %>%
    select(value) %>% 
    n_distinct()
  
  if (distinct == 4) {
    break
  }
}

# How many characters need to be processed before the first start-of-packet marker is detected?
print(y)

# Part 2 ------------------------------------------------------------------

# Find first string with fourteen unique values 
for(i in 1:ncol(stream)) {
  y <- i + 13
  
  distinct <- stream[i:y] %>% 
    pivot_longer(cols = starts_with("X")) %>%
    select(value) %>% 
    n_distinct()
  
  if (distinct == 14) {
    break
  }
}

# How many characters need to be processed before the first start-of-packet marker is detected?
print(y)
