# December 03 - Advent of Code 2022 ---------------------------------------

# Initiate
library(tidyr)
library(dplyr)
library(stringr)

# Part 1 ------------------------------------------------------------------

# Read data
strategy_data <- read.delim("data/03_dec_data.txt")

# Split each row into two variables by half
for (i in 1:300) {
  strategy_data$first[i] <- substr(strategy_data$items[i], 1, nchar(strategy_data$items[i])/2)
  strategy_data$second[i] <- substr(strategy_data$items[i], nchar(strategy_data$items[i])/2+1, nchar(strategy_data$items[i]))
}

d1 <- strategy_data[1,]

d2 <- data.frame(str_split_fixed(strategy_data$first[1], "", max(nchar(strategy_data$first[1]))),
                 str_split_fixed(strategy_data$second[1], "", max(nchar(strategy_data$second[1]))))

strategy_data_wide <- cbind(d1, d2)

# Split each compartment items into single character variables
for (i in 2:300) {
  
  d1 <- strategy_data[i,]
  
  d2 <- data.frame(str_split_fixed(strategy_data$first[i], "", max(nchar(strategy_data$first[i]))),
                   str_split_fixed(strategy_data$second[i], "", max(nchar(strategy_data$second[i]))))
  
  d3 <- cbind(d1, d2)
  
  strategy_data_wide <- bind_rows(strategy_data_wide, d3)
  
}

# Give proper comparment names
colnames(strategy_data_wide)[grep("*...1", colnames(strategy_data_wide))] <- "second_compartment"
colnames(strategy_data_wide)[grep("X", colnames(strategy_data_wide))] <- "first_compartment"

# Make longer
strategy_data_long <- strategy_data_wide %>% 
  pivot_longer(c("first_compartment", "second_compartment"), names_to = "compartment", values_to = "item", values_drop_na = TRUE)

# And then wider again
strategy_data_wider <- strategy_data_long %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = compartment, values_from = item) %>% 
  select(-row)

# Find ites in first compartment that also exists in second compartment
shared_items <- strategy_data_wider %>% 
  group_by(items) %>% 
  filter(first_compartment %in% second_compartment) %>% 
  ungroup()

# New dataset with only the shared items
shared_items <- shared_items %>% 
  group_by(items) %>% 
  distinct(first_compartment) %>% 
  select(items, first_compartment) %>% 
  rename(shared_item = first_compartment)

# Remove rows with NA's
shared_items <- na.omit(shared_items)

# Replace charachter with priority value
for (i in 1:26) {
  letter_lower <- letters[i]
  letter_upper <- LETTERS[i]

shared_items$shared_item <- gsub(letter_lower, i, shared_items$shared_item)  
shared_items$shared_item <- gsub(letter_upper, i + 26, shared_items$shared_item)  
}

# As number
shared_items$shared_item <- as.integer(shared_items$shared_item)

# Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?
sum(shared_items$shared_item)