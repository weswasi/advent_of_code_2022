# December 02 - Advent of Code 2022 ---------------------------------------

# Initiate
library(tidyr)

# Part 1 ------------------------------------------------------------------

# Read data
strategy_data <- read.delim("data/02_dec_data.txt")

# Split strategy variable to predicted value and response value
strategy_data <- strategy_data %>% 
  separate(strategy, c("predicted", "response"), remove = FALSE)

# Translate shape to actual rock/paper/scissors
strategy_data <- strategy_data %>% 
  mutate(predicted = case_when(predicted == "A" ~ "rock",
                               predicted == "B" ~ "paper", 
                               predicted == "C" ~ "scissors"))

strategy_data <- strategy_data %>% 
  mutate(response = case_when(response == "X" ~ "rock",
                              response == "Y" ~ "paper", 
                              response == "Z" ~ "scissors"))

# Did i win, lose or draw?
strategy_data <- strategy_data %>% 
  mutate(outcome = case_when(response == "rock" & predicted == "scissors" ~ "won",
                             response == "paper" & predicted == "rock" ~ "won",
                             response == "scissors" & predicted == "paper" ~ "won",
                             response == "scissors" & predicted == "rock" ~ "lost",
                             response == "rock" & predicted == "paper" ~ "lost",
                             response == "paper" & predicted == "scissors" ~ "lost",
                             TRUE ~ "draw"))

# Translate outcome to points
strategy_data <- strategy_data %>% 
  mutate(outcome_point = case_when(outcome == "won" ~ 6,
                                   outcome == "draw" ~ 3,
                                   TRUE ~ 0))

# Translate shape to points
strategy_data <- strategy_data %>% 
  mutate(response_point = case_when(response == "rock" ~ 1,
                                    response == "paper" ~ 2,
                                    response == "scissors" ~ 3))

# Total round points
strategy_data <- strategy_data %>% 
  mutate(total_round_points = response_point + outcome_point)

# What would your total score be if everything goes exactly according to your strategy guide?
sum(strategy_data$total_round_points)

# Part 2 ------------------------------------------------------------------

# Read data
strategy_data <- read.delim("data/02_dec_data.txt")

# Split strategy variable to predicted value and response value
strategy_data <- strategy_data %>% 
  separate(strategy, c("predicted", "recommended_outcome"), remove = FALSE)

# Translate shape to actual rock/paper/scissors
strategy_data <- strategy_data %>% 
  mutate(predicted = case_when(predicted == "A" ~ "rock",
                               predicted == "B" ~ "paper", 
                               predicted == "C" ~ "scissors"))

# Translate strategy to actual recommended outcome
strategy_data <- strategy_data %>% 
  mutate(recommended_outcome = case_when(recommended_outcome == "X" ~ "loose",
                                         recommended_outcome == "Y" ~ "draw", 
                                         recommended_outcome == "Z" ~ "win"))

# Create response based on predicted shape and recommended outome
strategy_data <- strategy_data %>% 
  mutate(response = case_when(recommended_outcome == "win" & predicted == "rock" ~ "paper",
                              recommended_outcome == "win" & predicted == "paper" ~ "scissors",
                              recommended_outcome == "win" & predicted == "scissors" ~ "rock",
                              recommended_outcome == "loose" & predicted == "rock" ~ "scissors",
                              recommended_outcome == "loose" & predicted == "paper" ~ "rock",
                              recommended_outcome == "loose" & predicted == "scissors" ~ "paper",
                              TRUE ~ predicted))

# Translate outcome to points
strategy_data <- strategy_data %>% 
  mutate(outcome_point = case_when(recommended_outcome == "win" ~ 6,
                                   recommended_outcome == "draw" ~ 3,
                                   TRUE ~ 0))

# Translate shape to points
strategy_data <- strategy_data %>% 
  mutate(response_point = case_when(response == "rock" ~ 1,
                                    response == "paper" ~ 2,
                                    response == "scissors" ~ 3))

# Total round points
strategy_data <- strategy_data %>% 
  mutate(total_round_points = response_point + outcome_point)

# Following the Elf's instructions for the second column, 
# what would your total score be if everything goes exactly according to your strategy guide?
sum(strategy_data$total_round_points)
