# December 08 - Advent of Code 2022 ---------------------------------------

# Initate
library(dplyr)
library(tidyr)
library(stringr)

# Part 1 ------------------------------------------------------------------

# load data 
trees <- readLines("data/08_dec_data.txt")
trees <- data.frame(trees)

# Split each tree section  into single character variables
d1 <- trees[1,]

d2 <- data.frame(str_split_fixed(trees$trees[1], "", max(nchar(trees$trees[1]))))

trees_data_long <- cbind(d1, d2)

for (i in 2:nrow(trees)) {
  d1 <- trees[i,]
  
  d2 <- data.frame(str_split_fixed(trees$trees[i], "", max(nchar(trees$trees[i]))))
  
  d3 <- cbind(d1, d2)
  
  trees_data_long <- bind_rows(trees_data_long, d3)
} 

rm(d1, d2, d3, i) # End 

# Dublicate all columns
trees_data_long <- trees_data_long %>% 
  mutate(
    across(
      .cols = 2:ncol(trees_data_long),
      .names = "visability_{.col}"
    )
  )

# As numeric
trees_data_long <- trees_data_long %>%
  mutate_if(is.character, as.numeric)

# Dummy if visible
for (i in 2:98) {
  
  for (j in 3:99) {
    
    k <- 99 + j
    
    left <- j - 1
    
    right <- j + 1
    
    up <- i - 1
    
    down <- i + 1
    
    tree <- trees_data_long[i, j]
    
    max_left <- max(trees_data_long[i, 2:left])
    
    max_right <- max(trees_data_long[i, right:100])
    
    max_up <- max(trees_data_long[1:up, j])
    
    max_down <- max(trees_data_long[down:99, j])
    
    trees_data_long[[k]][i] <- ifelse(tree <= max_left & tree <= max_right &
                                        tree <= max_up & tree <= max_down, 
                                      0, 1)
    
  }
}

# Give outer rows and columns value 1 since they all are visible
trees_data_long$visability_X99 <- 1
trees_data_long$visability_X1 <- 1
trees_data_long[1, 101:199] <- 1
trees_data_long[99, 101:199] <- 1

# Sum across columns
visible_trees <- trees_data_long %>% 
  select(starts_with("visability")) %>% 
  mutate(sum = rowSums(.))

# Consider your map; how many trees are visible from outside the grid?
sum(visible_trees$sum)

# Part 2 ------------------------------------------------------------------

