# December 01 - Advent of Code 2022 ---------------------------------------

# Initiate
library(dplyr)

# Part 1 ------------------------------------------------------------------

# Read data
elf_data <- read.delim("data/01_dec_data.txt", blank.lines.skip = FALSE)

# Elf id function
elf_data <- elf_data %>% 
  mutate(elf_id = 0)

count <- 1

for (i in 1:nrow(elf_data)) {
  elf_data$elf_id[[i]] <- if_else(is.na(elf_data$calories[[i]]), count + 1, count)
  count <- if_else(is.na(elf_data$calories[[i]]), count + 1, count)
}

rm(count, i)

# Sum calories per elf
elf_data_calories <- elf_data %>% 
  group_by(elf_id) %>% 
  summarise(total_calories = sum(calories, na.rm = TRUE)) %>% 
  arrange(desc(total_calories))

# Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
elf_data_calories[[2:1]]


# Part 2 ------------------------------------------------------------------

# Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?
elf_data_calories %>% 
head(3) %>% 
  summarise(total_calories = sum(total_calories))
