# December 01 - Advent of Code 2022 ---------------------------------------
#Initiate
library(dplyr)

# Read data
elf_data <- read.delim("data/01_dec_data.txt", blank.lines.skip = FALSE) %>% 
  rename(calories = X6110)

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

# How much calories does the elf carrying the most calories have?
elf_data_calories[[2:1]]
