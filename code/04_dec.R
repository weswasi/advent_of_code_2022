# December 04 - Advent of Code 2022 ---------------------------------------

# Initate
library(dplyr)
library(tidyr)

# Part 1 ------------------------------------------------------------------

# Read data
sections_data <- read.delim("data/04_dec_data.txt")

# Separate elf one and elf two's sections
sections_data <- sections_data %>% 
  separate(sections, into=c("elf_one", "elf_two"), sep=',')

# Separate so that one variable is the first section and the second variable is the last section, for first elf
sections_data <- sections_data %>% 
  separate(elf_one, into=c("elf_one_from", "elf_one_to"), sep='-')

# Separate so that one variable is the first section and the second variable is the last section, for second elf
sections_data <- sections_data %>% 
  separate(elf_two, into=c("elf_two_from", "elf_two_to"), sep='-')

# Numneric
sections_data <- sections_data %>% 
  mutate_at(c("elf_one_from", "elf_one_to", "elf_two_from", "elf_two_to"), as.numeric)

# Dummy if sections are overlaping
sections_data <- sections_data %>% 
  mutate(overlap = if_else(elf_one_from >= elf_two_from & elf_one_to <= elf_two_to |
                             elf_one_from <= elf_two_from & elf_one_to >= elf_two_to |
                             elf_one_from > elf_two_from & elf_one_to == elf_two_to & elf_one_from == elf_one_to |
                             elf_one_from == elf_two_from & elf_one_to < elf_two_to & elf_one_from == elf_one_to |
                             elf_one_from == elf_two_from & elf_one_to < elf_two_to & elf_two_from == elf_two_to |
                             elf_one_from < elf_two_from & elf_one_to == elf_two_to & elf_two_from == elf_two_to ,
                           1, 0))

# In how many assignment pairs does one range fully contain the other?
sum(sections_data$overlap)

# Part 2 ------------------------------------------------------------------

# Dummy if sections are overlaping
for (i in 1:1000) {
  elf_one <- seq(sections_data$elf_one_from[i], sections_data$elf_one_to[i])
  elf_two <- seq(sections_data$elf_two_from[i], sections_data$elf_two_to[i])
  
  intersection <- if_else(intersect(elf_one, elf_two) != 0, 1, 0)
  
  sections_data$overlap[i]  <- intersection[1]
}

# In how many assignment pairs do the ranges overlap?
sum(sections_data$overlap, na.rm = TRUE)
