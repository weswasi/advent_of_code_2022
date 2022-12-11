# December 05 - Advent of Code 2022 ---------------------------------------

# Initate
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)

# Part 1 ------------------------------------------------------------------

# Read crate move data
moves <- read.delim("data/05_dec_data.txt", skip = 9, header=FALSE)

# Read crates data
crates <- read.delim("data/05_dec_data.txt", header=FALSE)

# Separate column and remove unecessary columns
moves <- moves %>% 
  separate(V1, into = c("del_1", "move", "del_2", "from", "del_3", "to")) %>%
  select(-starts_with("del"))

# Only crates
crates <- crates %>% 
  slice(1:8)

# Split column into appropriate nunber of columns
crates  <- read.fwf(
  textConnection(crates$V1),
  widths = rep(4, ceiling(max(nchar(crates$V1) / 4))),
  stringsAsFactors = FALSE)

# Only alphanumeric characters
for (i in 1:ncol(crates)) {
  crates[[i]] <- str_replace_all(crates[[i]], "[^[:alnum:]]", "")
}
rm(i)

# Empty string to NA's
crates <- crates %>% 
  na_if('')

# From columns to dataframes
df_list <- lapply(1:9, function(i) data.frame(crates[i]))
df_list <- lapply(df_list, function(i) i[!is.na(i)])
df_list <- lapply(df_list, function(i) data.frame(i))
names(df_list) <- paste0("crate_", seq(1:9))
list2env(df_list,envir=.GlobalEnv)
rm(df_list)

# Rearange crates
for (i in 1:nrow(moves)){
  from <- as.numeric(moves$from[i])
  to <- as.numeric(moves$to[i])
  move <- as.numeric(moves$move[i])
  
  while (move > 0) {
    temp_df <- get(paste0("crate_", from)) %>% 
      head(1)
    
    assign(paste0("crate_", to), bind_rows(temp_df, get(paste0("crate_", to))))
    
    assign(paste0("crate_", from), get(paste0("crate_", from)) %>% 
             slice(2:n()))
    
    move <- move - 1
  }
}

# Only keep top crate
for (i in 1:9){
  assign(paste0("crate_", i), get(paste0("crate_", i)) %>% 
           slice(1))
}

# Bind columns
for (i in 2:9){
  crate_1 <- crate_1 %>% 
    bind_cols(get(paste0("crate_", i)))
}

# After the rearrangement procedure completes, what crate ends up on top of each stack?
crate_1 %>% 
  unite(crates, 1:ncol(crate_1), sep="")


# Part 2 ------------------------------------------------------------------

# From columns to dataframes
df_list <- lapply(1:9, function(i) data.frame(crates[i]))
df_list <- lapply(df_list, function(i) i[!is.na(i)])
df_list <- lapply(df_list, function(i) data.frame(i))
names(df_list) <- paste0("crate_", seq(1:9))
list2env(df_list,envir=.GlobalEnv)
rm(df_list)

# Rearange crates
for (i in 1:nrow(moves)){
  from <- as.numeric(moves$from[i])
  to <- as.numeric(moves$to[i])
  move <- as.numeric(moves$move[i])
  keep_from <- move + 1
  
  temp_df <- get(paste0("crate_", from)) %>% 
    head(move)
  
  assign(paste0("crate_", to), bind_rows(temp_df, get(paste0("crate_", to))))
  
  assign(paste0("crate_", from), get(paste0("crate_", from)) %>% 
           slice(keep_from:n()))
}

# Only keep top crate
for (i in 1:9){
  assign(paste0("crate_", i), get(paste0("crate_", i)) %>% 
           slice(1))
}

# Bind columns
for (i in 2:9){
  crate_1 <- crate_1 %>% 
    bind_cols(get(paste0("crate_", i)))
}

# After the rearrangement procedure completes, what crate ends up on top of each stack?
crate_1 %>% 
  unite(crates, 1:ncol(crate_1), sep="")
