library(anytime)
library(dbplyr)
library(tidyverse)

final_df = readRDS("/Users/kd/Downloads/STA 141B/final_df.rds")
print(head(final_df, 11))

date = as.Date(final_df$timestamp)
print(unique(date)) # 3 unique dates

print(summary(final_df)) # device type has 3 as dominating number; z coordinates all 0
print(colSums(is.na(final_df))) # no NAs
print(sapply(final_df, function(x) length(unique(x)))) # 1 singular ID, only 0.0 for z coords, only device type 1 and 3

# device type
print(table(final_df$device_type))
print(prop.table(table(final_df$device_type)))

# channel
print(table(final_df$channel)) # 2472000000 has smallest number of entries

# IDs
print(length(unique(final_df$id))) # confirmed only one ID 

# mac addresses
mac_counts = count(final_df, mac_address, sort=TRUE) # table of mac_address counts; columns "mac_address" and "n"
print(mac_counts) # 3 addresses have one entry only
