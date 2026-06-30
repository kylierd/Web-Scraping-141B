proc.time()
library(tidyverse)
text = readLines("/Users/kd/Downloads/STA 141B/offline")
text = text[substring(text, 1, 1) != "#"] # check if first char of every line is # to eliminate # lines

parse_line = function(line) {
  parts = strsplit(line, ";")[[1]]
  
  t_val = sub("t=", "", parts[1])
  id_val = sub("id=", "", parts[2])
  
  pos_vals = strsplit(sub("pos=", "", parts[3]), ",")[[1]] # gives the 0.0,0.0,0.0 (ex) divided into three parts
  x_val = pos_vals[1]
  y_val = pos_vals[2]
  z_val = pos_vals[3]
  
  deg_val = sub("degree=", "", parts[4])
  
  # mac addresses
  rest = parts[-(1:4)] # consider all parts after degree, aka the mac addresses
  macs = rest[rest != ""] # clean empty strings
  
  macs_parsed = lapply(macs, function(entry) { # applying to macs variable the following function:
    addr_split = strsplit(entry, "=")[[1]] # before = is address, after = is 3 descriptors
    if (length(addr_split) < 2) return(NULL) # checks for both the address and 3 descriptor item (2 items total)
    
    address = addr_split[1] # first item
    
    vals = strsplit(addr_split[2], ",")[[1]] # second item
    signal = ifelse(length(vals) >= 1, vals[1], NA)
    channel = ifelse(length(vals) >= 2, vals[2], NA)
    device_type = ifelse(length(vals) >= 3, vals[3], NA)
    
    data.frame(
      mac_address=address,
      signal=signal,
      channel=channel,
      device_type=device_type
    )
  })
  
  macs_parsed = macs_parsed[!sapply(macs_parsed, is.null)]
  if (length(macs_parsed) == 0) return(NULL)
  
  mac_df = do.call(rbind, macs_parsed)
  mac_df$timestamp=t_val
  mac_df$id=id_val
  mac_df$x=x_val
  mac_df$y=y_val
  mac_df$z=z_val
  mac_df$degree=deg_val
  
  return(mac_df)
}

library(anytime)
library(dplyr)

result_list = lapply(text, parse_line)
final_df = do.call(rbind, result_list)

final_df = final_df %>%
  mutate(
    timestamp=as.POSIXct(as.numeric(timestamp)/1000, origin="1970-01-01", tz="UTC"),
    x=as.numeric(x),
    y=as.numeric(y),
    z=as.numeric(z),
    degree=as.numeric(degree),
    signal=as.integer(signal),
    channel=as.numeric(channel),
    device_type=as.integer(device_type) 
  )

final_df = final_df[, c("timestamp", "id", "x", "y", "z",
                        "degree", "mac_address", "signal", "channel", "device_type")]

saveRDS(final_df, "final_df.rds")
normalizePath("final_df.rds")
proc.time()