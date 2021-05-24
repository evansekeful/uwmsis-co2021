require(tidyverse)
# import geo data - very big file, takes a few minutes
zip.df <- read.csv("kc_addresses.csv", stringsAsFactors = TRUE)

# select columns
zip.df <- zip.df[c("ZIP5","CTYNAME","LON","LAT")]

# remove duplicates
zip.df <- unique(zip.df)

# remove blanks
zip.df <- subset(zip.df, zip.df$CTYNAME != "")

# export csv for later use
write.csv(zip.df, "kc_zips.csv")
