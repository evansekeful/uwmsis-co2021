# load data
data.df <- read.csv("kc_house_data.csv", stringsAsFactors = TRUE)

# truncate date string
clean.date <- function(x) str_trunc(x,8,ellipsis = "")
data.df$date <- apply(data.df[2],2,clean.date)

# convert dates to date data type
data.df$date <- as.Date(data.df$date, "%Y%m%d")

# some more janitorials, because who's living in a house with no bathrooms
data.df <- data.df[data.df$bedrooms != 0, ]
data.df <- data.df[data.df$bathrooms != 0, ]

# remove row with 30 bedrooms
data.df <- data.df[data.df$bedrooms < 20, ]

# calculated columns
data.df <- data.df %>% mutate(age = as.numeric(format(data.df$date,"%Y")) - data.df$yr_built)
data.df <- data.df %>% mutate(age_renovated = ifelse(data.df$yr_renovated > 0,as.numeric(format(data.df$date,"%Y")) - data.df$yr_renovated,data.df$age))

