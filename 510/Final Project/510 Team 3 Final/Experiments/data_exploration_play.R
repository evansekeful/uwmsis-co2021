require(tidyverse)
require(lubridate)
require(gridExtra)
require(GGally)
require(maps)

theme_set(theme_bw())

# load data
data.df <- read.csv("kc_house_data.csv", stringsAsFactors = TRUE)
View(data.df)

# truncate date string
clean.date <- function(x) str_trunc(x,8,ellipsis = "")
data.df$date <- apply(data.df[2],2,clean.date)

# convert dates to date data type
data.df$date <- as.Date(data.df$date, "%Y%m%d")

# check for missing values


# some more janitorials, because who's living in a house with no bathrooms
data.df <- data.df[data.df$bedrooms != 0, ]
data.df <- data.df[data.df$bathrooms != 0, ]

# data structure
str(data.df)

# summary statistics
summary(data.df)

# time frame of house sales in sample
range(data.df$date, na.rm = TRUE)

# number of unique zip codes
kc_zipcodes <- unique(data.df$zipcode)
length(kc_zipcodes)

# exploratory scatter plots - round 1
p1 <- ggplot(data = data.df) + geom_point(aes(x=sqft_living, y=(price/1000)))
p2 <- ggplot(data = data.df) + geom_point(aes(x=sqft_lot, y=(price/1000)))
p3 <- ggplot(data = data.df) + geom_point(aes(x=sqft_above, y=(price/1000)))
p4 <- ggplot(data = data.df) + geom_point(aes(x=bedrooms, y=(price/1000)))
p5 <- ggplot(data = data.df) + geom_point(aes(x=sqft_living15, y=(price/1000)))
p6 <- ggplot(data = data.df) + geom_point(aes(x=sqft_lot15, y=(price/1000)))

grid.arrange(p1,p2,p3,p4,p5,p6, nrow = 2)

# what's going on with bedrooms?
ggplot(data = data.df) + geom_point(aes(x=bedrooms, y=(price/1000)))

# remove row with 30 bedrooms and covert to a factor
data.df <- data.df[data.df$bedrooms < 20, ]
data.df$bedrooms <- as.factor(data.df$bedrooms)

# exploratory scatter plots - round 2, with color
p7 <- ggplot(data = data.df) + geom_point(aes(x=sqft_living, y=(price/1000), color = bedrooms))
p8 <- ggplot(data = data.df) + geom_point(aes(x=sqft_living15, y=(price/1000), color = bedrooms))
p9 <- ggplot(data = data.df) + geom_point(aes(x=sqft_lot, y=(price/1000), color = bedrooms))
p10 <- ggplot(data = data.df) + geom_point(aes(x=sqft_lot15, y=(price/1000), color = bedrooms))

grid.arrange(p7,p8,p9,p10,nrow = 2)

# calculate some extra variables
data.df <- data.df %>% mutate(age = as.numeric(format(data.df$date,"%Y")) - data.df$yr_built)
data.df <- data.df %>% mutate(age_renovated = ifelse(data.df$yr_renovated > 0,as.numeric(format(data.df$date,"%Y")) - data.df$yr_renovated,data.df$age))

# couple more scatter plots
p11 <- ggplot(data = data.df) + geom_point(aes(x=age, y=(price/1000)))
p12 <- ggplot(data = data.df) + geom_point(aes(x=age_renovated, y=(price/1000)))

grid.arrange(p11,p12,nrow = 1)

# correlation plot
data.df$bedrooms <- as.numeric(data.df$bedrooms)
ggcorr(data.df[,3:23],name = "corr", label = TRUE, hjust = 1, label_size = 2.5,
       angle = -45, size = 3)

# histograms - bedrooms, grade, condition
ggplot(data= data.df,aes(x=bedrooms)) + geom_histogram(col = 'black', binwidth = 1)
ggplot(data= data.df,aes(x=floors)) + geom_histogram(col = 'black', binwidth = 1)
ggplot(data= data.df,aes(x=grade)) + geom_histogram(col = 'black', binwidth = 1)
ggplot(data= data.df,aes(x=condition)) + geom_histogram(col = 'black', binwidth = 1)

# distribution of price - skewed right
boxplot(data.df$price/1000)

# distribution of square footage - similarly skewed
boxplot(data.df$sqft_living)

# distribution of age 
boxplot(data.df$age)

# boxplots of other features
ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(bedrooms), y = (price/1000)))
ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(floors), y = (price/1000)))
ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(grade), y = (price/1000)))
ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(condition), y = (price/1000)))
ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(waterfront), y = (price/1000)))
ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(view), y = (price/1000)))

# summarize data by month and relabel month
data.bymonth <- data.df[c("date","price")]
data.bymonth <- data.bymonth %>% mutate(month = month(data.bymonth$date))
data.bymonth <- data.bymonth %>% mutate(year = year(data.bymonth$date))
data.bymonth <- data.bymonth %>% group_by(month,year) %>%
  summarise(price.bymonth = mean(price/1000))

data.bymonth$month <- as.Date(gsub(" ","",paste(data.bymonth$year,"-",data.bymonth$month,"-01"), fixed = TRUE), "%Y-%m-%d")

# line plot - sales over time
ggplot(data = data.bymonth, aes(x=month, y=price.bymonth)) + geom_line() + xlab("month") + ylab("average sell price") + scale_x_date(date_labels = "%b-%Y")

# plot sales on a map
king.county <- map_data('county','washington.king')


ggplot() + geom_polygon(data = king.county, aes(x=long, y = lat, group = group))+ 
  coord_fixed(1.3) 

# geocode - bring in geodata and approximate city
zip.df <- read.csv("kc_zips.csv", stringsAsFactors = TRUE)
zip.df <- zip.df[2:5]

zip.df$LON <- trunc(zip.df$LON*10^2)/10^2
zip.df$LAT <- trunc(zip.df$LAT*10^2)/10^2
zip.df <- unique(zip.df)

data.df$LON <- trunc(data.df$long*10^2)/10^2
data.df$LAT <- trunc(data.df$lat*10^2)/10^2
data.df$ZIP5 <- data.df$zipcode

data.df <- merge(x = data.df, y = zip.df, by = c("LON", "LAT", "ZIP5"))
data.df <- data.df[order(data.df$CTYNAME, na.last = TRUE), ]
data.df <- data.df[!duplicated(data.df$id), ]
data.df <- data.df[4:26]
data.df <- data.df[data.df$CTYNAME != "", ]

kc_cities <- unique(data.df$CTYNAME)
length(kc_cities)

# correlation between city and price


# reference - assigning variable names in s loop
for(i in 1:10) { 
  nam <- paste("A", i, sep = "")
  assign(nam, funtions)
}