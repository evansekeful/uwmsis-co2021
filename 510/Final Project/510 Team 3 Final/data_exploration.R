require(tidyverse)
require(lubridate)
require(gridExtra)
require(GGally)

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
p1 <- ggplot(data = data.df) + geom_point(aes(x=sqft_living, y=(price/1000)),color = "#1f61b7") + ylab("price in thousands") + xlab("sq. ft. living space")
p2 <- ggplot(data = data.df) + geom_point(aes(x=sqft_lot, y=(price/1000)),color = "#1f61b7") + ylab("price in thousands") + xlab("sq. ft. lot")
p3 <- ggplot(data = data.df) + geom_point(aes(x=sqft_above, y=(price/1000)),color = "#1f61b7") + ylab("price in thousands") + xlab("sq. ft. above basement")
p4 <- ggplot(data = data.df) + geom_point(aes(x=bedrooms, y=(price/1000)),color = "#1f61b7") + ylab("price in thousands")
p5 <- ggplot(data = data.df) + geom_point(aes(x=sqft_living15, y=(price/1000)),color = "#1f61b7") + ylab("price in thousands") + xlab("neighbor's sq. ft. living space")
p6 <- ggplot(data = data.df) + geom_point(aes(x=sqft_lot15, y=(price/1000)),color = "#1f61b7") + ylab("price in thousands") + xlab("neighbor's sq. ft. lot")

grid.arrange(p1,p2,p3,p4,p5,p6, nrow = 2)

# what's going on with bedrooms?
ggplot(data = data.df) + geom_point(aes(x=bedrooms, y=(price/1000)), color = "#1f61b7") + ylab("price in thousands")

# remove row with 30 bedrooms and covert to a factor
data.df <- data.df[data.df$bedrooms < 20, ]
data.df$bedrooms <- as.factor(data.df$bedrooms)

# exploratory scatter plots - round 2, with color
p7 <- ggplot(data = data.df) + geom_point(aes(x=sqft_living, y=(price/1000), color = bedrooms)) + ylab("price in thousands") + xlab("sq. ft. living space")
p8 <- ggplot(data = data.df) + geom_point(aes(x=sqft_living15, y=(price/1000), color = bedrooms)) + ylab("price in thousands") + xlab("neighbor's sq. ft. living space")
p9 <- ggplot(data = data.df) + geom_point(aes(x=sqft_lot, y=(price/1000), color = bedrooms)) + ylab("price in thousands") + xlab("sq. ft. lot")
p10 <- ggplot(data = data.df) + geom_point(aes(x=sqft_lot15, y=(price/1000), color = bedrooms)) + ylab("price in thousands") + xlab("neighbor's sq. ft. lot")

grid.arrange(p7,p8,p9,p10,nrow = 2)

# calculate some extra variables
data.df <- data.df %>% mutate(age = as.numeric(format(data.df$date,"%Y")) - data.df$yr_built)
data.df <- data.df %>% mutate(age_renovated = ifelse(data.df$yr_renovated > 0,as.numeric(format(data.df$date,"%Y")) - data.df$yr_renovated,data.df$age))

# couple more scatter plots
p11 <- ggplot(data = data.df) + geom_point(aes(x=age, y=(price/1000)),color = "#1f61b7") + ylab("price in thousands")
p12 <- ggplot(data = data.df) + geom_point(aes(x=age_renovated, y=(price/1000)),color = "#1f61b7") + ylab("price in thousands")

grid.arrange(p11,p12,nrow = 1)

# correlation plot
data.df$bedrooms <- as.numeric(data.df$bedrooms)
ggcorr(data.df[,3:23],name = "corr", label = TRUE, hjust = 1, label_size = 2,
       angle = -45, size = 3)

# histograms - bedrooms, grade, condition
h1 = ggplot(data= data.df,aes(x=bedrooms)) + geom_histogram(col = "#ffffff",fill = "#1f61b7", binwidth = 1)
h2 = ggplot(data= data.df,aes(x=floors)) + geom_histogram(col = "#ffffff",fill = "#1f61b7", binwidth = 1)
h3 = ggplot(data= data.df,aes(x=grade)) + geom_histogram(col = "#ffffff",fill = "#1f61b7", binwidth = 1)
h4 = ggplot(data= data.df,aes(x=condition)) + geom_histogram(col = "#ffffff",fill = "#1f61b7", binwidth = 1)

grid.arrange(h1,h2,h3,h4,nrow = 2)

# distribution of price - skewed right
boxplot(data.df$price/1000, ylab = "price", col = "#1f61b7", medcol = "#ffffff", whiskcol = "#1f61b7", staplecol = "#1f61b7", boxcol = "#ffffff") 

# distribution of square footage - similarly skewed
boxplot(data.df$sqft_living, ylab = "sq. ft. living space", col = "#1f61b7", medcol = "#ffffff", whiskcol = "#1f61b7", staplecol = "#1f61b7", boxcol = "#ffffff")

# distribution of age 
boxplot(data.df$age, ylab = "age", col = "#1f61b7", medcol = "#ffffff", whiskcol = "#1f61b7", staplecol = "#1f61b7", boxcol = "#ffffff")

# boxplots of other features
b1 = ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(bedrooms), y = (price/1000)), col = "#1f61b7") + ylab("price in thousands") + xlab("bedrooms")
b2 = ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(floors), y = (price/1000)), col = "#1f61b7") + ylab("price in thousands") + xlab("floors")
b3 = ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(grade), y = (price/1000)), col = "#1f61b7") + ylab("price in thousands") + xlab("grade")
b4 = ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(condition), y = (price/1000)), col = "#1f61b7") + ylab("price in thousands") + xlab("condition")
b5 = ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(waterfront), y = (price/1000)), col = "#1f61b7") + ylab("price in thousands") + xlab("waterfront")
b6 = ggplot(data = data.df) + geom_boxplot(aes(x = as.factor(view), y = (price/1000)), col = "#1f61b7") + ylab("price in thousands") + xlab("view")

grid.arrange(b1, b2, b3, b4, b5, b6, nrow = 3)

# summarize data by month and relabel month
data.bymonth <- data.df[c("date","price")]
data.bymonth <- data.bymonth %>% mutate(month = month(data.bymonth$date))
data.bymonth <- data.bymonth %>% mutate(year = year(data.bymonth$date))
data.bymonth <- data.bymonth %>% group_by(month,year) %>%
  summarise(price.bymonth = mean(price/1000))

data.bymonth$month <- as.Date(gsub(" ","",paste(data.bymonth$year,"-",data.bymonth$month,"-01"), fixed = TRUE), "%Y-%m-%d")

# line plot - sales over time
ggplot(data = data.bymonth, aes(x=month, y=price.bymonth)) + geom_line(color = "#1f61b7") + xlab("month") + ylab("average sell price") + scale_x_date(date_labels = "%b-%Y")

