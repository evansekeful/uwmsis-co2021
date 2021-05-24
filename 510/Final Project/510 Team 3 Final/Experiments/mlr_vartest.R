require(Metrics)
require(forecast)

# model 1 (cc >= 0.3)
set.seed(1234)

train.index <- sample(1:nrow(data.df), nrow(data.df)*0.7)
train.df <- data.df[train.index, ]
valid.df <- data.df[-train.index, ]
house1.lm<-lm(price~bedrooms*bathrooms*sqft_living*floors*waterfront*view*grade*sqft_above*sqft_basement*lat*sqft_living15,data=train.df)

options(scipen = 999)
summary(house1.lm)

house1.lm.pred <- predict(house1.lm, valid.df)
options(scipen=999)
accuracy(house1.lm.pred, valid.df$price)

# method 2
data.df.m1 <- data.df[,c("bedrooms","bathrooms","sqft_living","floors","waterfront","view","grade","sqft_above","sqft_basement","lat","sqft_living15","price")]

set.seed(1234)

train.index <- sample(1:nrow(data.df.m1), nrow(data.df.m1)*0.7)
train.df <- data.df.m1[train.index, ]
valid.df <- data.df.m1[-train.index, ]
house1.lm<-lm(price~. ,data=train.df)

sink("method2-output.txt")
options(scipen = 999)
summary(house1.lm)

house1.lm.pred <- predict(house1.lm, valid.df)
options(scipen=999)
accuracy(house1.lm.pred, valid.df$price)
sink()

# method 3
set.seed(1234)

train.index <- sample(1:nrow(data.df), nrow(data.df)*0.7)
train.df <- data.df[train.index, ]
valid.df <- data.df[-train.index, ]
house1.lm<-lm(price~bedrooms+bathrooms+sqft_living+floors+waterfront+view+grade+sqft_above+sqft_basement+lat+sqft_living15,data=train.df)

sink("method3-output.txt")
options(scipen = 999)
summary(house1.lm)

house1.lm.pred <- predict(house1.lm, valid.df)
options(scipen=999)
accuracy(house1.lm.pred, valid.df$price)
sink()
