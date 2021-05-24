require(Metrics)
require(forecast)

# model 1 (cc >= 0.3)
data.df.m1 <- data.df[,c("bedrooms","bathrooms","sqft_living","floors","waterfront","view","grade","sqft_above","sqft_basement","lat","sqft_living15","price")]

set.seed(1234)

train.index <- sample(1:nrow(data.df.m1), nrow(data.df.m1)*0.7)
train.df <- data.df.m1[train.index, ]
valid.df <- data.df.m1[-train.index, ]
house1.lm<-lm(price~. ,data=train.df)

options(scipen = 999)
summary(house1.lm)

house1.lm.pred <- predict(house1.lm, valid.df)
options(scipen=999)
accuracy(house1.lm.pred, valid.df$price)

# model 2 (cc >= 0.5)
data.df.m2 <- data.df[,c("bathrooms","sqft_living","grade", "sqft_above","sqft_living15","price")]

set.seed(1234)
train.index <- sample(1:nrow(data.df.m2), nrow(data.df.m2)*0.7)
train.df <- data.df.m2[train.index, ]
valid.df <- data.df.m2[-train.index, ]
house2.lm<-lm(price~.,data=train.df)
options(scipen = 999)
summary(house2.lm)

house2.lm.pred <- predict(house2.lm, valid.df)
options(scipen=999)
accuracy(house2.lm.pred, valid.df$price)


# model 3 (all variables)
data.df.m3 <- data.df

set.seed(1234)
train.index <- sample(1:nrow(data.df), nrow(data.df)*0.7)
train.df <- data.df[train.index, ]
valid.df <- data.df[-train.index, ]
house3.lm<-lm(price~.,data=train.df)
options(scipen = 999)
summary(house3.lm)

house3.lm.pred <- predict(house3.lm, valid.df)
options(scipen=999)
accuracy(house3.lm.pred, valid.df$price)
