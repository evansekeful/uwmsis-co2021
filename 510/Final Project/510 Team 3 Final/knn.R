require(caret)
require(FNN)

data.df.knn <- data.df

data.df.knn$price/500000
data.df.knn$price <- factor(round(data.df.knn$price/500000))
levels(data.df.knn$price)

set.seed(11) 
# partition data
train.index <- sample(1:nrow(data.df.knn), 0.6*nrow(data.df.knn))
train.df <- data.df.knn[train.index, ]
valid.df <- data.df.knn[-train.index,]

train.norm.df <- train.df
valid.norm.df <- valid.df


# normalize data compute mean and standard deviation of each column
norm.values <- preProcess(train.df[, 3:21], method=c("center", "scale"))

train.norm.df[, 4:21] <- predict(norm.values, train.df[, 4:21])
valid.norm.df[, 4:21] <- predict(norm.values, valid.df[, 4:21])
str(train.norm.df)

# use ?knn to find out more information
# It worth noting that the input argument cl must be a factor!
knn.pred <- knn(train.norm.df[, 4:21], valid.norm.df[, 4:21], 
                cl = train.norm.df[, 3], k = 5)

confusionMatrix(knn.pred, valid.norm.df[, 3])