library(caret)
library(rpart)
library(rpart.plot)

# convert variables to factors
data.df.class <- data.df

# floors
data.df.class$floors<- as.factor(data.df.class$floors)
levels(data.df.class$floors)

# waterfront
data.df.class$waterfront<- as.factor(data.df.class$waterfront)
levels(data.df.class$waterfront)<- c("No Waterfront", "Waterfront")
levels(data.df.class$waterfront)

# condition
data.df.class$condition<- as.factor(data.df.class$condition)
levels(data.df.class$condition) <- c("Poor","Fair","Average","Good","Very Good")
levels(data.df.class$condition)

# grade
data.df.class$grade<- as.factor(data.df.class$grade)
levels(data.df.class$grade)

# zipcode
data.df.class$zipcode<- as.factor(data.df.class$zipcode)
levels(data.df.class$zipcode)

data.df.class$view<- as.factor(data.df.class$view)
levels(data.df.class$view)

data.df.class$bedrooms <- as.numeric(data.df.class$bathrooms)

# count bins
kc_price <- unique(data.df.class$price)
length(kc_price)

# CART
set.seed(1234)
train.index <- sample(1:nrow(data.df.class), nrow(data.df.class)*0.7)  

train.df <- data.df.class[train.index, ]
valid.df <- data.df.class[-train.index, ]

# regression tree
default.full.ct <- rpart(price ~ bedrooms + bathrooms + sqft_living + sqft_above + 
                           grade + sqft_living15 + sqft_lot + waterfront + view + floors +
                           condition + sqft_basement, data= train.df, method = "anova",
                         control = rpart.control(cp = -1, minsplit = 3 , maxdepth = 3))

prp(default.full.ct, box.palette = "Blues")

# classification tree 1 - bedrooms & bathrooms
default.ct <- rpart(price ~ bathrooms + bedrooms, data= train.df, method = "class",
                    control = rpart.control(cp = -1, minsplit = 1 , maxdepth = 3)) 
prp(default.ct, box.palette = "Blues")

# confusion matrix 1 
default.ct.point.prod <- predict(default.ct, train.df, type = "class")
confusionMatrix(factor(default.ct.point.prod), factor(train.df$price))

# convert price into bins $10,000 increments
data.df.class$price/10000
data.df.class$price <- factor(round(data.df.class$price/10000))
levels(data.df.class$price)

# classification tree 2 - most of the predictors
default.full.ct <- rpart(price ~ bedrooms + bathrooms + sqft_living + sqft_above + 
                           grade + sqft_living15 + sqft_lot + waterfront + view + floors +
                           condition + sqft_basement, data= train.df, method = "class",
                         control = rpart.control(cp = -1, minsplit = 1 , maxdepth = 3))
prp(default.full.ct, box.palette = "Blues")

# confusion matrix 2
default.ct.point.prod <- predict(default.full.ct, train.df, type = "class")
confusionMatrix(factor(default.ct.point.prod), factor(train.df$price))

# we don't speak of classification tree 3

# classification tree 4 - waterfront, view, floors
default.total.ct <- rpart(price ~ waterfront + view + floors, data= train.df, method = "class",
                          control = rpart.control(cp = -1, minsplit = 1 , maxdepth = 3))
prp(default.total.ct, box.palette = "Blues")

# confusion matrix 4
default.ct.point.prod <- predict(default.total.ct, train.df, type = "class")
confusionMatrix(factor(default.ct.point.prod), factor(train.df$price))

# classification tree 5 - just waterfront

default.total.ct <- rpart(price ~ waterfront, data= train.df, method = "class",
                          control = rpart.control(cp = -1, minsplit = 1 , maxdepth = 3))
prp(default.total.ct, box.palette = "Blues")

# confusion matrix 5
default.ct.point.prod <- predict(default.total.ct, train.df, type = "class")
confusionMatrix(factor(default.ct.point.prod), factor(train.df$price))

# classification tree 6 - year build & year renovated
default.total.ct <- rpart(price ~ yr_built + yr_renovated, data= train.df, method = "class",
                          control = rpart.control(cp = -1, minsplit = 1 , maxdepth = 3))
prp(default.total.ct, box.palette = "Blues")

# confusion matrix 6
default.ct.point.prod <- predict(default.total.ct, train.df, type = "class")
confusionMatrix(factor(default.ct.point.prod), factor(train.df$price))
