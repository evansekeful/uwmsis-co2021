library(rpart)
library(rpart.plot)
library(caret)
data.df <- read.csv("FlightDelay.csv", stringsAsFactors = TRUE)

# 1
load("TrainIndex.rda") # which stores the variable train.index 
train.df <- data.df[train.index, ] 
valid.df <- data.df[-train.index, ]

# 2
train.df.var <- train.df[c("CRS_DEP_TIME","CARRIER","DEST","DISTANCE","ORIGIN","DAY_WEEK","Weather","Flight.Status")]
default.ct <- rpart(Flight.Status~., data = train.df.var, method = "class")

prp(default.ct, type=3, extra = 1)

#3
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")
confusionMatrix(default.ct.point.pred, factor(valid.df$Flight.Status))

#6
train.df.var2 <- train.df[c("CRS_DEP_TIME","CARRIER","DEST","DISTANCE","ORIGIN","DAY_WEEK","Flight.Status")]
default2.ct <- rpart(Flight.Status~., data = train.df.var2, method = "class")

prp(default2.ct, type=3, extra = 1)
