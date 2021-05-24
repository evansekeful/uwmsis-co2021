library(caret)
#1
data.df <- read.csv("phone_sale.csv", stringsAsFactors = TRUE)

data.df$Phone_sale <- as.numeric(data.df$Phone_sale == "Yes")

data.df$Any_cc_miles_12mo <- factor(data.df$Any_cc_miles_12mo)
levels(data.df$Any_cc_miles_12mo) <- c("N","Y")
data.df$Any_cc_miles_12mo <- relevel(data.df$Any_cc_miles_12mo, ref = "Y")

#2
selected.var <- c(5,9,10)
selected.df <- data.df[, selected.var]

#3
load("a4_TrainIndex.rda")
train.df <- selected.df[train.index, ] 
valid.df <- selected.df[-train.index, ]

#4
logit.reg <- glm(Phone_sale ~ ., data = train.df, family = "binomial")
summary(logit.reg)

#5
Bonus_trans <- 50
Any_cc_miles_12mo <- "Y"
Phone_sale <- 1
test.probability <- data.frame(Bonus_trans,Any_cc_miles_12mo,Phone_sale)

predict(logit.reg, test.probability,  type = "response")

#6
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)
confusionMatrix(factor(pred), factor(valid.df$Phone_sale), positive = "1")

#7
sum(train.df$Phone_sale == 1)/nrow(train.df)
