# which cluster performs better with current performant mlr model
# output to text file
sink("km_individual-models_all.txt")
for(i in 1:9) { 
  result_label <- paste("Cluster",i,"Model w/ All Variables")
  print(result_label)
  
  set.seed(1234)
  train.index <- sample(1:nrow(k_list.dfs[[i]]), nrow(k_list.dfs[[i]])*0.7)
  train.df <- k_list.dfs[[i]][train.index, ]
  valid.df <- k_list.dfs[[i]][-train.index, ]
  house.lm<-lm(price~.,data=train.df)
  options(scipen = 999)
  print(summary(house.lm))
  
  house.lm.pred <- predict(house.lm, valid.df)
  options(scipen=999)
  print(accuracy(house.lm.pred, valid.df$price))
}
sink()

# test (cc >= 0.3)
sink("km_individual-models_cc03.txt")
for(i in 1:9) { 
  result_label <- paste("Cluster",i,"Model w/ All Variables cc >= 0.3")
  print(result_label)
  
  tmp <- k_list.dfs[[i]][,c("bedrooms","bathrooms","sqft_living","floors","waterfront","view","grade","sqft_above","sqft_basement","lat","sqft_living15","price")]
  
  set.seed(1234)
  train.index <- sample(1:nrow(tmp), nrow(tmp)*0.7)
  train.df <- tmp[train.index, ]
  valid.df <- tmp[-train.index, ]
  house.lm<-lm(price~.,data=train.df)
  options(scipen = 999)
  print(summary(house.lm))
  
  house.lm.pred <- predict(house.lm, valid.df)
  options(scipen=999)
  print(accuracy(house.lm.pred, valid.df$price))
}
sink()

# test (cc >= 0.5)
sink("km_individual-models_cc05.txt")
for(i in 1:9) { 
  result_label <- paste("Cluster",i,"Model w/ All Variables cc >= 0.5")
  print(result_label)
  
  tmp <- k_list.dfs[[i]][,c("bathrooms","sqft_living","grade", "sqft_above","sqft_living15","price")]
  
  set.seed(1234)
  train.index <- sample(1:nrow(tmp), nrow(tmp)*0.7)
  train.df <- tmp[train.index, ]
  valid.df <- tmp[-train.index, ]
  house.lm<-lm(price~.,data=train.df)
  options(scipen = 999)
  print(summary(house.lm))
  
  house.lm.pred <- predict(house.lm, valid.df)
  options(scipen=999)
  print(accuracy(house.lm.pred, valid.df$price))
}
sink()