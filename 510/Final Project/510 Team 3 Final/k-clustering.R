require(caret)
# compute mean and standard deviation of each column
norm.values <- preProcess(data.df, method=c("center", "scale"))

# we perform the transformation/normalization
data.df.norm <- predict(norm.values, data.df)

# select variables to perform cluster analysis
data.df.var <- data.df.norm[3:21]

# set seed for reproducibility
set.seed(666)
km.v1 <- kmeans(data.df.var, 5)

# check out cluster centroids
v1.centroids <- as.data.frame(km.v1$centers)
View(v1.centroids)

# try some more clusters and plot elbow chart
wss <- function(k) {
  kmeans(data.df.var, k, nstart = 10 )$tot.withinss
}

k.values <- 1:15
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     col = "#1f61b7",
     xlab="number of clusters K",
     ylab="total within-clusters sum of squares")

# peak at centroids for best clustering model
set.seed(666)
km.v2 <- kmeans(data.df.var, 9)
v2.centroids <- as.data.frame(km.v2$centers)
View(v2.centroids)
write.csv(v2.centroids,"k-clusters.csv")

# add clusters to master data frame
data.df$km.cluster=km.v2$cluster

# subset data frame by clusters
k_list.dfs <- split(data.df, as.factor(data.df$km.cluster))

# explore features of clusters through plots
# (haven't figured out how to put the base boxplot on a grid)
for(i in 1:9) { 
  boxplot(df_list[[i]]$price/1000, ylab = "price")
}

for(i in 1:9) { 
  boxplot(df_list[[i]]$age/1000, ylab = "age")
}

for(i in 1:9) { 
  boxplot(df_list[[i]]$sqft_living/1000, ylab = "sq. ft. living space")
}

