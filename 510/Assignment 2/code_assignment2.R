library(gridExtra)
setwd("G:/My Drive/Continuing Ed/UW MSIS/510 Data Mining & Analytics/Assignments/Assignment 2")
data.df <- read.csv("dailykos.csv", stringsAsFactors = TRUE)
set.seed(1000)
km <- kmeans(data.df, 7)

# question 1 
km$size

# question 2
top_n = 10
df_names <- c()
df_obvs <- matrix(nrow = 0,ncol = top_n)
for(i in 1:length(km$size)) { 
  df_names = append(df_names,paste("cluster", i, sep = ""))
  df_obvs = rbind(df_obvs,names(head(sort(km$centers[i,], decreasing = TRUE), n = top_n)))
}
top_obvs.df <- setNames(data.frame(aperm(df_obvs, resize = TRUE)),df_names)

png(filename = "A2-2.png",width = 800, height = 400,res = 72, bg = "white")
grid.table(top_obvs.df)
dev.off()
