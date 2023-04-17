#load data
df <- USArrests

#remove rows with missing values
df <- na.omit(df)

# df %>% ggqqplot()
# df %>% sqrt()

#scale each variable to have a mean of 0 and sd of 1
df <- scale(df)

#view first six rows of dataset
head(df)

library(cluster)
library(factoextra)

#create plot of number of clusters vs total within sum of squares
factoextra::fviz_nbclust(df, stats::kmeans, method = "wss")
