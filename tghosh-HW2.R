#
# Author: Trina Ghosh
# Purpose: IST 707 - HW 2 - kMeans & Decision Trees
#
#

# Loading the dataset
file.choose()
Essay <- read.csv("/Users/trinaghosh/Desktop/Trina/IST 707/HW 2/Disputed_Essay_data.csv"
                  , header = T, stringsAsFactors = F)
summary(Essay)
str(Essay)

#Loading necessary packages
install.packages("factoextra")
library(factoextra)

#To detect NA values
sum(!complete.cases(Essay))


#Visualizing distance
distance <- get_dist(Essay, method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Running the default kmeans fucntion
set.seed(120)
k1 <- kmeans(Essay[,3:72], centers = 2)
str(k1)
table(k1$cluster, Essay$author)
#Visualizing the default kmeans function
fviz_cluster(k1, data = Essay[,3:72])

k2 <- kmeans(Essay[,3:72], centers = 2, iter.max = 1)
str(k2)
table(k2$cluster, Essay$author)
fviz_cluster(k2, data = Essay[,3:72], fill="red")
#Not much difference in both kmeans fucntion outputs.

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot Essay for k = 1 to k = 15.
kmax <- 20
wss <- sapply(1:kmax, 
              function(k)
                {kmeans(Essay[,3:72], k, nstart=50,iter.max = 100)$tot.withinss})
wss
par(bty="o")
plot(1:kmax, wss,
     type="b", pch = 19, 
     xlab="Number of clusters (k)",
     ylab="Total within-ness",
     main="Elbow Method for Optimization"
     )

?hclust





#Dividing the dataset into test and train
test_Essay <- Essay[Essay$author == "dispt",]
test_Essay <- test_Essay[,-2]
rownames(test_Essay) <- NULL
df <- Essay[Essay$author != "dispt",]
df <- df[,-2]


row_num <- sample(nrow(df),size = 0.8*nrow(df), replace = F)
train_df <- df[row_num,]
valid_df <- df[-row_num,]
rownames(train_df) <- NULL
rownames(valid_df) <- NULL
train_df <- droplevels(train_df)
valid_df <- droplevels(valid_df)

fit1 <- rpart(author ~ ., data = train_df, method = "class")
summary(fit1)
predicted1 <- predict(fit1, valid_df)
predicted1
fancyRpartPlot(fit1)

fit2 <- rpart(author ~ ., data = test_Essay, method = "class")
predicted2 <- predict(fit2, test_Essay)
predicted2
fancyRpartPlot(fit2)




#D_T <- cbind(tra)
#x <- cbind(train_Essay, test_Essay)
# grow tree 
df <- train_Essay[,-2]

train_Essay <- droplevels(train_Essay)
test_Essay <- droplevels(test_Essay)

fit <- rpart(author ~ ., data = df
             , method="class")
summary(fit)
#Predict Output 
predicted <- predict(fit,test_Essay,type = "prob")
predicted 

library(rattle)
fancyRpartPlot(fit)

#SAHI WALA CODE:

test_Essay <- Essay[Essay$author == "dispt",]
test_Essay <- test_Essay[,-2]
rownames(test_Essay) <- NULL

df <- Essay[Essay$author != "dispt",]
df <- df[,-2]


row_num <- sample(nrow(df),size = 0.8*nrow(df), replace = F)
train_df <- df[row_num,]
valid_df <- df[-row_num,]
rownames(train_df) <- NULL
rownames(valid_df) <- NULL
train_df <- droplevels(train_df)
valid_df <- droplevels(valid_df)

#Training & Validating the model:
DTmodel <- train(author ~ ., data = train_df
                 , method = "rpart")

predicted1 <- predict(DTmodel, newdata = valid_df, type = "prob")
predicted1
table(predict(DTmodel, newdata = valid_df), valid_df$author)

predicted2 <- predict(DTmodel, newdata = test_Essay, type = "prob")
predicted2
table(predict(DTmodel, newdata = test_Essay), test_Essay$author)

#Tuning & Pruning

DTmodel_tune <- train(author ~ ., data = train_df
                      , method = "rpart", tuneLength = 15)
print(DTmodel_tune$finalModel)

table(predict(DTmodel_tune, newdata = valid_df, type = "raw")
      , valid_df$author)

DTmodel_tune2 <- train(author ~ ., data = train_df
                       , method = "rpart"
                       , tuneGrid = expand.grid(cp = seq(0, 0.1, 0.01)))
print(DTmodel$finalModel)

DTmodel_preprune <- train(author ~ ., data = train_df
                          , method = "rpart"
                          , metric = "Accuracy"
                          , tuneLength = 8
                          , control = rpart.control(minsplit = 50, 
                                                    minbucket = 20, 
                                                    maxdepth = 5)
                          )
print(DTmodel_preprune$finalModel)

DTmodel_postprune <- prune(DTmodel$finalModel, cp = 0.2)
print(DTmodel_postprune)
plot(DTmodel$finalModel)
text(DTmodel$finalModel)

table(predict(DTmodel, newdata = test_Essay), test_Essay$author)
fancyRpartPlot(DTmodel$finalModel)
library(rpart.plot)
prp(DTmodel$finalModel)


