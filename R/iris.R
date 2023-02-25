
 library(tidyverse)
 library(knitr)
library(rmarkdown)
library(caret)
library(randomForest)
library(kernlab)

iris <- read.table("C:\\Users\\Acer\\Downloads\\iris.csv", sep=",", header=TRUE) 
str(iris)
set.seed(2022)
 data <- iris
 head(data)
dim(data)
 split1<- sample(c(rep(0, 0.8 * nrow(data)), rep(1, 0.2 * nrow(data))))
 split1
 train <- data[split1 == 0, ]
head(train)
dim(train)
test <- data[split1== 1, ]
 head(test)
dim(test)
colnames(train)
 train$petal.length
 PL <- train$petal.length
 summary(PL)
boxplot(PL)
boxplot(iris[, 1:4])
 sd(PL)
 SW <- iris$sepal.width
sd(SW)
 pie(counts)
 barplot(counts) 
 ma <- as.matrix(train[, 1:4])
 colMeans(ma)
 colSums(ma)
 control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
fit.lda <- train(variety~., data=train, method="lda", metric=metric, trControl=control)
fit.cart <- train(variety~., data=train, method="rpart", metric=metric, trControl=control)
 train(variety~., data=train, method="knn", metric=metric, trControl=control)
 fit.svm <- train(variety~., data=train, method="svmRadial", metric=metric, trControl=control)
fit.rf <- train(variety~., data=train, method="rf", metric=metric, trControl=control)

results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
 summary(results)
dotplot(results)

