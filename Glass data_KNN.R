# Upload/Read glass data
glass <- read.csv(file.choose())

# Exploratory Data Analysis

# table of diagnosis
table(glass$Type)

str(glass$Type)
# recode Types into factor and rename the types for better classification
glass$Type <- factor(glass$Type, levels = c("1", "2", "3", "4", "5", "6", "7"), labels = c("Type A", "Type B", "Type C", "Type D", "Type E", "Type F", "Type G"))

# table or proportions with more informative labels
round(prop.table(table(glass$Type)) * 100, digits = 2)

# summarize any three numeric features
summary(glass[c("RI", "Mg", "Ca")])

# create normalization function for easy working and uniform working of data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the glass data
glass_n <- as.data.frame(lapply(glass[1:9], normalize))
glass_n <- cbind(glass_n, glass$Type)

# confirm that normalization worked
summary(glass_n$Ca)

# create training and test data
library(caret)
indatapartition <- createDataPartition(glass_n$`glass$Type`, p=.70, list = FALSE)
glass_train <- glass_n[indatapartition,]
glass_test <- glass_n[-indatapartition,]


#---- Training a model on the data ----
# load the "class" library
install.packages("class")
library(class)
glass_test_pred <- knn(train = glass_train[, -10], test = glass_test[, -10],
                      cl = glass_train[, 10], k = 9)

## ---- Evaluating model performance ---- ##
confusion_test <- table(x = glass_test[, 10], y = glass_test_pred)
confusion_test
Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy 

# testing with different k values for accuracy
# k=21
glass_test_pred <- knn(train = glass_train[, -10], test = glass_test[, -10],
                       cl = glass_train[, 10], k = 21)

## ---- Evaluating model performance ---- ##
confusion_test <- table(x = glass_test[, 10], y = glass_test_pred)
confusion_test
Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy # 67.21

# k=13
glass_test_pred <- knn(train = glass_train[, -10], test = glass_test[, -10],
                       cl = glass_train[, 10], k = 13)

## ---- Evaluating model performance ---- ##
confusion_test <- table(x = glass_test[, 10], y = glass_test_pred)
confusion_test
Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy # 62.29

# k = 3
glass_test_pred <- knn(train = glass_train[, -10], test = glass_test[, -10],
                       cl = glass_train[, 10], k = 3)

## ---- Evaluating model performance ---- ##
confusion_test <- table(x = glass_test[, 10], y = glass_test_pred)
confusion_test
Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy # 63.93

# Training Accuracy to compare against test accuracy
glass_train_pred <- knn(train = glass_train[, -10], test = glass_train[, -10], cl = glass_train[, 10], k = 9)

confusion_train <- table(x = glass_train[, 10], y = glass_train_pred)
confusion_train

Accuracy_train <- sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train

## Improving model performance ----

# use the scale() function to z-score standardize a data frame
glass_z <- as.data.frame(scale(glass[-10]))
glass_z <- cbind(glass_z, glass$Type)

indatapartition <- createDataPartition(glass_z$`glass$Type`, p=.80, list = FALSE)
glass_train_z <- glass_z[indatapartition,]
glass_test_z <- glass_z[-indatapartition,]

# re-classify test cases
glass_test_predz <- knn(train = glass_train_z[,-10], test = glass_test_z[,-10],
                      cl = glass_train_z[, 10], k = 9)

confusion_testz <- table(x = glass_test_z[, 10], y = glass_test_predz)
confusion_testz

Accuracy_z <- sum(diag(confusion_testz))/sum(confusion_testz)
Accuracy_z # 85%
# we see a significant increase in the accuracy while we have scaled data than normalised

# check with different k values
# k = 3
glass_test_predz <- knn(train = glass_train_z[,-10], test = glass_test_z[,-10],
                        cl = glass_train_z[, 10], k = 3)

confusion_testz <- table(x = glass_test_z[, 10], y = glass_test_predz)
confusion_testz

Accuracy_z <- sum(diag(confusion_testz))/sum(confusion_testz)
Accuracy_z # 80%

# k = 7
glass_test_predz <- knn(train = glass_train_z[,-10], test = glass_test_z[,-10],
                        cl = glass_train_z[, 10], k = 7)

confusion_testz <- table(x = glass_test_z[, 10], y = glass_test_predz)
confusion_testz

Accuracy_z <- sum(diag(confusion_testz))/sum(confusion_testz)
Accuracy_z # 85%

########################################################
#Lets Construct a For Loop and Build Multiple Models for Different K values for Normalised data

accuracy <- c()
for (i in 1:33) #This will Take k values from 1 to 33
{
  class1 <- knn(train = glass_train[,-10], test = glass_test[,-10], cl = glass_train[,10], k=i)
  CrossTable(glass_test$`glass$Type`, class1, prop.r = F, prop.c = F,prop.chisq = F)
  tab1 <- table(glass_test$`glass$Type`, class1)
  accuracy <- c(accuracy, round(sum(diag(tab1))/sum(tab1)*100, digits = 2))
}

summary(accuracy)
boxplot(accuracy)

AccuracyTable <- data.frame("K.value" = 1:33, "Accuracy" = accuracy)
attach(AccuracyTable)

ggplot(AccuracyTable, mapping = aes(K.value, Accuracy)) + geom_line(linetype = "dashed") + geom_point() + ggtitle("Model Accuracy for Different K-Value")
# This shows the K value 1 gives the best accuracy
########################################################
#Lets Construct a For Loop and Build Multiple Models for Different K values for Scaled data

accuracy <- c()
for (i in 1:33) #This will Take k values from 1 to 33
{
  class1 <- knn(train = glass_train_z[,-10], test = glass_test_z[,-10], cl = glass_train_z[,10], k=i)
  CrossTable(glass_test_z$`glass$Type`, class1, prop.r = F, prop.c = F,prop.chisq = F)
  tab1 <- table(glass_test_z$`glass$Type`, class1)
  accuracy <- c(accuracy, round(sum(diag(tab1))/sum(tab1)*100, digits = 2))
}

summary(accuracy)
boxplot(accuracy)

AccuracyTable <- data.frame("K.value" = 1:33, "Accuracy" = accuracy)
attach(AccuracyTable)

ggplot(AccuracyTable, mapping = aes(K.value, Accuracy)) + geom_line(linetype = "dashed") + geom_point() + ggtitle("Model Accuracy for Different K-Value")
# This shows the K value 3 gives the best accuracy