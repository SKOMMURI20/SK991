#Author Sathish Kommuri, Couse 3 & Task 3 (R)
#install the whole tidyverse:
install.packages("tidyverse")
library(caret)
library(data.table)
library(e1071)

#Read Data file 1 existing, file 2 new products
data1 <- read_csv("existingproductattributes2017.csv")
data2 <- read_csv("newproductattributes2017.csv")

# Dummify the data - Converts the Char columsn to Binary values.
newDataFrame1 <- dummyVars(" ~ .", data = data1)
readyData1 <- data.frame(predict(newDataFrame1, newdata = data1))
str(readyData1)
newDataFrame2 <- dummyVars(" ~ .", data = data2)
readyData2 <- data.frame(predict(newDataFrame2, newdata = data2))
str(readyData2)
complete.cases(readyData1)
complete.cases(readyData2)
which(complete.cases(readyData1))
which(!complete.cases(readyData1))
#na storage
nas <- which(!complete.cases(readyData1))
#remove NAs
data1final <- readyData1[-nas,]
data1final
summary(data1final)

#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample of the data
data2final <- data1final[sample(1:nrow(data1final), replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining3 <- createDataPartition(data2final$Price, p = .75, list = FALSE)
training3 <- data2final[inTraining3,]
testing3 <- data2final[-inTraining3,]

# SVM Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
svm2 <- train(Price ~., data = data2final, method = "svmLinear", trControl = train_control,  preProcess = c("center","scale"))
#View the model
svm2
plot(svm2)
