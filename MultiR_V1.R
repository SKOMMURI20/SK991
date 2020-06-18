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


# SVM Model
library(e1071) 

smodel <- svm(ProductTypeLaptop ~ . , readyData1)
spred <- predict(smodel, readyData1)

points(readyData1$ProductTypePC, spred, col = "red", pch=4)
