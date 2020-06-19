#Author Sathish Kommuri, Couse 3 & Task 3 (R)
#install the whole tidyverse:
library(caret)
library(data.table)
library(corrplot)
library(ggplot2)
library(ggpubr)
#Read Data files 
data1 <- fread("existingproductattributes2017.csv")
data2 <- fread("newproductattributes2017.csv")

# Dummify the data - Converts the Char columsn to Binary values.
newDataFrame1 <- dummyVars(" ~ .", data = data1)
readyData1 <- data.frame(predict(newDataFrame1, newdata = data1))
str(readyData1)
newDataFrame2 <- dummyVars(" ~ .", data = data2)
readyData2 <- data.frame(predict(newDataFrame2, newdata = data2))
str(readyData2)
which(!complete.cases(readyData1))
which(!complete.cases(readyData2))
#na storage
nas <- which(!complete.cases(readyData1))
#remove NAs
data1final <- readyData1[-nas,]
data1final
summary(data1final)

#set seed
set.seed(998)

#create a 20% sample of the data
data2final <- data1final[sample(1:nrow(data1final), replace=FALSE),]

# define an 75%/25% train/test split of the dataset
partition <- createDataPartition(data1final$Volume, p = .75, list = FALSE)
training <- data1final[partition,]
testing <- data1final[-partition,]

# SVM Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
svm <- train(Volume ~., data = data1final, method = "svmLinear", trControl = train_control,tuneGrid = expand.grid(C = seq(0, 2, length = 10)))
rf <- train(Volume ~., data = data1final, method = "rf", trControl = train_control)
gb <- train(Volume ~., data = data1final, method = "gbm", trControl = train_control)
#View the model
svm
rf
gb
#to view the relative importance of features
summary(gb)

# Make predictions on the test data
svm$bestTune
predicted_svm <- predict(svm, testing)
final_svm <- cbind(testing,predicted_svm)
plot(svm)

predicted_rf <- predict(rf, testing)
final_rf <- cbind(testing,predicted_rf)
plot(rf)

predicted_gb <- predict(gb, testing)
final_gb <- cbind(testing,predicted_gb)
plot(gb)

#plotting Actual vs predicted values of testing 
#SVM
svm_compare_plot <-ggplot()+geom_line(aes(x = as.numeric(row.names(final_svm)), y = Volume), data = final_svm, color = "blue")+
geom_line(aes(x = as.numeric(row.names(final_svm)), y = predicted_svm), data = final_svm, color = "red") + xlab("index")

#RF
rf_compare_plot <- ggplot()+geom_line(aes(x = as.numeric(row.names(final_rf)), y = Volume), data = final_rf, color = "blue")+
  geom_line(aes(x = as.numeric(row.names(final_rf)), y = predicted_rf), data = final_rf, color = "red") + xlab("index")

#GB
gb_compare_plot <- ggplot()+geom_line(aes(x = as.numeric(row.names(final_gb)), y = Volume), data = final_gb, color = "blue")+
  geom_line(aes(x = as.numeric(row.names(final_gb)), y = predicted_gb), data = final_gb, color = "red") + xlab("index")

figure <- ggarrange(svm_compare_plot, rf_compare_plot, gb_compare_plot,
                    labels = c("SVM", "RF", "GB"),
                    ncol = 1, nrow = 3)
figure

# Prediction on the newdata with the best prediction data model
data2$Volume <- predict(rf, readyData2)

write.csv(data2, "T3output.csv", row.names = TRUE)




