
library(caret)
library(dplyr)
library(readr)
library(tidyr)

#loading training data
training <- read.csv("trainingData.csv")

# Removing zero variance variables
remove_zerovar_training <- training[ -which(apply(training, 2, var) == 0 )] 
# checking if we have any zero variance columns
which(apply(remove_zerovar_training, 2, var) == 0)


# take off extra dependent variables 
remove_zerovar_training <- remove_zerovar_training[, -c(466,467,472:474)]
# check if they are all removed
names(remove_zerovar_training)[465:469]


# combine dependent variables into one  
remove_zerovar_training <- unite(remove_zerovar_training, col = "LOCATION", c("BUILDINGID",
                                                        "FLOOR", "SPACEID", "RELATIVEPOSITION"), sep = "", remove = FALSE)
# convert data type to factor  
remove_zerovar_training$LOCATION <- as.factor(remove_zerovar_training$LOCATION)
# make sure the data type has been converted
str(remove_zerovar_training$LOCATION)

# break the dataset into three datasets by buildings
training_building0 <- subset(remove_zerovar_training, BUILDINGID == 0)
training_building1 <- subset(remove_zerovar_training, BUILDINGID == 1)
training_building2 <- subset(remove_zerovar_training, BUILDINGID == 2)
# remove individual location variables
training_building0[, c(467:470)] <- NULL
training_building1[, c(467:470)] <- NULL
training_building2[, c(467:470)] <- NULL
# applying factor() to avoid extra levels
training_building0$LOCATION <- factor(training_building0$LOCATION)
training_building1$LOCATION <- factor(training_building1$LOCATION)
training_building2$LOCATION <- factor(training_building2$LOCATION)
# check how many levels of LOCATION for building0
str(training_building0$LOCATION)


set.seed(220)
# create 10-fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

# split training and testing datasets
Partitioning_building0 <- createDataPartition(training_building0$LOCATION, p = .75, list = FALSE )
training_building0_data <- training_building0[Partitioning_building0, ]
testing_building0_data <- training_building0[-Partitioning_building0, ]


#------------- for building 0 --------------------- training for 3 different models C5.0, RF, KNN
# C5.0 model
C50_building0 <- train(LOCATION~., data = training_building0_data, method = "C5.0", 
                trControl = fitControl)
# testing 
prediction_C50_building0 <- predict(C50_building0, testing_building0_data)

# random forest
rf_building0 <- train(LOCATION~., data = training_building0_data, method = "rf",
               trControl = fitControl)
prediction_rf_building0 <- predict(rf_building0, testing_building0_data)

# KNN
KNN_building0 <- train(LOCATION~., data = training_building0_data, method = "knn",
                trControl = fitControl)
prediction_KNN_building0 <- predict(KNN_building0, testing_building0_data)

# ---------------- Apply same for building 1 and 2 as well ---------------

#----------- confusion matrix - Building 0 ----------------------- get accuracy and Kappa values---------
# evaluating C5.0 
cm_C50_b0 <- confusionMatrix(prediction_C50_building0, testing_building0_data$LOCATION)
postResample(prediction_C50_b0, testing_building0_data$LOCATION)

# evaluating Random Forest
cm_rf_b0 <- confusionMatrix(prediction_rf_building0, testing_building0_data$LOCATION)
postResample(prediction_rf_b0, testing_building0_data$LOCATION)

# evaluating KNN
cm_KNN_b0 <- confusionMatrix(prediction_KNN_building0, testing_building0_data$LOCATION)
postResample(prediction_KNN_b0, testing_building0_data$LOCATION)

# resampling for all three models 
resample_b0 <- resamples( list(C50 = C50_building0, RF = rf_building0, KNN = KNN_building0))
summary(resample_b0)

# ---------------- Apply same for building 1 and 2 as well ---------------

# -------------- Actual prediction on validation data -----------------

testing_dataset <- read.csv("validationData.csv")
remove_zerovar_testing <- testing_dataset[ -which(apply(testing_dataset, 2, var) == 0 )] 
remove_zerovar_testing <- remove_zerovar_testing[, -c(466,467,472:474)]
# check if they are all removed
names(remove_zerovar_testing)[465:469]

remove_zerovar_testing <- unite(remove_zerovar_testing, col = "LOCATION", c("BUILDINGID",
                                                                              "FLOOR", "SPACEID", "RELATIVEPOSITION"), sep = "", remove = FALSE)
# convert data type to factor  
remove_zerovar_training$LOCATION <- as.factor(remove_zerovar_testing$LOCATION)
# make sure the data type has been converted
str(remove_zerovar_testing$LOCATION)

# break the dataset into three datasets by buildings
validating_building0 <- subset(remove_zerovar_testing, BUILDINGID == 0)

# predict using the method with highest accuracy and kappa values ---- Model might change
prediction_C50_building0_validationData <- predict(C50_building0, validating_building0)



# ---------------- Apply same for building 1 and 2 as well ---------------
