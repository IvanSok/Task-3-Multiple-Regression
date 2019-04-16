###############################################################################

# Packages:
pacman::p_load(readr, caret, corrplot, e1071, randomForest, mlbench, rstudioapi)

###############################################################################

# Github setup:
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(current_path)


###############################################################################

# Datasets:
existing_products <- read.csv("datasets/existingproductattributes2017.csv")
new_products <- read.csv("datasets/newproductattributes2017.csv")

###############################################################################

# Dummify the data:

# For existing product attributes:
newDF <- dummyVars("~.", data = existing_products)
readyData <- data.frame(predict(newDF, newdata = existing_products))
str(readyData) #checking if there are any nominal values

# For new product attributes:
newnewDF <- dummyVars("~.", data = new_products)
newreadyData <- data.frame(predict(newnewDF, newdata = new_products))
str(newreadyData)

###############################################################################

# Analysing/deleting missing values:

# For existing product attributes:
summary(readyData)
readyData$BestSellersRank <- NULL #deleting attribute column that has missing values

# For new product attributes:
summary(newreadyData)

###############################################################################

# Correlation analysis:
corrData <- cor(readyData)
corrData

###############################################################################

# Heat Map:
corrplot(corrData) # We can see that 2/3/4/5 star reviews and positive service reviews have a strong correlation with Volume.

###############################################################################

# Removing unnecessery attributes:

# For existing product attributes:
readyData$ProfitMargin <- NULL
readyData$ProductHeight <- NULL
readyData$ProductWidth <- NULL
readyData$ProductDepth <- NULL
readyData$ShippingWeight <- NULL
readyData$Recommendproduct <- NULL
readyData$NegativeServiceReview <- NULL
readyData$x1StarReviews <- NULL
readyData$x5StarReviews <- NULL
readyData$Price <- NULL
readyData$ProductNum <- NULL
readyData$ProfitMargin <- NULL
readyData$ProductType.Tablet <- NULL
readyData$ProductType.Software <- NULL
readyData$ProductType.Smartphone <- NULL
readyData$ProductType.PrinterSupplies <- NULL
readyData$ProductType.Printer <- NULL
readyData$ProductType.PC <- NULL
readyData$ProductType.Netbook <- NULL
readyData$ProductType.Laptop <- NULL
readyData$ProductType.GameConsole <- NULL
readyData$ProductType.ExtendedWarranty <- NULL
readyData$ProductType.Display <- NULL
readyData$ProductType.Accessories <- NULL

# For new product attributes:
newreadyData$ProfitMargin <- NULL
newreadyData$ProductHeight <- NULL
newreadyData$ProductWidth <- NULL
newreadyData$ProductDepth <- NULL
newreadyData$ShippingWeight <- NULL
newreadyData$Recommendproduct <- NULL
newreadyData$NegativeServiceReview <- NULL
newreadyData$x1StarReviews <- NULL
newreadyData$x5StarReviews <- NULL
newreadyData$Price <- NULL
newreadyData$ProductNum <- NULL
newreadyData$ProfitMargin <- NULL
newreadyData$ProductType.Tablet <- NULL
newreadyData$ProductType.Software <- NULL
newreadyData$ProductType.Smartphone <- NULL
newreadyData$ProductType.PrinterSupplies <- NULL
newreadyData$ProductType.Printer <- NULL
newreadyData$ProductType.PC <- NULL
newreadyData$ProductType.Netbook <- NULL
newreadyData$ProductType.Laptop <- NULL
newreadyData$ProductType.GameConsole <- NULL
newreadyData$ProductType.ExtendedWarranty <- NULL
newreadyData$ProductType.Display <- NULL
newreadyData$ProductType.Accessories <- NULL

###############################################################################

# Linear Regression Model:
set.seed(123)

trainSize <- round(nrow(readyData)*0.7)
trainSize

testSize <- nrow(readyData) - trainSize
testSize

training_indices <- sample(seq_len(nrow(readyData)), size = trainSize)


trainSet <- readyData[training_indices,]
testSet <- readyData[-training_indices,]

linear_model <- lm(Volume~.,trainSet)
summary(linear_model) #Multiple R^2 = 0.9148, p-value: < 2.2e-16 (almost 0 = very significant) 

lm_predictions <- predict(linear_model, testSet)
lm_predictions

postResample(lm_predictions, testSet$Volume) #performance metrics

###############################################################################

# Support Vector Machine (SVM) model:
set.seed(123)

trainSize <- round(nrow(readyData)*0.7)
trainSize

testSize <- nrow(readyData) - trainSize
testSize

training_indices <- sample(seq_len(nrow(readyData)), size = trainSize)

trainSet <- readyData[training_indices,]
testSet <- readyData[-training_indices,]

svm_model <- svm(Volume~., data = trainSet)
svm_model

svm_predictions <- predict(svm_model, testSet)
svm_predictions

postResample(svm_predictions, testSet$Volume) # performance metrics

###############################################################################

# Random Forest:
set.seed(123)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
rf_model <- train(Volume~., data = trainSet, method = "rf",
                  trControl = trctrl, preProcess = c("center", "scale"),
                  tunelength = 15)
rf_model

rf_predictions <- predict(rf_model,testSet)
rf_predictions

postResample(svm_predictions, testSet$Volume) # performance metrics

###############################################################################

# k-NN model:
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knnFit <- train(Volume ~., data = trainSet, method = "knn",
                trControl = trctrl, preProcess = c("center", "scale"),
                tuneLength = 10)
knnFit 

knn_predictions <- predict(knnFit, newdata = testSet) #predicting on testing set
knn_predictions

postResample(knn_predictions, testSet$Volume) # performance metrics

###############################################################################

# Predictions for new product data set (using the best model):
new_product_predictions_knn <- predict(knnFit, newdata = newreadyData)
new_product_predictions_knn

finalPred = new_product_predictions_knn #storing predictions

###############################################################################

# Adding predictions to the new products data:
output <- newreadyData
output$predictions <- finalPred
output

###############################################################################

#Creating a csv file that includes final predictions and storing it on hard drive:
write.csv(output, file = "C2.T3output.csv", row.names = TRUE)

###############################################################################