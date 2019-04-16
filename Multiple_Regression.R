###############################################################################

# Packages:
pacman::p_load(readr, caret, corrplot)

###############################################################################

# Datasets:
existing_products <- read.csv("datasets/existingproductattributes2017.csv")
new_products <- read.csv("datasets/newproductattributes2017.csv")

###############################################################################

# Dummify the data:
newDF <- dummyVars("~.", data = existing_products)
readyData <- data.frame(predict(newDF, newdata = existing_products))
str(readyData) #checking if there are any nominal values

###############################################################################

# Analysing/deleting missing values:
summary(existing_products)
existing_products$BestSellersRank <- NULL #deleting attribute column that has missing values

###############################################################################

# Correlation analysis:
corrData <- cor(readyData)
corrData

###############################################################################

# Heat Map:
corrplot(corrData) # We can see that 2/3/4/5 star reviews and positive service reviews have a strong correlation with Volume.

###############################################################################

# Removing unnecessery attributes:
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

###############################################################################

# RMSE calculation:
RSS <- c(crossprod(linear_model$residuals)) #Residual sum of squares
MSE <- RSS / length(linear_model$residuals) #Mean squared error
RMSE <- sqrt(MSE) #Root MSE
RMSE

###############################################################################











