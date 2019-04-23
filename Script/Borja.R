###############################################################################

# Packages:
pacman::p_load(rstudioapi, readr, caret, corrplot, e1071, dplyr, car, GGally,
               Quandl, xts)

# Load data ---------------------------

# Datasets:
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

existing_products <- read.csv("datasets/existingproductattributes2017.csv")
new_products <- read.csv("datasets/newproductattributes2017.csv")

#### Pre-processing the Data ##################################################

# Dummify the data:
newDF <- dummyVars("~.", data = existing_products)
readyData <- data.frame(predict(newDF, newdata = existing_products))
str(readyData) #checking if there are any nominal values
rm(newDF) # Removing "newDF", as it won't be used from now on.

# Analysing/deleting missing values
summary(readyData)
readyData$BestSellersRank <- NULL #deleting attribute column that has missing values

# Correlation analysis:
ggcorr(readyData)
cor(readyData) # We can see that 2/3/4 star reviews and positive service reviews have a strong correlation with Volume.

# Removing unnecessery attributes:
readyData$x5StarReviews <- NULL # Mandatory
readyData$Price <- NULL # Mandatory
readyData$ProfitMargin <- NULL
readyData$ProductHeight <- NULL
readyData$ProductWidth <- NULL
readyData$ProductDepth <- NULL
readyData$ShippingWeight <- NULL
readyData$Recommendproduct <- NULL
readyData$NegativeServiceReview <- NULL
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

 # Removing repeated lines and Volume outliers:

readyData <- distinct(readyData)
readyData <- readyData[readyData$Volume<4000,]

# Creating Train and Test sets --------
set.seed(123)

trainSize <- round(nrow(readyData)*0.75)
trainSize

testSize <- nrow(readyData) - trainSize
testSize

training_indices <- sample(seq_len(nrow(readyData)), size = trainSize)

trainSet <- readyData[training_indices,]
testSet <- readyData[-training_indices,]

# #Feature Engeneering ----
linear_model <- lm(Volume~ 0 + x4StarReviews + x3StarReviews + x2StarReviews +
                     x1StarReviews, trainSet)
summary(linear_model)

trainSet$WeightedReviews <- 2.203*trainSet$x4StarReviews +
  0.296*trainSet$x3StarReviews + 0.036*trainSet$x2StarReviews + 
  0.434*trainSet$x1StarReviews
trainSet$x1StarReviews <- NULL
trainSet$x2StarReviews <- NULL
trainSet$x3StarReviews <- NULL
trainSet$x4StarReviews <- NULL

testSet$WeightedReviews <- 2.203*testSet$x4StarReviews +
  0.296*testSet$x3StarReviews + 0.036*testSet$x2StarReviews + 
  0.434*testSet$x1StarReviews
testSet$x1StarReviews <- NULL
testSet$x2StarReviews <- NULL
testSet$x3StarReviews <- NULL
testSet$x4StarReviews <- NULL

# Modeling --------------------

# Multiple models and metrics:

models <- c("lm", "svmLinear", "rf", "knn")

comb_metric <- c()

for (i in models){
  fit <- train(Volume~., data = trainSet, method = i)
  pred <- predict(fit, testSet)
  metric <- postResample(pred, testSet$Volume)
  comb_metric <- cbind(comb_metric, metric)
}

colnames(comb_metric) <- c(models)
# comb_metric <- as.data.frame(comb_metric)
names(comb_metric)
melted_data <- reshape::melt(comb_metric)
melted_data

colnames(melted_data) <- c("Metrics", "Models", "Value")

ggplot(data = melted_data, aes(x = Models, y = Value)) + 
  geom_col(color="Blue", fill="lightblue") + 
  facet_grid(Metrics~., scales = "free")
 #We'll select the Random Forest, as it seems to be the one with best performance.

# Creating the best model to the whole dataset ----

# Feature Engineering in order to apply the model:

readyData$WeightedReviews <- 2.203*readyData$x4StarReviews +
  0.296*readyData$x3StarReviews + 0.036*readyData$x2StarReviews + 
  0.434*readyData$x1StarReviews
readyData$x1StarReviews <- NULL
readyData$x2StarReviews <- NULL
readyData$x3StarReviews <- NULL
readyData$x4StarReviews <- NULL

# PREDICTIONS FOR NEW PRODUCT DATASET (USING THE BEST MODEL):

set.seed(123)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
rf_model <- train(Volume~., data = readyData, method = "rf",
                  trControl = trctrl, preProcess = c("center", "scale"),
                  tunelength = 15)

readyNewData <- new_products

 # Feature Engineering in order to apply the model:
readyNewData$WeightedReviews <- 2.203*readyNewData$x4StarReviews +
  0.296*readyNewData$x3StarReviews + 0.036*readyNewData$x2StarReviews + 
  0.434*readyNewData$x1StarReviews

 # Applying the model:

new_product_predictions <- predict(rf_model, newdata = readyNewData)
new_product_predictions

readyNewData$VolumePredictions <- round(new_product_predictions, digits = 0) #storing predictions
readyNewData$Profitability <- round((readyNewData$ProfitMargin*
                                       readyNewData$Price)*
                                      readyNewData$VolumePredictions, 
                                    digits = 2)

# Removing unnecessery attributes from New Products:
readyNewData$x5StarReviews <- NULL
readyNewData$x4StarReviews <- NULL
readyNewData$x3StarReviews <- NULL
readyNewData$x2StarReviews <- NULL
readyNewData$x1StarReviews <- NULL
readyNewData$x1StarReviews <- NULL
readyNewData$ProductHeight <- NULL
readyNewData$ProductWidth <- NULL
readyNewData$ProductDepth <- NULL
readyNewData$ShippingWeight <- NULL
readyNewData$Recommendproduct <- NULL
readyNewData$NegativeServiceReview <- NULL
readyNewData$ProfitMargin <- NULL
readyNewData$BestSellersRank <- NULL
readyNewData$Volume <- NULL

output1 <- readyNewData %>% group_by(ProductType) %>% 
  summarise(sum_volume = sum(VolumePredictions))

output2 <- readyNewData %>% group_by(ProductType) %>% 
  summarise(sum_Profitability = sum(Profitability))

output <- merge(output1, output2, sort = TRUE)

# readyNewData <- readyNewData[order(-readyNewData$P) , ] # Optional
# top5 <- head(readyNewData, 5) # Optional

#CREATING A CSV FILE THAT INCUDES FINAL PREDICTIONS AND STORING IT ON THE HARD DRIVE:
write.csv(output, file = "C2.T3output.csv", row.names = TRUE)
# write.csv(top5, file = "C2.T3Top5.csv", row.names = TRUE)