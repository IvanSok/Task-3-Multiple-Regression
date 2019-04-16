pacman::p_load(rstudioapi,readr, caret, mlbench, rpart, rpart.plot, party,
               randomForest, C50, inum, RColorBrewer, bindrcpp)

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

getwd()
list.files("dataset/") #Optional
existing_products <- read.csv("dataset/existingproductattributes2017.csv")
str(existing_products)
new_products <- read.csv("dataset/newproductattributes2017.csv")
str(new_products)

# Dummy Data
dmy <- dummyVars(" ~ .", data = existing_products)
readyData <- data.frame(predict(dmy, newdata = existing_products))
