install.packages(c("tidyverse", "caret", "randomForest", "e1071", "smotefamily"))


library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(smotefamily)

telco <- read.csv(file.choose(), stringsAsFactors = FALSE)
str(telco)


# Convert blank strings in TotalCharges to NA
telco$TotalCharges[telco$TotalCharges == " "] <- NA

# Convert to numeric
telco$TotalCharges <- as.numeric(telco$TotalCharges)

# How many NAs?
sum(is.na(telco$TotalCharges))

# Drop rows with missing TotalCharges
telco <- telco |> tidyr::drop_na(TotalCharges)



# Target as factor
telco$Churn <- factor(telco$Churn, levels = c("No", "Yes"))

# SeniorCitizen as factor
telco$SeniorCitizen <- factor(telco$SeniorCitizen, levels = c(0, 1))

# Convert all character predictors (except customerID) to factors
char_cols <- sapply(telco, is.character)
char_cols["customerID"] <- FALSE
telco[, char_cols] <- lapply(telco[, char_cols], factor)

# Drop ID column
telco$customerID <- NULL

str(telco)




set.seed(123)

train_index <- caret::createDataPartition(telco$Churn, p = 0.7, list = FALSE)
train <- telco[train_index, ]
test  <- telco[-train_index, ]

prop.table(table(train$Churn))
prop.table(table(test$Churn))



# ---- GLM: Logistic regression ----
glm_fit <- glm(Churn ~ ., data = train, family = binomial)

summary(glm_fit)   # good for methodology section

# Predictions on test set
glm_prob <- predict(glm_fit, newdata = test, type = "response")
glm_pred <- ifelse(glm_prob > 0.5, "Yes", "No")
glm_pred <- factor(glm_pred, levels = c("No", "Yes"))

# Confusion matrix
glm_cm <- caret::confusionMatrix(glm_pred, test$Churn, positive = "Yes")
glm_cm



# ---- Random Forest ----
set.seed(123)
rf_fit <- randomForest::randomForest(
  Churn ~ ., data = train,
  ntree = 500,   # number of trees
  mtry  = 5,     # number of variables tried at each split
  importance = TRUE
)

rf_fit   # brief summary

rf_pred <- predict(rf_fit, newdata = test)
rf_cm <- caret::confusionMatrix(rf_pred, test$Churn, positive = "Yes")
rf_cm

# Variable importance plot – nice for slides
randomForest::varImpPlot(rf_fit)



# ---- SVM with radial kernel ----
set.seed(123)
svm_fit <- e1071::svm(
  Churn ~ ., data = train,
  kernel = "radial",
  cost   = 1,
  gamma  = 0.1
)

svm_pred <- predict(svm_fit, newdata = test)
svm_cm <- caret::confusionMatrix(svm_pred, test$Churn, positive = "Yes")
svm_cm




# ---- kNN ----
set.seed(123)
ctrl <- caret::trainControl(method = "cv", number = 5)

knn_fit <- caret::train(
  Churn ~ ., data = train,
  method = "knn",
  trControl = ctrl,
  tuneLength = 10   # try several k values
)

knn_fit   # shows best k

knn_pred <- predict(knn_fit, newdata = test)
knn_cm <- caret::confusionMatrix(knn_pred, test$Churn, positive = "Yes")
knn_cm




results_round <- results
results_round[ , -1] <- round(results_round[ , -1], 3)  # round all columns except Model
results_round


