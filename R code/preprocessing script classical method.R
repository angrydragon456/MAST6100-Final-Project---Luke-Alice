library(dplyr)
library(tidyr)
library(randomForest)
library(caret)
library(ggplot2)

# Load the dataset
data <- read.csv('IntOrg_NCD_variables_2024_02_02.csv')

data$Prevalence_obesity_adults# Data Preprocessing

# Identifying numeric and categorical columns
numeric_cols <- sapply(data, is.numeric)
categorical_cols <- c('Country', 'ISO', 'Sex', 'Region', 'Superregion')

# Fill NA in numeric columns with median
data[numeric_cols] <- lapply(data[numeric_cols], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Fill NA in categorical columns with the most frequent value (Mode)
for (col in categorical_cols) {
  mode_value <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
  data[[col]][is.na(data[[col]])] <- mode_value
}

# Check again for any remaining NA values in the dataset
sum(is.na(data))
# Encoding categorical variables
data[categorical_cols] <- lapply(data[categorical_cols], factor)

# Creating the target variable - categorizing 'Prevalence_obesity_adults' into 'Low', 'Medium', 'High'
data$Obesity_Category <- cut(data$Prevalence_obesity_adults, breaks = c(0, 0.1, 0.2, Inf), labels = c('Low', 'Medium', 'High'))


# Dropping the original 'Prevalence_obesity_adults' as it's now converted into categories
data$Prevalence_obesity_adults <- NULL

# Splitting the data into features (X) and target variable (y)
y <- data$Obesity_Category
X <- data %>% select(-Obesity_Category)

# Splitting the dataset into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(y, p = .7, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

dummies_model <- dummyVars(" ~ .", data = X_train)
X_train_transformed <- predict(dummies_model, newdata = X_train)
X_test_transformed <- predict(dummies_model, newdata = X_test)

# Model Selection and Training with transformed data
rf_classifier <- randomForest(x = X_train_transformed, y = y_train, ntree = 500)

# Predicting the test set results
y_pred <- predict(rf_classifier, newdata = X_test_transformed)

# Evaluating the model using the confusion matrix
confusion_matrix <- confusionMatrix(y_pred, y_test)

# Displaying the classification report
print(confusion_matrix)

# Displaying detailed accuracy report
accuracy_report <- confusion_matrix$overall
print(accuracy_report)
