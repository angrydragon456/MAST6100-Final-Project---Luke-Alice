# ============================================
# Model 4: Neural Network (MLP) with nnet
# ============================================

library(nnet)

# Remove Churn_num so it is not used as a predictor
telco_train$Churn_num <- NULL
telco_test$Churn_num  <- NULL

# Encode predictors using model.matrix again (now without Churn_num)
x_train <- model.matrix(Churn ~ . - 1, data = telco_train)
x_test  <- model.matrix(Churn ~ . - 1, data = telco_test)


train_nn <- as.data.frame(x_train)
train_nn$Churn <- telco_train$Churn

test_nn <- as.data.frame(x_test)
test_nn$Churn <- telco_test$Churn

set.seed(123)

nn_fit <- nnet(
  Churn ~ .,
  data  = train_nn,
  size  = 5,        # number of hidden units
  decay = 5e-4,     # weight decay (regularisation)
  maxit = 200,      # maximum iterations
  trace = TRUE      # print training progress
)

# Class predictions on test data
nn_class <- predict(nn_fit, newdata = test_nn, type = "class")

# Confusion matrix
nn_cm <- table(Predicted = nn_class, Actual = test_nn$Churn)
nn_cm

# Accuracy
nn_accuracy <- mean(nn_class == test_nn$Churn)
nn_accuracy

# Sensitivity and specificity
nn_sensitivity <- get_sensitivity(nn_cm)
nn_specificity <- get_specificity(nn_cm)

nn_sensitivity
nn_specificity


nn_prob <- predict(nn_fit, newdata = test_nn, type = "raw")  # matrix of probs
# For binary classes, second column is usually "Yes"
head(nn_prob)




