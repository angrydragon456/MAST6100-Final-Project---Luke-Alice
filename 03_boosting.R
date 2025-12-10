# ============================================
# Model 3: Boosting (GBM)
# ============================================

library(gbm)

# Create a numeric version of the response for gbm
telco_train$Churn_num <- ifelse(telco_train$Churn == "Yes", 1, 0)
telco_test$Churn_num  <- ifelse(telco_test$Churn == "Yes", 1, 0)

set.seed(123)

boost_fit <- gbm(
  formula = Churn_num ~ . - Churn,    #Use numeric versions 
  data = telco_train,
  distribution = "bernoulli",   # For binary classification
  n.trees = 5000,               # Initial number of trees (we'll tune later)
  interaction.depth = 1,        # Stumps
  shrinkage = 0.01,             # Learning rate
  bag.fraction = 0.5,           # Stochastic gradient boosting
  train.fraction = 1.0,         # Use full training set
  n.cores = 1                   # Keeps results reproducible
)

boost_fit

best_iter <- gbm.perf(boost_fit, method = "OOB")
best_iter


# Predicted probabilities
boost_prob <- predict(boost_fit,
                      newdata = telco_test,
                      n.trees = best_iter,
                      type = "response")

# Convert to class labels
boost_class <- ifelse(boost_prob > 0.5, "Yes", "No")
boost_class <- factor(boost_class, levels = c("No", "Yes"))

# Confusion matrix
boost_cm <- table(Predicted = boost_class, Actual = telco_test$Churn)
boost_cm

# Accuracy
boost_accuracy <- mean(boost_class == telco_test$Churn)
boost_accuracy

# Sensitivity and specificity
boost_sensitivity <- get_sensitivity(boost_cm)
boost_specificity <- get_specificity(boost_cm)

boost_sensitivity
boost_specificity


summary(boost_fit, n.trees = best_iter)


