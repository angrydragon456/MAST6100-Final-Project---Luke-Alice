# ============================================
# Model 1: Linear Discriminant Analysis (LDA)
# ============================================

# Fit LDA model using all predictors
lda_fit <- lda(Churn ~ ., data = telco_train)

# Inspect model: prior probabilities, group means, coefficients
lda_fit

# Predict on test data
lda_pred <- predict(lda_fit, newdata = telco_test)

# Predicted classes
lda_class <- lda_pred$class

# Confusion matrix
lda_cm <- table(Predicted = lda_class, Actual = telco_test$Churn)
lda_cm

# Accuracy
lda_accuracy <- mean(lda_class == telco_test$Churn)
lda_accuracy

# Sensitivity and specificity
lda_sensitivity <- get_sensitivity(lda_cm)
lda_specificity <- get_specificity(lda_cm)

lda_sensitivity
lda_specificity


lda_fit$prior
