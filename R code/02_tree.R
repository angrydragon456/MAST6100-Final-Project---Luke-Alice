# ============================================
# Model 2: Decision Tree (CART)
# ============================================

library(rpart)

# Fit a classification tree using all predictors
tree_fit <- rpart(
  Churn ~ .,
  data   = telco_train,
  method = "class"
)

# Check the summary
summary(tree_fit)

# Display the complexity parameter table
printcp(tree_fit)

# Plot cross-validated error vs tree size
plotcp(tree_fit)

# Choose cp with minimum cross-validated error
best_cp <- tree_fit$cptable[which.min(tree_fit$cptable[,"xerror"]), "CP"]

# Prune the tree
tree_pruned <- prune(tree_fit, cp = best_cp)
tree_pruned

# Plot the (possibly pruned) tree
plot(tree_pruned)
text(tree_pruned, pretty = 0)

final_tree <- tree_pruned
# Predict class labels on test data
tree_class <- predict(final_tree, newdata = telco_test, type = "class")

# Confusion matrix
tree_cm <- table(Predicted = tree_class, Actual = telco_test$Churn)
tree_cm

# Accuracy
tree_accuracy <- mean(tree_class == telco_test$Churn)
tree_accuracy

# Run again for the unpruned tree for comparison
final_tree <- tree_fit
# Predict class labels on test data
tree_class <- predict(final_tree, newdata = telco_test, type = "class")

# Confusion matrix
tree_cm <- table(Predicted = tree_class, Actual = telco_test$Churn)
tree_cm

# Accuracy
tree_accuracy <- mean(tree_class == telco_test$Churn)
tree_accuracy

# Sensitivity and specificity
tree_sensitivity <- get_sensitivity(tree_cm)
tree_specificity <- get_specificity(tree_cm)

tree_sensitivity
tree_specificity
