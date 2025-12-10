# ============================================
# MAST6100 Final Project - Data Preprocessing
# Telco Customer Churn
# ============================================

# Load libraries
library(MASS)      # LDA
library(rpart)     # Decision trees
library(gbm)       # Boosting 
library(keras)     # Neural networks
library(ggplot2)   # Plots 

# Set working directory
setwd("C:/Users/ldpow/OneDrive - University of Kent/Machine Learning and Deep Learning 2025 Autumn/RSudio/MAST6100_FinalProject")


# Load dataset
telco <- read.csv("data/Telco-Customer-Churn_Kaggle.csv",
                  stringsAsFactors = FALSE)

# Check the structure
str(telco)
head(telco)
summary(telco)

# Clean variable TotalCharges
# Empty strings "" in TotalCharges should be treated as missing
telco$TotalCharges[telco$TotalCharges == ""] <- NA

# Convert to numeric
telco$TotalCharges <- as.numeric(telco$TotalCharges)

# Check missing values per column
colSums(is.na(telco))

# Remove rows with NA (very few)
telco <- na.omit(telco)

# ---- Remove customerID (not a predictor)
telco$customerID <- NULL

# Check structure after cleaning
str(telco)

# SeniorCitizen is coded as 0/1 but is not numerical
telco$SeniorCitizen <- factor(telco$SeniorCitizen)

# All character columns -> factors
char_cols <- sapply(telco, is.character)
telco[char_cols] <- lapply(telco[char_cols], factor)

# Make sure Churn is a factor with "No" as reference
telco$Churn <- relevel(telco$Churn, ref = "No")

#Check the data
str(telco$Churn)
table(telco$Churn)
prop.table(table(telco$Churn))


set.seed(123)  # for reproducibility

# 70/30 train/test split
n <- nrow(telco)
train_index <- sample(1:n, size = round(0.7 * n))
telco_train <- telco[train_index, ]
telco_test  <- telco[-train_index, ]

# Check Yes/No balance in train and test
prop.table(table(telco_train$Churn))
prop.table(table(telco_test$Churn))


get_sensitivity <- function(cm) {
  # TP / (TP + FN)
  TP <- cm["Yes", "Yes"]
  FN <- cm["No", "Yes"]
  TP / (TP + FN)
}

get_specificity <- function(cm) {
  # TN / (TN + FP)
  TN <- cm["No", "No"]
  FP <- cm["Yes", "No"]
  TN / (TN + FP)
}

prop.table(table(telco$Churn))




