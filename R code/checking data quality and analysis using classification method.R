############################################
# Aneta Badzova AB2421                     #
# MAST5956 - Big Data and Machine Learning #
# Group coding                             #
# checking the quality of the data         #
############################################

#############################################################################################################################
#The Data set provided has 16800 observations of 21 variables, Of these variables 16 of them are numeric while,             #
#the rest are character based. As this is a large data set it is essential that the quality of the data is checked,         # 
#to ensure the accuracy, reliability, and usability of the data for analysis and                                            #
#decision-making. By assessing data quality, organizations can identify and address issues such as missing values,          #
#errors, inconsistencies, and outliners that may compromise the integrity of analyses and conclusions. Poor data quality    # 
#can lead to incorrect insights, flawed predictions, and unreliable recommendations, which can have significant consequences#
#for businesses, research endeavors, and policy-making. Therefore, conducting thorough data quality checks is essential to  #
#mitigate risks, enhance data-driven decision-making, and foster trust in the data and the insights derived from it.        #
#############################################################################################################################


#Here we import the data set provided ensuring that it is csv. I have renamed the 
#data set file name from - "IntOrg_NCD_variables_2024_02_02.csv" to "code" as this
# will make it easier during the process.

library(readr)
code <- read_csv("IntOrg_NCD_variables_2024_02_02.csv")
View(code)

#These are some of the packages we will be using that allow us to use certain functions 

install.packages("devtools")
devtools::install_github("r-lib/conflicted")
install.packages("tidyverse")
library(tidyverse)
install.packages("skimr")
library (skimr)
install.packages("tidyr")
library (tidyr)
install.packages("dplyr")
library(dplyr)

summary (code) # This gives us a summary of the data provided and also indicated if the variable is numeric or character 
str (code) # This prints the summary of the structure allowing us to look at the first few values of each variable, this helps for getting a quick overview of the data and understanding its content.

start.year <- 1975   # Set start year for analysis
end.year <- 2016   # Set end year for analysis

## Data quality check -checking column names, is a way of checking for errors and to identify and missing of unexpected variables

required_columns <- c("Country", "ISO","Sex", "Region", "Superregion", 
                      "Year", "Mean_BMI_children", "Prevalence_obesity_children", 
                      "Prevalence_overweight_children", "Prevalence_underweight_children", 
                      "Mean_BMI_adults", "Prevalence_obesity_adults", 
                      "Prevalence_underweight_adults", "Prevalence_morbid_obesity_adults", 
                      "Diabetes_prevalence", "Systolic_blood_pressure", 
                      "Prevalence_raised_blood_pressure", "Years_of_education", 
                      "Urbanisation", "Western_diet_score", "GDP_USD")
# Using the code below brings up all of the variables, with this you can interpert which ones may be missing 
print(names(code))

#This helps to identify if any of the columns are missing 
missing_columns <- setdiff(required_columns, names(code))

if (length(missing_columns) > 0) {
  cat("The following required columns are missing:\n")
  cat(paste(missing_columns, collapse = ", "), "\n")
} else {
  cat("All required columns are present.\n")
}

#Code below provides you with all necessary information of the data 
code %>% skim ()

#By using the code below it shows us How many columns there are, and the variable types. chr stands for character  
glimpse (code)
# these codes tell us what can be found in that particular variable
class (code$Sex) # This tell's us it is a character variable
unique (code$Sex) # This shows us what is within  the variable, in this case because we are looking at the variable Sex, we have males and females within

#Selecting variables 
names (code) # Brings up all 21 variables 

code %>% 
  select (Country, ISO, Year, Sex, ends_with("children"),
          ends_with("adults"), ends_with ("pressure"), 
          Region, Superregion, Urbanisation, Years_of_education,
          Western_diet_score, GDP_USD, Diabetes_prevalence)
# By using "ends_with()" we can select all variables that end in the same word which speeds up the process especially with big data sets.

#Filter observations - when looking at the variable countries, this code brings up a list of all  the 
#countries used in  this data set. Through this code we can identify that there are 200 countries. 
# We do this to all 5 character variables 
unique (code$Country)# 200 countries 
unique (code$ISO)# 200 
unique (code$Sex) # we are looking at both males and females 
unique (code$Region) # There are 22 regions
unique (code$Superregion) # There are 10 super regions 

#Missing Data - we do this for the numeric values to identify if there is any missing data 
mean (code$Mean_BMI_children)
mean (code$Prevalence_obesity_children)
mean (code$Prevalence_overweight_children)
mean (code$Prevalence_underweight_children)
mean (code$Mean_BMI_adults)
mean (code$Prevalence_obesity_adults)
mean (code$Prevalence_underweight_adults)
mean (code$Prevalence_morbid_obesity_adults)

#The codes above all returned with the mean of the variable, however, the ones below did not
# as they had missing values. This needs to be fixed to ensure the quality of the date.

mean (code$Prevalence_raised_blood_pressure)
mean (code$Diabetes_prevalence)
mean (code$Systolic_blood_pressure)
mean (code$Years_of_education)
mean (code$Urbanisation)
mean (code$Western_diet_score)
mean(code$GDP_USD)

# To remove the missing values we do the following:  

mean (code$Prevalence_raised_blood_pressure, na.rm = TRUE)
mean(code$Diabetes_prevalence,  na.rm = TRUE)
mean (code$Systolic_blood_pressure, na.rm = TRUE)
mean (code$Years_of_education, na.rm = TRUE)
mean (code$Urbanisation, na.rm = TRUE)
mean (code$Western_diet_score, na.rm = TRUE)
mean (code$GDP_USD, na.rm = TRUE)


# However, to analyse what data is missing, the following code has to be applied:
#incomplete case
code %>% 
  select (Prevalence_raised_blood_pressure, Diabetes_prevalence, Systolic_blood_pressure,
          Years_of_education, Urbanisation, Western_diet_score, GDP_USD) %>%
filter (!complete.cases(.)) %>% 
  drop_na_(Diabetes_prevalence, Western_diet_score) 


#This fills the incomplete cases with "none"

library (tidyr) # allows us to use the function
library(dplyr)  # allows us to use the function 

 code <- code %>% 
    mutate(Diabetes_prevalence = replace_na(Diabetes_prevalence, "none"))
  
#In doing so, this gives us an observation of the variables that are missing data.A possibility
# is that the data is missing as some individuals did not have raised blood pressure, or diabetes
# however, if this were the case we would expect to see 0 rather than N/A therefore we replaced the missing 
# value with "none" to ensure that we maintain data completeness, avoid bias, and improve model performance 

 library(magrittr) # so that we can use the  %>% 
 
#recoding variables - Changing male and female to numeric values will simplify the analysis stage
 code %>% select (Sex)
 code %>%
   select(Sex) %>% 
   mutate (Sex= recode(Sex,
                       "Male" = 1,
                       "Female" =2))
 
# Overall,this is a comprehensive data quality check examining column names, missing values, variable characteristics,and handling missing data appropriately. 
#By doing so, this also makes some data transformations to prepare the data set for further analysis. This helps lay the foundation for subsequent analysis and interpretation, and
#ensure that the data is accurate, reliable, and suitable for making informed decisions and drawing meaningful conclusions.

 
 ## Data analysis using classification method
 
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