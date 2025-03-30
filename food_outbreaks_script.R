# Project: Build an ML model in R to predict high-risk foods for foodborne illness 
# outbreaks using the CDC Foodborne Disease Outbreaks (1998-2015) dataset from Kaggle

# Install required packages (if not installed)
install.packages(c("tidyverse", "caret", "randomForest", "xgboost", "janitor", "ggplot2"))

# Load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(janitor)
library(ggplot2)

# Load the dataset
food_outbreaks <- read.csv("outbreaks.csv", stringsAsFactors = TRUE)

# View structure
str(food_outbreaks)

# Check missing values
colSums(is.na(food_outbreaks))

# Preview data
head(food_outbreaks)
View(food_outbreaks)

# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)

# Convert categorical variables to factors
food_outbreaks <- food_outbreaks %>%
  mutate(
    State = as.factor(State),
    Location = as.factor(Location),
    Species = as.factor(Species),
    Serotype_Genotype = as.factor(Serotype.Genotype),
    Status = as.factor(Status),
    Food = as.factor(Food)
  )

# Handle missing values: Replace blanks with NA
food_outbreaks[food_outbreaks == ""] <- NA

# Drop rows where Food is missing (since it's our main predictor)
food_outbreaks <- food_outbreaks %>% drop_na(Food)

# Fill missing values in categorical columns with "Unknown". convert to char then factor
food_outbreaks <- food_outbreaks %>%
  mutate(
    Location = as.factor(replace_na(as.character(Location), "Unknown")),
    Species = as.factor(replace_na(as.character(Species), "Unknown")),
    Serotype_Genotype = as.factor(replace_na(as.character(Serotype_Genotype), "Unknown")),
    Status = as.factor(replace_na(as.character(Status), "Unknown"))
  )

# Convert Status to binary (1 = Confirmed, 0 = Suspected)
food_outbreaks$Status <- ifelse(food_outbreaks$Status == "Confirmed", 1, 0)

# Create a target variable
# Define threshold for high-risk outbreaks (median number of illnesses)
illness_threshold <- median(food_outbreaks$Illnesses, na.rm = TRUE)

# Create High_Risk_Food label (1 = High Risk, 0 = Low Risk)
food_outbreaks <- food_outbreaks %>%
  mutate(High_Risk_Food = ifelse(Illnesses > illness_threshold, 1, 0)) %>%
  select(-Illnesses, -Hospitalizations, -Fatalities)  # Remove numeric severity columns

# One-Hot Encode Food Column. Since Food contains multiple values in one row, we split it into separate food categories.
# Create binary columns for each unique food type
# Load necessary libraries
library(tidyverse)
library(stringr)

# Replace semicolons (;) with commas (,) for consistency
food_outbreaks$Food <- str_replace_all(food_outbreaks$Food, ";", ",")

# Get unique food items
unique_foods <- unique(unlist(strsplit(as.character(food_outbreaks$Food), ", ")))

# Create binary columns for each unique food type
for (food in unique_foods) {
  food_column_name <- make.names(food)  # Ensure valid column names
  food_outbreaks[[food_column_name]] <- ifelse(str_detect(food_outbreaks$Food, fixed(food)), 1, 0)
}

# Drop the original Food column
food_outbreaks <- select(food_outbreaks, -Food)

# Split Data into Training & Testing Sets
set.seed(123)
trainIndex <- createDataPartition(food_outbreaks$High_Risk_Food, p = 0.8, list = FALSE)
train_set <- train_set %>%
  mutate(across(where(is.factor), ~ fct_explicit_na(., na_level = "Unknown")))
test_set  <- food_outbreaks[-trainIndex, ]

# Train a Random Forest model
set.seed(123)
rf_model <- randomForest(High_Risk_Food ~ ., data = train_set, ntree = 500, importance = TRUE)

# Print model summary
print(rf_model)

# Feature Importance
importance(rf_model)
varImpPlot(rf_model)





