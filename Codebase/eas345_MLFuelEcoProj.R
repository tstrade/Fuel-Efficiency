# Load necessary libraries
library(randomForest)

# Specify the path to Car Specification Dataset file
file_path <- "~/DataScience/FuelConsumption/CarSpecs1945to2020.csv"

# Read CSV file into a data frame
CarSpecs_45to20 <- read.csv(file_path)

# Select the specified columns 
selected_columns <- c(1, 2, 3, 5, 6, 9, 11, 12, 18, 
                      32, 33, 35, 36, 37, 38, 39, 
                      40, 43, 49, 51, 53, 55, 56, 64)

cleaned_CarSpecs <- CarSpecs_45to20[, selected_columns]

# Show the excluded columns
removed_columns <- c(4, 7, 10, 13, 14, 15, 16, 17, 19,
                     20, 21, 22, 23, 24, 25, 26, 27, 28,
                     29, 30, 31, 34, 41, 42, 44, 45, 46,
                     47, 48, 50, 52, 54, 57, 58, 59, 60,
                     61, 62, 63, 65, 66, 67, 68, 69, 70,
                     71, 72, 73, 74, 75, 76, 77, 78)

removed_CarSpecs <-CarSpecs_45to20[, removed_columns]
colnames(removed_CarSpecs)

# Remove rows with any missing data or "NA"
cleaned_CarSpecs <- cleaned_CarSpecs[complete.cases(cleaned_CarSpecs) & !apply(cleaned_CarSpecs, 1, function(x) any(x == "NA")), ]

# Remove rows with values less than 1996 in column 5
# to match the scope of other dataset
cleaned_CarSpecs <- cleaned_CarSpecs[cleaned_CarSpecs[, 5] >= 1996, ]

# View the cleaned dataset in a separate window
View(cleaned_CarSpecs)

# Select the relevant columns for the model (columns 6 to 22 and 24, with column 23 as the target)
model_carspec_data <- cleaned_CarSpecs[, c(6:22, 24, 23)]

# Rename columns for clarity (if needed)
colnames(model_carspec_data)[ncol(model_carspec_data)] <- "Fuel_Economy"  # Renaming column 23 as Fuel_Economy

# Split the data into training and testing sets (80/20 split)
set.seed(123)  # For reproducibility
train_carspec_indices <- sample(1:nrow(model_carspec_data), 0.8 * nrow(model_carspec_data))
train_carspec_data <- model_carspec_data[train_carspec_indices, ]
test_carspec_data <- model_carspec_data[-train_carspec_indices, ]

# Fit the random forest model
rf_carspec_model <- randomForest(Fuel_Economy ~ ., data = train_carspec_data)

# Print the model summary
print(rf_carspec_model)

# Make predictions on the test set
predictions <- predict(rf_carspec_model, newdata = test_carspec_data)

# Compare predictions with actual values
results <- data.frame(Actual = test_carspec_data$Fuel_Economy, Predicted = predictions)

# View the results
print(results)
results
