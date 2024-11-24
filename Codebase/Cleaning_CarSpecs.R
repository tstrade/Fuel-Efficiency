library(dplyr)

# Specify the path to Car Specification Dataset file
file_path <- "~/DataScience/Fuel-Efficiency/DataSources/CarSpecs1945to2020.csv"

# Read CSV file into a data frame
CarSpecs_45to20 <- read.csv(file_path)

# Select the specified columns 
selected_columns <- c(1, 2, 3, 5, 6, 9, 11, 12, 18, 
                      32, 33, 35, 36, 37, 38, 39, 
                      40, 43, 49, 51, 53, 55, 56, 64)

cleaned_CarSpecs <- CarSpecs_45to20[, selected_columns]

# Remove rows with any missing integer data or "NA"
cleaned_CarSpecs <- na.omit(cleaned_CarSpecs)

# Replace empty string data with 'Other' for clear categorization
cleaned_CarSpecs[cleaned_CarSpecs == ''] <- 'Other'

# Arrange columns into relevant format (columns 6 to 22 and 24, with column 23 as the target)
cleaned_CarSpecs <- cleaned_CarSpecs[, c(4, 6:22, 24, 23)]

# Rename columns for clarity (if needed)
colnames(cleaned_CarSpecs)[ncol(cleaned_CarSpecs)] <- "Fuel_Economy"  # Renaming column 23 as Fuel_Economy

# For the model creation, replace strings with unique integers
df <- cleaned_CarSpecs
uf <- unique(c(as.character(df$Body_type), as.character(df$engine_type),
               as.character(df$drive_wheels), as.character(df$transmission),
               as.character(df$injection_type), as.character(df$cylinder_layout), 
               as.character(df$compression_ratio), as.character(df$boost_type),
               as.character(df$engine_placement)))

df$Body_type <- as.numeric(factor(df$Body_type,levels=uf))
df$engine_type <- as.numeric(factor(df$engine_type,levels=uf))
df$drive_wheels <- as.numeric(factor(df$drive_wheels,levels=uf))
df$transmission <- as.numeric(factor(df$transmission,levels=uf))
df$injection_type <- as.numeric(factor(df$injection_type,levels=uf))
df$cylinder_layout <- as.numeric(factor(df$cylinder_layout,levels=uf))
df$compression_ratio <- as.numeric(factor(df$compression_ratio,levels=uf))
df$boost_type <- as.numeric(factor(df$boost_type,levels=uf))
df$engine_placement <- as.numeric(factor(df$engine_placement,levels=uf))

CarSpecs_model <- df
CarSpecs_cleaned <- cleaned_CarSpecs

CarSpecs_cleaned <- CarSpecs_cleaned[2:20]
str(CarSpecs_cleaned)

CarSpecs_model <- CarSpecs_model[2:20]
str(CarSpecs_model)

CarSpecs_cleaned$number_of_seats <- substring(CarSpecs_cleaned$number_of_seats, 1, 1)
CarSpecs_model$number_of_seats <- substring(CarSpecs_model$number_of_seats, 1, 1)

CarSpecs_cleaned$number_of_seats <- strtoi(CarSpecs_cleaned$number_of_seats)
CarSpecs_model$number_of_seats <- strtoi(CarSpecs_model$number_of_seats)

CarSpecs_cleaned$curb_weight_kg <- strtoi(CarSpecs_cleaned$curb_weight_kg)
CarSpecs_model$curb_weight_kg <- strtoi(CarSpecs_model$curb_weight_kg)

CarSpecs_cleaned$maximum_torque_n_m <- strtoi(CarSpecs_cleaned$maximum_torque_n_m)
CarSpecs_model$maximum_torque_n_m <- strtoi(CarSpecs_model$maximum_torque_n_m)

CarSpecs_cleaned$fuel_grade <- strtoi(CarSpecs_cleaned$fuel_grade)
CarSpecs_model$fuel_grade <- strtoi(CarSpecs_model$fuel_grade)

View(CarSpecs_cleaned)
View(CarSpecs_model)

write.csv(CarSpecs_cleaned, "Cleaned_CarSpecs_1945to2020.csv")
write.csv(CarSpecs_model, "Model_CarSpecs_1945to2020.csv")
