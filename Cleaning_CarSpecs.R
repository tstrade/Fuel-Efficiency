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
# removed_columns <- c(4, 7, 10, 13, 14, 15, 16, 17, 19,
                     #20, 21, 22, 23, 24, 25, 26, 27, 28,
                     #29, 30, 31, 34, 41, 42, 44, 45, 46,
                     #47, 48, 50, 52, 54, 57, 58, 59, 60,
                     #61, 62, 63, 65, 66, 67, 68, 69, 70,
                     #71, 72, 73, 74, 75, 76, 77, 78)

#removed_CarSpecs <-CarSpecs_45to20[, removed_columns]
# View(removed_CarSpecs)

# Remove rows with any missing integer data or "NA"
cleaned_CarSpecs <- na.omit(cleaned_CarSpecs)

# Replace empty string data with 'Other' for clear categorization
cleaned_CarSpecs[cleaned_CarSpecs == ''] <- 'Other'

# Remove rows with values less than 1996 in column 5 to match the scope of other data set
cleaned_CarSpecs <- cleaned_CarSpecs[cleaned_CarSpecs[, 5] >= 1996, ]

# Arrange columns into relevant format (columns 6 to 22 and 24, with column 23 as the target)
cleaned_CarSpecs <- cleaned_CarSpecs[, c(6:22, 24, 23)]

# Rename columns for clarity (if needed)
colnames(cleaned_CarSpecs)[ncol(cleaned_CarSpecs)] <- "Fuel_Economy"  # Renaming column 23 as Fuel_Economy

# View the cleaned data set in a separate window
View(cleaned_CarSpecs)

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

model_CarSpecs <- df
View(model_CarSpecs)

write.csv(cleaned_CarSpecs, "Cleaned_CarSpecs_1945to2020.csv")
write.csv(model_CarSpecs, "Model_CarSpecs_1945to2020.csv")
