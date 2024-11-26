library(dplyr)

# Specify the path to Car Specification Dataset file
file_path <- "~/DataScience/Fuel-Efficiency/DataSources/CarSpecs1945to2020.csv"

# Read CSV file into a data frame
CarSpecs <- read.csv(file_path)

# Select the specified columns 
selected_columns <- c(2, 3, 4, 5, 7, 8, 9, 11, 12, 13, 
                      14, 15, 16, 17, 18, 32, 33, 35, 36, 
                      38, 39, 41, 42, 45, 48, 49, 50, 51, 
                      53, 55, 56, 59, 60, 61, 62, 64, 65,
                      67, 68)

CarSpecs <- CarSpecs[, selected_columns]

# Get rid of rows that use diesel
CarSpecs <- subset(CarSpecs, !(fuel_grade %in% 'diesel'))

StrCols <- c("Make", "Modle", "Generation", "Series", "Trim", "Body_type", "injection_type", 
             "cylinder_layout", "engine_type", "drive_wheels", "transmission", 
             "max_speed_km_per_h", "rear_brakes", "front_brakes")

# Many rows are strings when they should be numbers, so we can convert
StrtoNumCols <- c("number_of_seats", "length_mm", "width_mm", "height_mm", "wheelbase_mm", 
             "front_track_mm", "curb_weight_kg", "maximum_torque_n_m", "rear_track_mm",
             "turnover_of_maximum_torque_rpm", "capacity_cm3", "engine_hp_rpm", 
             "fuel_tank_capacity_l", "fuel_grade", "max_speed_km_per_h")

convertStrtoNum <- function(df_col) {
  replace(df_col, df_col == "", "0")
  df_col <- substring(df_col[df_col != "0"], 1, 1)
  df_col <- strtoi(df_col)
  temp <- mean(df_col[!is.na(df_col)])
  df_col[is.na(df_col)] <- temp
  df_col
}

for (x in 1:length(StrtoNumCols)) {
  CarSpecs[StrtoNumCols[x]] <- convertStrtoNum((CarSpecs[StrtoNumCols[x]]))
}

# To fill in NA values, we take the average value of the column and use that as the new value
NumCols <- c("Year_from", "number_of_cylinders", "valves_per_cylinder", "cylinder_bore_mm", 
             "stroke_cycle_mm", "engine_hp", "number_of_gears", "mixed_fuel_consumption_per_100_km_l",
             "acceleration_0_100_km.h_s", "city_fuel_per_100km_l", "highway_fuel_per_100km_l")

for (x in 1:length(NumCols)) {
  df <- CarSpecs[NumCols[x]]
  df[is.na(df)] <- mean(df[!is.na(df)])
  CarSpecs[NumCols[x]] <- df
}

# Empty string data can be labeled as other - Random Forest Regression can handle this
CarSpecs[CarSpecs == ''] <- 'Other'

# Rearrange columns and emphasize target variable
CarSpecs <- CarSpecs[, c(1:35, 37:39, 36)]
colnames(CarSpecs)[ncol(CarSpecs)] <- "Fuel_Economy"

# For good measure, remove any remain NA values
CarSpecs <- na.omit(CarSpecs)

write.csv(CarSpecs, "~/DataScience/Fuel-Efficiency/DataSources/Final_Model_Data_Str_Num.csv")

chars <- c()
for (x in 1:length(StrCols)) {
  chars[x] <- as.character(CarSpecs[StrCols[x]])
}
uf <- unique(chars)
for (x in 1:length(StrCols)) {
  CarSpecs[StrCols[x]] <- as.numeric(factor(CarSpecs[StrCols[x]], levels=uf))
}

View(CarSpecs)
str(CarSpecs)

write.csv(CarSpecs, "~/DataScience/Fuel-Efficiency/DataSources/Final_Model_Data_Num_Only.csv")
