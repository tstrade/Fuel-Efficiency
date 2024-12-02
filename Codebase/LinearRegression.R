library(ggplot2)
library(dplyr)

# Specify the path to Car Specification Dataset file
path_model <- "~/DataScience/Fuel-Efficiency/DataSources/Data Used/Model_CarSpecs_1945to2020.csv"

# Read CSV file into a data frame
CarSpecs_model <- read.csv(path_model)
CarSpecs_model <- CarSpecs_model[2:20]

# Replacing data with the average column value
CarSpecs_model$curb_weight_kg <- replace(CarSpecs_model$curb_weight_kg, is.na(CarSpecs_model$curb_weight_kg), mean(CarSpecs_model$curb_weight_kg, na.rm=TRUE))
CarSpecs_model$maximum_torque_n_m <- replace(CarSpecs_model$maximum_torque_n_m, is.na(CarSpecs_model$maximum_torque_n_m), mean(CarSpecs_model$maximum_torque_n_m, na.rm=TRUE))
CarSpecs_model$fuel_grade <- replace(CarSpecs_model$fuel_grade, is.na(CarSpecs_model$fuel_grade), mean(CarSpecs_model$fuel_grade, na.rm=TRUE))
# This suggest the possibility of revisiting how the data was cleaned
# If the model is performing poorly, more columns can be added and cleaned in this manner

# ------------------------------------------------------------------------------------------------------------

linreg_length <- lm(Fuel_Economy~length_mm, CarSpecs_model)
summary(linreg_length)

ggplot(CarSpecs_model, aes(x=length_mm, y=Fuel_Economy)) +
  geom_point() + 
  geom_smooth(method='lm') +
  ggtitle("Linear Regression: Fuel Economy vs. Body Length")

# ------------------------------------------------------------------------------------------------------------

linreg_curb <- lm(Fuel_Economy~curb_weight_kg, CarSpecs_model)
summary(linreg_curb)

ggplot(CarSpecs_model, aes(x=curb_weight_kg, y=Fuel_Economy)) +
  geom_point() + 
  geom_smooth(method='lm') +
  ggtitle("Linear Regression: Fuel Economy vs. Curb Weight")

# ------------------------------------------------------------------------------------------------------------

linreg_torque <- lm(Fuel_Economy~maximum_torque_n_m, CarSpecs_model)
summary(linreg_torque)

ggplot(CarSpecs_model, aes(x=maximum_torque_n_m, y=Fuel_Economy)) +
  geom_point() + 
  geom_smooth(method='lm') +
  ggtitle("Linear Regression: Fuel Economy vs. Maximum Torque")

# ------------------------------------------------------------------------------------------------------------

linreg_hp <- lm(Fuel_Economy~engine_hp, CarSpecs_model)
summary(linreg_hp)

ggplot(CarSpecs_model, aes(x=engine_hp, y=Fuel_Economy)) +
  geom_point() + 
  geom_smooth(method='lm') +
  ggtitle("Linear Regression: Fuel Economy vs. Horsepower")

# ------------------------------------------------------------------------------------------------------------

linreg_combine <- lm(Fuel_Economy~length_mm + curb_weight_kg + maximum_torque_n_m + engine_hp, CarSpecs_model)
summary(linreg_combine)

ggplot(CarSpecs_model, aes(x=(length_mm + curb_weight_kg + maximum_torque_n_m + engine_hp), y=Fuel_Economy)) +
  geom_point() + 
  geom_smooth(method='lm') +
  ggtitle("Linear Regression: Fuel Economy vs. Specs")

# ------------------------------------------------------------------------------------------------------------

model <- lm(Fuel_Economy~Body_type + number_of_seats + length_mm + curb_weight_kg +
              log(maximum_torque_n_m) + injection_type + cylinder_layout + number_of_cylinders +
              compression_ratio + engine_type + valves_per_cylinder + boost_type +
              engine_placement + log(engine_hp) + drive_wheels + number_of_gears +
              transmission + fuel_grade, CarSpecs_model)

summary(model)

ggplot(CarSpecs_model, aes(x=Body_type + number_of_seats + length_mm + curb_weight_kg +
                             log(maximum_torque_n_m) + injection_type + cylinder_layout + number_of_cylinders +
                             compression_ratio + engine_type + valves_per_cylinder + boost_type +
                             engine_placement + log(engine_hp) + drive_wheels + number_of_gears +
                             transmission + fuel_grade, CarSpecs_model, y=Fuel_Economy)) +
  geom_point() + 
  geom_smooth(method='lm') +
  ggtitle("Linear Regression: Fuel Economy vs. Specs")
