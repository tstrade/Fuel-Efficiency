library(ggplot2)
library(dplyr)

# Specify the path to Car Specification Dataset file
path_model <- "~/DataScience/Fuel-Efficiency/DataSources/Model_CarSpecs_1945to2020.csv"

# Read CSV file into a data frame
CarSpecs_model <- read.csv(path_model)
CarSpecs_model <- CarSpecs_model[2:20]

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

