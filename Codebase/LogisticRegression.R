library(ggplot2)
library(dplyr)

# Specify the path to Car Specification Dataset file
path_model <- "~/DataScience/Fuel-Efficiency/DataSources/Data Used/Model_CarSpecs_1945to2020.csv"

# Read CSV file into a data frame
CarSpecs_model <- read.csv(path_model)
CarSpecs_model <- CarSpecs_model[2:20]

# Normalize Data
diff <- (CarSpecs_model$Fuel_Economy - min(CarSpecs_model$Fuel_Economy))
minmax <- (max(CarSpecs_model$Fuel_Economy) - min(CarSpecs_model$Fuel_Economy))
CarSpecs_model$Fuel_Economy <- diff/minmax

# Previous runs indicated NA data still exists
na_counts_base <- colSums(is.na(CarSpecs_model))
na_counts_base

# Replacing data with the average column value
CarSpecs_model$curb_weight_kg <- replace(CarSpecs_model$curb_weight_kg, is.na(CarSpecs_model$curb_weight_kg), mean(CarSpecs_model$curb_weight_kg, na.rm=TRUE))
CarSpecs_model$maximum_torque_n_m <- replace(CarSpecs_model$maximum_torque_n_m, is.na(CarSpecs_model$maximum_torque_n_m), mean(CarSpecs_model$maximum_torque_n_m, na.rm=TRUE))
CarSpecs_model$fuel_grade <- replace(CarSpecs_model$fuel_grade, is.na(CarSpecs_model$fuel_grade), mean(CarSpecs_model$fuel_grade, na.rm=TRUE))
# This suggest the possibility of revisiting how the data was cleaned
# If the model is performing poorly, more columns can be added and cleaned in this manner

str(CarSpecs_model)
View(CarSpecs_model)

# ------------------------------------------------------------------------------------------------------------

logr_bodylen <- glm(Fuel_Economy~length_mm, 
                     data=CarSpecs_model, family=quasibinomial(link="logit"))

summary(logr_bodylen)

ggplot(CarSpecs_model, aes(x=length_mm, y=Fuel_Economy)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), se = FALSE) 

# ------------------------------------------------------------------------------------------------------------

logr_curb <- glm(Fuel_Economy~curb_weight_kg, 
                    data=CarSpecs_model, family=quasibinomial(link="logit"))

summary(logr_curb)

ggplot(CarSpecs_model, aes(x=curb_weight_kg, y=Fuel_Economy)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), se = FALSE) 

# ------------------------------------------------------------------------------------------------------------

logr_torque <- glm(Fuel_Economy~maximum_torque_n_m, 
                    data=CarSpecs_model, family=quasibinomial(link="logit"))

summary(logr_torque)

ggplot(CarSpecs_model, aes(x=maximum_torque_n_m, y=Fuel_Economy)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), se = FALSE) 

# ------------------------------------------------------------------------------------------------------------

logr_horsepower <- glm(Fuel_Economy~engine_hp, 
                    data=CarSpecs_model, family=quasibinomial(link="logit"))

summary(logr_horsepower)

ggplot(CarSpecs_model, aes(x=engine_hp, y=Fuel_Economy)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), se = FALSE) 

# ------------------------------------------------------------------------------------------------------------

logr_specs <- glm(Fuel_Economy~length_mm + curb_weight_kg + maximum_torque_n_m + engine_hp, 
                    data=CarSpecs_model, family=quasibinomial(link="logit"))

summary(logr_specs)

ggplot(CarSpecs_model, aes(x=length_mm + curb_weight_kg + maximum_torque_n_m + engine_hp, y=Fuel_Economy)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), se = FALSE) 

# ------------------------------------------------------------------------------------------------------------

model <- glm(Fuel_Economy~Body_type + number_of_seats + length_mm + curb_weight_kg + 
               maximum_torque_n_m + injection_type + cylinder_layout + number_of_cylinders +
               compression_ratio + engine_type + valves_per_cylinder + boost_type +
               engine_placement + engine_hp + drive_wheels + number_of_gears +
               transmission + fuel_grade, 
             data=CarSpecs_model, 
             family=quasibinomial(link="logit"))

summary(model)

ggplot(CarSpecs_model, aes(x=Body_type + number_of_seats + length_mm + curb_weight_kg + 
                             maximum_torque_n_m + injection_type + cylinder_layout + number_of_cylinders +
                             compression_ratio + engine_type + valves_per_cylinder + boost_type +
                             engine_placement + engine_hp + drive_wheels + number_of_gears +
                             transmission + fuel_grade, y=Fuel_Economy)) + 
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), se = FALSE) +
  ggtitle("Logistic Regression Model of Fuel Economy vs. All Variables") +
  xlab("All Specifications")
