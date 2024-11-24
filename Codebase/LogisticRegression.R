library(ggplot2)
library(dplyr)

# Specify the path to Car Specification Dataset file
path_model <- "~/DataScience/Fuel-Efficiency/DataSources/Model_CarSpecs_1945to2020.csv"

# Read CSV file into a data frame
CarSpecs_model <- read.csv(path_model)
CarSpecs_model <- CarSpecs_model[2:20]

# Normalize Data
diff <- (CarSpecs_model$Fuel_Economy - min(CarSpecs_model$Fuel_Economy))
minmax <- (max(CarSpecs_model$Fuel_Economy) - min(CarSpecs_model$Fuel_Economy))
CarSpecs_model$Fuel_Economy <- diff/minmax

View(CarSpecs_model)


# ------------------------------------------------------------------------------------------------------------

logr_bodylen <- glm(Fuel_Economy~length_mm, 
                     data=CarSpecs_model, family=quasibinomial(link="logit"))

summary(logr_bodylen)

prediction_bodylen <- data.frame(length_mm=seq(min(CarSpecs_model$length_mm),
                                        max(CarSpecs_model$length_mm)))

prediction_bodylen$Fuel_Economy <- predict(logr_bodylen, prediction_bodylen, type="response")

plot(Fuel_Economy~length_mm, data=CarSpecs_model)
lines(Fuel_Economy~length_mm, prediction_bodylen, col="green")


# ------------------------------------------------------------------------------------------------------------