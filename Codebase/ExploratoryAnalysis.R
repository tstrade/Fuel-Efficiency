library(ggplot2)
library(dplyr)

# Specify the path to Car Specification Dataset file
path_model <- "~/DataScience/Fuel-Efficiency/DataSources/Model_CarSpecs_1945to2020.csv"
path_cleaned <- "~/DataScience/Fuel-Efficiency/DataSources/Cleaned_CarSpecs_1945to2020.csv"

# Read CSV file into a data frame
CarSpecs_cleaned <- read.csv(path_cleaned)
CarSpecs_cleaned <- CarSpecs_cleaned[2:20]
CarSpecs_model <- read.csv(path_model)
CarSpecs_model <- CarSpecs_model[2:20]

View(CarSpecs_cleaned)
# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=Body_type,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Body Type")

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=number_of_seats,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Number of Seats")

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=length_mm,y=Fuel_Economy, color=Fuel_Economy)) + geom_point() + 
  geom_smooth() + ggtitle("Fuel Economy Distribution as a Function of Body Length (mm)")

ggplot(CarSpecs_cleaned, aes(x=length_mm)) + geom_density()
# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=curb_weight_kg,y=Fuel_Economy, color=Fuel_Economy)) + geom_point() + 
  geom_smooth() + ggtitle("Fuel Economy Distribution as a Function of Curb Weight")

ggplot(CarSpecs_cleaned, aes(x=curb_weight_kg)) + geom_density()
ggplot(CarSpecs_cleaned, aes(x=log(curb_weight_kg))) + geom_density()

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=maximum_torque_n_m,y=Fuel_Economy, color=Fuel_Economy)) + geom_point() + 
  geom_smooth() + ggtitle("Fuel Economy Distribution as a Function of Maximum Torque (Nm)")

ggplot(CarSpecs_cleaned, aes(x=maximum_torque_n_m)) + geom_density()
ggplot(CarSpecs_cleaned, aes(x=log(maximum_torque_n_m))) + geom_density()

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=injection_type,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Injection Type") + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=cylinder_layout,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Cylinder Layout")

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=number_of_cylinders,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() +
  ggtitle("Fuel Economy Distribution as a Function of Number of Cylinders")

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=compression_ratio,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Compression Ratio") + 
  theme(axis.text.x = element_text(angle=80, hjust=1))

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=engine_type,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Engine Type") + 
  theme(axis.text.x = element_text(angle=30, hjust=1))

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=valves_per_cylinder,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Number of Valves per Cylinder")

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=boost_type,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Boost Type") + 
  theme(axis.text.x = element_text(angle=30, hjust=1))

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=engine_placement,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Engine Placement") + 
  theme(axis.text.x = element_text(angle=30, hjust=1))

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=engine_hp,y=Fuel_Economy, color=Fuel_Economy)) + geom_point() + 
  geom_smooth() + ggtitle("Fuel Economy Distribution as a Function of Horsepower")

ggplot(CarSpecs_cleaned, aes(x=engine_hp)) + geom_density()
ggplot(CarSpecs_cleaned, aes(x=log(engine_hp))) + geom_density()

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=drive_wheels,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Drive Wheels") + 
  theme(axis.text.x = element_text(angle=30, hjust=1))

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=number_of_gears,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Number of Gears")

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=transmission,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Transmission") + 
  theme(axis.text.x = element_text(angle=30, hjust=1))

# ------------------------------------------------------------------------------------------------------------

ggplot(CarSpecs_cleaned, aes(x=fuel_grade,y=Fuel_Economy, color=Fuel_Economy)) + geom_count() + 
  ggtitle("Fuel Economy Distribution as a Function of Fuel Grade")

# ------------------------------------------------------------------------------------------------------------




