library(randomForest)
library(dplyr)
library(ggplot2)
library(rfviz)

# Specify the path to cleaned data
df <- read.csv("~/DataScience/Fuel-Efficiency/DataSources/Final_Model_Data_Num_Only.csv")

dim(df)
str(df)

# Reproducibility
set.seed(7)

# Split 80/20 into training/test 
train <- df %>% dplyr::sample_frac(0.80)
test <- dplyr::anti_join(df, train, by = 'X')

write.csv(train, "Training_Data.csv")
write.csv(test, "Testing_Data.csv")

# Build the Random Forest Regression
model <- randomForest(Fuel_Economy ~ ., data = train, ntree = 2000,
                      importance = TRUE, keep.forest = TRUE)

summary(model)

treesize(model, terminal = TRUE)

plot(model, type="l", main="RFR Error vs. Number of Trees")

# Visualize variable importance
imp <- importance(model)
important_vars <- rownames(imp)[order(imp[,1], decreasing = TRUE)]

varImpPlot(model, sort = TRUE, scale = TRUE, 
           main = "Ranked Importance of Predictors")

varUsed(model, by.tree = FALSE, count = TRUE)

write.csv(getTree(model, k = 1, labelVar = TRUE), "Single_Tree_Example.csv")

which.min(model$mse)

sqrt(model$mse[which.min(model$mse)])
# RMSE = 0.05581966

# Test the model
prediction <- predict(model, newdata = test)
table(prediction, test$Fuel_Economy)
prediction


# Display results
results <- cbind(prediction, test$Fuel_Economy)
results

colnames(results) <- c('pred','real')
results <- as.data.frame(results)

View(results)

write.csv(results, file="Fuel_Economy_RFR_Results.csv")

partialPlot(model, train, engine_hp, "veriscolor")
# Calculate the accuracy of the model
pred_mean <- mean(prediction)
pred_stdDev <- sd(prediction)

test_mean <- mean(test$Fuel_Economy)
test_stdDev <- sd(test$Fuel_Economy)

# H0 : prediction mean = test mean, 95% confidence
mean_diff <- test_mean - pred_mean

z0 <- mean_diff / sqrt((test_stdDev**2 / nrow(test)) + (pred_stdDev**2 / nrow(test)))
z0
# z0 = 0.07044321 < 1.96 (z_0.025)
# Therefore, we fail to reject the null hypothesis



# Let's see if we can tune the model
tuned <- tuneRF(x = train[1:39], y = train$Fuel_Economy,
                ntreeTry = 2500, ,tryStart = 5,
                stepFactor = 1.5, improve = 0.1,
                trace = FALSE)

# The ideal number of candidates at each split is 19
#   The model defaults to num. vars / 3, 
#   so let's retrain the model with this new information
tuned_model <- randomForest(Fuel_Economy ~ ., data = train, ntree = 2000,
                            mtry = 19, importance = TRUE, keep.forest = TRUE,
                            keep.inbag = TRUE)

summary(tuned_model)

treesize(tuned_model, terminal = TRUE)

plot(tuned_model, type="l", main="Tuned RFR Error vs. Number of Trees")

# Visualize variable importance
tuned_imp <- importance(tuned_model)
tuned_imp_vars <- rownames(tuned_imp)[order(tuned_imp[,1], decreasing = TRUE)]

varImpPlot(tuned_model, sort = TRUE, scale = TRUE, 
           main = "Ranked Importance of Predictors (Tuned)")

varUsed(tuned_model, by.tree = FALSE, count = TRUE)

write.csv(getTree(tuned_model, k = 1, labelVar = TRUE), "Single_Tree_Tuned_Example.csv")

which.min(tuned_model$mse)

sqrt(tuned_model$mse[which.min(tuned_model$mse)])
# RMSE = 0.05532445

# Test the model
tuned_prediction <- predict(tuned_model, newdata = test)
table(tuned_prediction, test$Fuel_Economy)
tuned_prediction


# Display results
tuned_results <- cbind(tuned_prediction, test$Fuel_Economy)
tuned_results

colnames(tuned_results) <- c('pred','real')
tuned_results <- as.data.frame(tuned_results)

View(tuned_results)

write.csv(tuned_results, file="Fuel_Economy_RFR_Tuned_Results.csv")

# Calculate the accuracy of the model
tuned_pred_mean <- mean(tuned_prediction)
tuned_pred_stdDev <- sd(tuned_prediction)

test_mean <- mean(test$Fuel_Economy)
test_stdDev <- sd(test$Fuel_Economy)

# H0 : prediction mean = test mean, 95% confidence
tuned_mean_diff <- test_mean - tuned_pred_mean

z0 <- mean_diff / sqrt((test_stdDev**2 / nrow(test)) + (tuned_pred_stdDev**2 / nrow(test)))
z0
# z0 = 0.06767788 < 1.96 (z_0.025)
# Therefore, we fail to reject the null hypothesis

# Let's compare the initial model to the tuned model
models_mean_diff <- tuned_pred_mean - pred_mean

models_z0 <- models_mean_diff / sqrt((tuned_pred_stdDev**2 / nrow(test)) + (pred_stdDev**2 / nrow(test)))
models_z0
# z0 = 0.002822168 < 1.96 (z_0.025)
# Therefore, the is not a statistically significant difference between the two models

left_conf <- models_mean_diff - 1.96 * sqrt((tuned_pred_stdDev**2 / nrow(test)) + (pred_stdDev**2 / nrow(test)))
right_conf <- models_mean_diff + 1.96 * sqrt((tuned_pred_stdDev**2 / nrow(test)) + (pred_stdDev**2 / nrow(test)))
# -0.00189 < u1 - u2 = 0 (H0) < 0.00190

dim(df)
dim(train)
dim(test)
dim(results)
dim(tuned_results)
# Let's view some variations / distributions in the original, training, testing,
# initial prediction, and tuned prediction data
df_norm <- rnorm(57967, mean = mean(df$Fuel_Economy, na.rm = TRUE), 
                 sd = sd(df$Fuel_Economy, na.rm = TRUE))
train_norm <- rnorm(46374, mean = mean(train$Fuel_Economy, na.rm = TRUE),
                    sd = sd(train$Fuel_Economy, na.rm = TRUE))
test_norm <- rnorm(11593, mean = mean(test$Fuel_Economy, na.rm = TRUE),
                   sd = sd(test$Fuel_Economy, na.rm = TRUE))
init_rslts_norm <- rnorm(11593, mean = mean(results$pred, na.rm = TRUE),
                   sd = sd(results$pred, na.rm = TRUE))
tuned_rslts_norm <- rnorm(11593, mean = mean(tuned_results$pred, na.rm = TRUE),
                          sd = sd(tuned_results$pred, na.rm = TRUE))


