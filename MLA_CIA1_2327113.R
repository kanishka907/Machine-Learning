# Load necessary libraries
library(tidyverse)
library(caret)

# Load the dataset
games <- read.csv('C:/Users/KIIT/Downloads/games.csv')

# View the structure of the dataset to understand its columns
str(games)

# Create a new feature for total score (if not already present)
if (!"total_score" %in% colnames(games)) {
  games$total_score <- games$PTS_home + games$PTS_away
}

# Check if total_score is created
str(games)
summary(games$total_score)

# Ensure that there are no missing values in the total_score column
games <- na.omit(games)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(games$total_score, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- games[trainIndex, ]
testData  <- games[-trainIndex, ]
testData

# Verify the split
dim(trainData)
dim(testData)


# Plot histogram of the target variable
ggplot(games, aes(x = total_score)) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black") + 
  ggtitle("Distribution of Total Score") +
  xlab("Total Score") + 
  ylab("Frequency")


# Convert categorical variables to factors (adjust based on actual variable names)
games$home_team <- as.factor(games$HOME_TEAM_ID)
games$away_team <- as.factor(games$VISITOR_TEAM_ID)

# Boxplot of total score by home team
ggplot(games, aes(x = home_team, y = total_score)) + 
  geom_boxplot() + 
  ggtitle("Total Score by Home Team") +
  xlab("Home Team") + 
  ylab("Total Score") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Boxplot of total score by away team
ggplot(games, aes(x = away_team, y = total_score)) + 
  geom_boxplot() + 
  ggtitle("Total Score by Away Team") +
  xlab("Away Team") + 
  ylab("Total Score") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Load necessary libraries
library(reshape2)

# Calculate the correlation matrix
correlations <- cor(games %>% select_if(is.numeric))

# Melt the correlation matrix for ggplot
melted_correlations <- melt(correlations)

# Plot the heatmap
ggplot(melted_correlations, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) + 
  coord_fixed()



# Load necessary libraries
library(ggplot2)

# Simple Linear Regression
simple_model <- lm(total_score ~ PTS_home, data=games)
summary(simple_model)

# Plotting the regression line
ggplot(games, aes(x=PTS_home, y=total_score)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  labs(title="Simple Linear Regression", x="Points Scored by Home Team", y="Total Score")



# Multiple Linear Regression
multiple_model <- lm(total_score ~ PTS_home + PTS_away + FG_PCT_home + FG_PCT_away, data=games)
summary(multiple_model)

# Plotting the predicted vs actual values
multiple_predictions <- predict(multiple_model, newdata=games)
ggplot(games, aes(x=total_score, y=multiple_predictions)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title="Multiple Linear Regression: Predicted vs Actual", x="Actual Total Score", y="Predicted Total Score")



# Load necessary libraries
library(glmnet)

# Prepare data for Ridge Regression
x <- model.matrix(total_score ~ PTS_home + PTS_away + FG_PCT_home + FG_PCT_away, data=games)[, -1]
y <- games$total_score

# Perform Ridge Regression
set.seed(123)
ridge_model <- cv.glmnet(x, y, alpha=0)  # alpha=0 for Ridge
print(ridge_model)

# Best lambda
best_lambda_ridge <- ridge_model$lambda.min
best_lambda_ridge

# Plotting the cross-validated MSE for Ridge Regression
plot(ridge_model)




# Perform Lasso Regression
set.seed(123)
lasso_model <- cv.glmnet(x, y, alpha=1)  # alpha=1 for Lasso
print(lasso_model)

# Best lambda
best_lambda_lasso <- lasso_model$lambda.min
best_lambda_lasso

# Plotting the cross-validated MSE for Lasso Regression
plot(lasso_model)




# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(games$total_score, p = .8, list = FALSE, times = 1)
trainData <- games[trainIndex, ]
testData  <- games[-trainIndex, ]

# Prepare test data for Ridge and Lasso
x_test <- model.matrix(total_score ~ PTS_home + PTS_away + FG_PCT_home + FG_PCT_away, data=testData)[, -1]
y_test <- testData$total_score

# Predicting with the models
# Simple Linear Regression
simple_predictions <- predict(simple_model, newdata=testData)

# Multiple Linear Regression
multiple_predictions <- predict(multiple_model, newdata=testData)

# Ridge Regression
ridge_predictions <- predict(ridge_model, s=best_lambda_ridge, newx=x_test)

# Lasso Regression
lasso_predictions <- predict(lasso_model, s=best_lambda_lasso, newx=x_test)

# Calculate RMSE for comparison
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

simple_rmse <- rmse(y_test, simple_predictions)
multiple_rmse <- rmse(y_test, multiple_predictions)
ridge_rmse <- rmse(y_test, ridge_predictions)
lasso_rmse <- rmse(y_test, lasso_predictions)

# Print RMSE values
simple_rmse
multiple_rmse
ridge_rmse
lasso_rmse


# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(games$total_score, p = .8, list = FALSE, times = 1)
trainData <- games[trainIndex, ]
testData  <- games[-trainIndex, ]

# Prepare test data for Ridge and Lasso
x_test <- model.matrix(total_score ~ PTS_home + PTS_away + FG_PCT_home + FG_PCT_away, data=testData)[, -1]
y_test <- testData$total_score

# Predicting with the models
# Simple Linear Regression
simple_predictions <- predict(simple_model, newdata=testData)

# Multiple Linear Regression
multiple_predictions <- predict(multiple_model, newdata=testData)

# Ridge Regression
ridge_predictions <- predict(ridge_model, s=best_lambda_ridge, newx=x_test)

# Lasso Regression
lasso_predictions <- predict(lasso_model, s=best_lambda_lasso, newx=x_test)

# Calculate RMSE for comparison
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

simple_rmse <- rmse(y_test, simple_predictions)
multiple_rmse <- rmse(y_test, multiple_predictions)
ridge_rmse <- rmse(y_test, ridge_predictions)
lasso_rmse <- rmse(y_test, lasso_predictions)

# Print RMSE values
print(paste("Simple Linear Regression RMSE:", simple_rmse))
print(paste("Multiple Linear Regression RMSE:", multiple_rmse))
print(paste("Ridge Regression RMSE:", ridge_rmse))
print(paste("Lasso Regression RMSE:", lasso_rmse))

# Plotting RMSE values
rmse_values <- data.frame(
  Model = c("Simple Linear", "Multiple Linear", "Ridge", "Lasso"),
  RMSE = c(simple_rmse, multiple_rmse, ridge_rmse, lasso_rmse)
)

ggplot(rmse_values, aes(x=Model, y=RMSE)) +
  geom_bar(stat="identity", fill="skyblue") +
  labs(title="RMSE of Different Regression Models", x="Model", y="RMSE")




# Calculate RMSE for each model
simple_rmse <- rmse(y_test, simple_predictions)
multiple_rmse <- rmse(y_test, multiple_predictions)
ridge_rmse <- rmse(y_test, ridge_predictions)
lasso_rmse <- rmse(y_test, lasso_predictions)

# Convert RMSE to MSE for comparison (MSE = RMSE^2)
simple_mse <- simple_rmse^2
multiple_mse <- multiple_rmse^2
ridge_mse <- ridge_rmse^2
lasso_mse <- lasso_rmse^2

# Select the best model based on the lowest MSE
best_model <- ifelse(min(simple_mse, multiple_mse, ridge_mse, lasso_mse) == simple_mse, "Simple Linear Regression",
                     ifelse(min(simple_mse, multiple_mse, ridge_mse, lasso_mse) == multiple_mse, "Multiple Linear Regression",
                            ifelse(min(simple_mse, multiple_mse, ridge_mse, lasso_mse) == ridge_mse, "Ridge Regression", "Lasso Regression")))

print(paste("The best model is:", best_model))


