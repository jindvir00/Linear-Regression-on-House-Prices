# Load necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# Load the dataset
data <- read.csv("C:\\Users\\jindm\\Downloads\\house-prices.csv")

# Explore the dataset
head(data)
summary(data)
str(data)

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Check correlations between variables
correlation_matrix <- cor(data[, c("Price", "Size", "Bedrooms", "Bathrooms")])
print(correlation_matrix)

# Visualize correlation matrix
corrplot(correlation_matrix, method = "color")

# Data preprocessing
# Remove rows with missing values
data <- na.omit(data)

# Split the dataset into training and testing sets
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Train the linear regression model
linear_model <- lm(Price ~ Size + Bedrooms + Bathrooms , data = train_data)

# Summary of the model
summary(linear_model)

# Predictions on the test set
predictions <- predict(linear_model, newdata = test_data)

# Model evaluation
# Calculate Mean Squared Error (MSE)
mse <- mean((test_data$Price - predictions)^2)
print(paste("Mean Squared Error (MSE):", mse))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))

# Visualize predicted vs. actual prices
ggplot(test_data, aes(x = predictions, y = Price)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Predicted Price", y = "Actual Price", title = "Predicted vs Actual Prices")

# Conclusion
# We built a linear regression model to predict house prices based on features like size, bedrooms, bathrooms, and year built. The model achieved an RMSE of approximately [RMSE value], indicating [interpretation of performance]. Further improvements could be made by considering additional features or using more advanced modeling techniques.
