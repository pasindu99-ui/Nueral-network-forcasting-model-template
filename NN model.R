
# Load the necessary libraries
library(readxl)
library(neuralnet)

# Define a function to normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Define a function to de-normalize the data
denormalize <- function(x, norm_params) {
  return (x * (norm_params$max[3] - norm_params$min[3]) + norm_params$min[3])
}

# Load the data
consumption <- read_excel("F:/digree/Second year/second sem/Machine Learning and Data Mining/coursework/uow_consumption.xlsx")

# Normalize the data
norm_params <- list(min = apply(consumption[, -1], 2, min),
                    max = apply(consumption[, -1], 2, max))

consumption[, -1] <- apply(consumption[, -1], 2, normalize)

# Modify the column names
colnames(consumption)[-1] <- c("X6.00PM", "X7.00PM", "X8.00PM")

train <- consumption[1:380, ]
test <- consumption[381:470, ]

# Add two new columns to the `train` and `test` data frames
train$t_4 <- train$X8.00PM[nrow(train)-4]
train$t_7 <- train$X8.00PM[nrow(train)-7]
test$t_4 <- test$X8.00PM[nrow(test)-4]
test$t_7 <- test$X8.00PM[nrow(test)-7]

# Update the `formula` variable
formula <- X8.00PM ~ X6.00PM + X7.00PM + t_4 + t_7

# Retrain the neural network model
model <- neuralnet(formula, data = train[, -1], hidden = c(20, 20),
                   act.fct = "logistic", linear.output = TRUE)
plot(model)
print(model)

test_data_index <- 4

print(test$X6.00PM[test_data_index])
print(test$X7.00PM[test_data_index])

# Create a new data frame with the input values
new_test_data <- data.frame(date = test$date[test_data_index],
                            X6.00PM = test$X6.00PM[test_data_index],
                            X7.00PM = test$X7.00PM[test_data_index],
                            t_4 = test$X8.00PM[nrow(test)-4],
                            t_7 = test$X8.00PM[nrow(test)-7])

# Generate the predicted value
predicted_value_norm <- predict(model, newdata = new_test_data[, -1])

predicted_value <- denormalize(predicted_value_norm, norm_params)

actual_6PM <- test$X6.00PM[test_data_index] * (norm_params$max[1] - norm_params$min[1]) + norm_params$min[1]
actual_7PM <- test$X7.00PM[test_data_index] * (norm_params$max[2] - norm_params$min[2]) + norm_params$min[2]

print(paste("Actual value at 6PM: ", actual_6PM))
print(paste("Actual value at 7PM: ", actual_7PM))

# Print the predicted value and the actual energy consumption value
print(paste("Predicted value: ", predicted_value))
print(paste("Actual value: ", denormalize(test$X8.00PM[test_data_index],norm_params)))

