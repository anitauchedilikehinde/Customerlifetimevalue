# import relevant libraries
library(dplyr) # data manipulation
library(tidyr) # data tidying
library(caret) # machine learning

# load dataset from chapter 3
head(rfm_data4)
dim(rfm_data4)

# normalization 
# subset the dataset with only columns to be normalized
glm_data_full <- rfm_data4[, c('Recency', 'Frequency', 'Total_Amount', 'DPR', 'Age', 'Avg_Amount')]

# remove one-time customers
glm_data <- glm_data_full[!glm_data_full$Frequency == 0, ]
head(glm_data)
dim(glm_data)

# apply mean center normalization to all columns except CSI and CLV
glm_data <- glm_data %>%
  mutate_all(~ . - mean(., na.rm = TRUE)) %>%
  mutate_all(~ ./sd(., na.rm = TRUE))
head(glm_data)

# add the CSI and CLV columns back to the dataset
glm_data_full <- rfm_data4[!rfm_data4$Frequency == 0, c('CSI', 'CLV')]
glm_data <- cbind(glm_data, glm_data_full)
head(glm_data)
dim(glm_data)

### split data into train and test sets (75%/25%)
set.seed(50)
train_index <- createDataPartition(glm_data$CLV, p = 0.75, list = FALSE)
train_data <- glm_data[train_index, ]
test_data <- glm_data[-train_index, ]

dim(train_data)
dim(test_data)

### GLM model
# build the preliminary model
glm_model <- train(CLV ~ ., data = train_data, method = "glm", family = "gaussian")

# Make predictions on the test set
test_pred <- predict(glm_model, newdata = test_data)

# Evaluate the model
test_rmse <- RMSE(test_pred, test_data$CLV)
test_r_squared <- R2(test_pred, test_data$CLV)
n <- nrow(test_data)
p <- length(glm_model$coefficients) - 1
adj_r_squared <- 1 - ((n - 1) / (n - p - 1)) * (1 - test_r_squared)
mse <- mean((test_data$CLV - test_pred)^2)
mae <- mean(abs(test_data$CLV - test_pred))

# Print the results
cat("Test R-squared:", test_r_squared, "\n")
cat(paste("Adjusted R-squared: ", round(adj_r_squared, 4), "\n"))
cat("Test RMSE:", test_rmse, "\n")
cat(paste("MSE: ", round(mse, 4), "\n"))
cat(paste("MAE: ", round(mae, 4), "\n"))

### Model tuning
# Step 1: create a grid of hyper parameters to choose from
hyperparameters <- expand.grid(alpha = seq(0, 1, by = 0.1), lambda = seq(0, 1, by = 0.1))

# Step 2: Define the model training and evaluation process using 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary, 
                              verboseIter = TRUE)

# Step 3: Fit GLM model with hyperparameter tuning and 10-fold cross-validation
glm_model <- train(CLV ~ ., data = train_data, method = "glmnet",
                    trControl = train_control, tuneGrid = hyperparameters, 
                    metric = "RMSE", standardize = TRUE)

# Step 4: Get predictions 
test_pred <- predict(glm_model, newdata = test_data)

# Step 5: Evaluate the model
test_rmse <- RMSE(test_pred, test_data$CLV)
test_r_squared <- R2(test_pred, test_data$CLV)
n <- nrow(test_data)
p <- length(glm_model$coefficients) - 1
adj_r_squared <- 1 - ((n - 1) / (n - p - 1)) * (1 - test_r_squared)
mse <- mean((test_data$CLV - test_pred)^2)
mae <- mean(abs(test_data$CLV - test_pred))

# Step 6: Print the results
cat("Test R-squared:", test_r_squared, "\n")
cat(paste("Adjusted R-squared: ", round(adj_r_squared, 4), "\n"))
cat("Test RMSE:", test_rmse, "\n")
cat(paste("MSE: ", round(mse, 4), "\n"))
cat(paste("MAE: ", round(mae, 4), "\n"))
