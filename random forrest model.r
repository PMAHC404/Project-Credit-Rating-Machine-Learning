library(readr)
library(tidyverse)
library(caret)

# 1.loading data
data <- read_csv("fininsdata.csv")
data$rating_group <- factor(data$rating_group,
                            levels = c("spec", "low", "medium", "high"),
                            labels = c("spec", "low", "medium", "high"))

# 2. Split data 80% for train and 20% for test
split_data <- function (data, train_size = 0.8) {
  set.seed(747)
  n <- nrow(data)
  train_id <- sample(1:n, n*train_size)
  train_df <- data[train_id, ]
  test_df <- data[-train_id, ]
  return(list(train = train_df,
              test = test_df))
}

prep_data <- split_data(data, train_size = 0.8)
train_data <- prep_data$train
test_data <- prep_data$test


# 3. train decision tree model
ctrl <- trainControl(
  method = "LOOCV",
  verboseIter = TRUE
)

set.seed(750)
rf_model <- train(form = rating_group ~ .,
               data = train_data,
               method = "rf",
               trControl = ctrl)


# 4. Prediction
predict <- predict(rf_model, newdata = test_data)



# 5. Evaluate model
confusionMatrix(data = predict,
                reference = test_data$rating_group,
                mode = "prec_recall")
