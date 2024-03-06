

# train Decision Tree model
ctrl <- trainControl(
  method = "LOOCV",
  verboseIter = TRUE
)

grid <- expand.grid(maxdepth = 6)

set.seed(555)
tree_model <- train(form = rating_group ~ .,
                    data = train_data,
                    method = "rpart2",
                    trControl = ctrl,
                    tuneGrid = grid)

# Visualize Decision Tree model
rpart.plot(tree_model$finalModel)


# Prediction by Decision Tree Model
tree_predict <-  predict(tree_model, test_data)


# Evaluate Decision Tree model
confusionMatrix(tree_predict,
                reference =test_data$rating_group,
                mode = "prec_recall")


