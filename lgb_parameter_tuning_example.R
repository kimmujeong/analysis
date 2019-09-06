#lgb 파라미터 튜닝 예제
#https://github.com/Microsoft/LightGBM/issues/127
data(agaricus.train, package = "lightgbm")
train <- agaricus.train
dtrain <- lgb.Dataset(train$data, label = train$label, free_raw_data = FALSE)
data(agaricus.test, package = "lightgbm")
test <- agaricus.test
dtest <- lgb.Dataset.create.valid(dtrain, test$data, label = test$label)
valids <- list(test = dtest)

grid_search <- expand.grid(Depth = 8,
                           L1 = 0:5,
                           L2 = 0:5)

model <- list()
perf <- numeric(nrow(grid_search))

for (i in 1:nrow(grid_search)) {
  model[[i]] <- lgb.train(list(objective = "regression",
                               metric = "l2",
                               lambda_l1 = grid_search[i, "L1"],
                               lambda_l2 = grid_search[i, "L2"],
                               max_depth = grid_search[i, "Depth"]),
                          dtrain, #data
                          2, #nrounds 
                          valids, #valids = list()
                          min_data = 1,
                          learning_rate = 1,
                          early_stopping_rounds = 5)
  perf[i] <- min(rbindlist(model[[i]]$record_evals$test$l2))
}
model[[1]]$record_evals
cat("Model ", which.min(perf), " is lowest loss: ", min(perf), sep = "")
print(grid_search[which.min(perf), ])
