library(fastAdaboost)
library(lightgbm)
library(Matrix)
train<-read.csv("porto/train.csv")
test<-read.csv("porto/test.csv")
#target:보험청구 가능성

#adaboost
fit<-adaboost(target~.-id-target, data=train, nIter = 2) # nIter: 분류갯수
result<-predict(fit, newdata = test)
test$target<-result$class

write.csv(test[,c("id","target")], file="save_nIter=3.csv", row.names = FALSE)

#lightgbm
lgb_train<-lgb.Dataset(data = train, label=train$target)
categoricals_vec=colnames(train)[3:ncol(train)]

params = list(objective = "binary",
                metric = "auc",
                min_sum_hessian_in_leaf = 1,
                feature_fraction = 0.7,
                bagging_fraction = 0.7,
                bagging_freq = 5,
                min_data = 100,
                max_bin = 50,
                lambda_l1 = 8,
                lambda_l2 = 1.3,
                min_data_in_bin=100,
                min_gain_to_split = 10,
                min_data_in_leaf = 30,
                is_unbalance = TRUE)



# lgb_model_cv = lgb.cv(params = lgb_grid, data = lgb_train, learning_rate = 0.02, num_leaves = 25,
#                   num_threads = 2 , nrounds = 7000, early_stopping_rounds = 50,
#                   eval_freq = 20, eval = lgb_normalizedgini,
#                   categorical_feature = categoricals.vec, nfold = 5, stratified = TRUE)
model <- lgb.cv(params, lgb_train, 10, nfold=5, min_data=1, learning_rate=1, early_stopping_rounds=10)

best_iter = 525
lgb_model = lgb.train(params = lgb_grid, data = lgb_train, learning_rate = 0.02,
                      num_leaves = 25, num_threads = 2 , nrounds = best_iter,
                      eval_freq = 20, eval = lgb_normalizedgini,
                      categorical_feature = categoricals_vec)
lgb.normalizedgini()
