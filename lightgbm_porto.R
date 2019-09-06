library(data.table)
library(Matrix)
library(dplyr)
library(MLmetrics) #cv
library(lightgbm)
library(ggplot2)

set.seed(257)
train<-read.csv("porto/train.csv")
test<-read.csv("porto/test.csv")
head(train)
######################
# #Pre Processing
# median.impute = function(x){
#   x = as.data.frame(x)
#   for (i in 1:ncol(x)){
#     x[which(x[,i]== -1),i] = NA
#   }
#   
#   x = x %>% mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% as.data.table()
#   return(x)
# }
# 
# train = median.impute(train)
# test  = median.impute(test)
######################

#Feature Engineering
test$target = NA
data = rbind(train, test)
#######################
# data[, fe_amount_NA := rowSums(data == -1, na.rm = T)]
# data[, ps_car_13_ps_reg_03 := ps_car_13*ps_reg_03]
# data[, ps_reg_mult := ps_reg_01*ps_reg_02*ps_reg_03]
######################

#LGB데이터셋으로 변환
varnames = setdiff(colnames(data), c("id", "target")) # setdiff:차집합 data컬럼이름에서 id, target 제외하고 가져오기

train_sparse = Matrix(as.matrix(train[,varnames]), sparse=TRUE) #sparse matrix 형태로 변환
test_sparse  = Matrix(as.matrix(test[,varnames]), sparse=TRUE) #sparse matrix 형태로 변환

y_train  = train[,c("target")]
test_ids = test[,c("id")]

lgb.train = lgb.Dataset(data=train_sparse, label=y_train)

categoricals.vec = colnames(train)[c(grep("cat",colnames(train)))]
# "ps_ind_02_cat" "ps_ind_04_cat" "ps_ind_05_cat" "ps_car_01_cat" "ps_car_02_cat" "ps_car_03_cat" "ps_car_04_cat"
# "ps_car_05_cat" "ps_car_06_cat" "ps_car_07_cat" "ps_car_08_cat" "ps_car_09_cat" "ps_car_10_cat" "ps_car_11_cat"


#파라미터 세팅 
lgb.grid = list(objective = "binary",
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

# Gini 평가함수 설정(Gini for Lgb)
lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}

#교차검증, Cross Validation
lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.02, num_leaves = 25,
                      num_threads = 2 , nrounds = 7000, early_stopping_rounds = 50, #nrounds:7000
                      eval_freq = 20, eval = lgb.normalizedgini,
                      categorical_feature = categoricals.vec, nfold = 5, stratified = TRUE)
best.iter = lgb.model.cv$best_iter #예제 525, 실제돌려보면 300~310부근나옴
best.iter=525

#모델생성 
lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.02,
                      num_leaves = 25, num_threads = 2 , nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini,
                      categorical_feature = categoricals.vec)

#변수중요도
tree_imp <- lgb.importance(lgb.model, percentage = TRUE)
lgb.plot.importance(tree_imp, top_n = 10, measure = "Gain") #lgb자체함수 
ggplot(tree_imp,aes(x=reorder(Feature, Gain),y=Gain))+geom_bar(stat="identity")+coord_flip() #ggplot표현 

#예측 및 결과파일 생성
result<-predict(lgb.model,test_sparse)
test$target<-result
write.csv(test[,c("id","target")], file="save_csv/save_lightgbm_best_iter.csv", row.names = FALSE)

############################## 
#           변수제거         #  
############################## 
select_var<-tree_imp[1:44,]$Feature
train2<-train %>%
  select(id,target,select_var)
test2<-test %>%
  select(id, select_var)
test2$target<-NA

train_sparse2 = Matrix(as.matrix(train[,select_var]), sparse=TRUE) #sparse matrix 형태로 변환
test_sparse2  = Matrix(as.matrix(test[,select_var]), sparse=TRUE) #sparse matrix 형태로 변환

y_train2  = train[,c("target")]
test_ids2 = test[,c("id")]

lgb.train2 = lgb.Dataset(data=train_sparse2, label=y_train2)

lgb.model2 = lgb.train(params = lgb.grid, data = lgb.train2, learning_rate = 0.02,
                      num_leaves = 25, num_threads = 2 , nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)

#변수중요도
tree_imp2 <- lgb.importance(lgb.model2, percentage = TRUE)
lgb.plot.importance(tree_imp2, top_n = 10, measure = "Gain") #lgb자체함수 
ggplot(tree_imp2,aes(x=reorder(Feature, Gain),y=Gain))+geom_bar(stat="identity")+coord_flip() #ggplot표현 

#예측 및 결과파일 생성
result<-predict(lgb.model2,test_sparse2)
test2$target<-result
write.csv(test2[,c("id","target")], file="save_csv/save_lightgbm_select_var.csv", row.names = FALSE)

##############################
#         파라미터탐색       #
##############################

grid_search <- expand.grid(bagging_fraction = c(0.4,0.6,0.7))

model <- list()
perf <- numeric(nrow(grid_search))

for (i in 1:nrow(grid_search)) {
  model[[i]] <- lgb.train(params=list(objective = "binary",
                               metric = "auc",
                               min_sum_hessian_in_leaf = 1,
                               feature_fraction = 0.7,
                               bagging_freq = 5,
                               min_data = 100,
                               max_bin = 50,
                               lambda_l1 = 8,
                               lambda_l2 = 1.3,
                               min_data_in_bin=100,
                               min_gain_to_split = 10,
                               min_data_in_leaf = 30,
                               is_unbalance = TRUE,
                               bagging_fraction =  grid_search[i, "bagging_fraction"]),
                          data = lgb.train, learning_rate = 0.02,
                          num_leaves = 25, num_threads = 2 , nrounds = 299,
                          eval_freq = 20)
  #perf[i] <- min(rbindlist(model[[i]]$record_evals$test$l2))
}
# grid_search
cat("Model ", which.min(perf), " is lowest loss: ", min(perf), sep = "","\n")
print(grid_search[which.min(perf), ])


##############################
#         표준화,scale       #
##############################
scale_train_var<-train %>%
  select(-c(id,target)) %>%
  scale()

scale_train_idtarget<-train[,c("id","target")]
scale_train<-cbind(scale_train_idtarget,scale_train_var)

scale_test_var<-test %>%
  select(-c(id,target)) %>%
  scale()

scale_test_idtarget<-test[,c("id","target")]
scale_test<-cbind(scale_test_idtarget,scale_test_var)

head(scale_train)
head(scale_test)

scale_test$target = NA

#Create LGB Dataset
scale_train_sparse = Matrix(as.matrix(scale_train[,varnames]), sparse=TRUE) #sparse matrix 형태로 변환
scale_test_sparse  = Matrix(as.matrix(scale_test[,varnames]), sparse=TRUE) #sparse matrix 형태로 변환

scale_y_train  = scale_train[,c("target")]
scale_test_ids = scale_test[,c("id")]

lgb.train = lgb.Dataset(data=scale_train_sparse, label=scale_y_train)


#Setting up LGBM Parameters
lgb.grid = list(objective = "binary",
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

# Setting up Gini Eval Function
# Gini for Lgb
lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}

#Cross Validation
#lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.02, num_leaves = 25,
#                   num_threads = 2 , nrounds = 7000, early_stopping_rounds = 50,
#                   eval_freq = 20, eval = lgb.normalizedgini,
#                   categorical_feature = categoricals.vec, nfold = 5, stratified = TRUE)

#best.iter = lgb.model.cv$best_iter
best.iter = 525

# Train final model
lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.02,
                      num_leaves = 25, num_threads = 2 , nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini,
                      categorical_feature = categoricals.vec)

#Create and Submit Predictions
result<-predict(lgb.model,scale_test_sparse)
scale_test$target<-result
write.csv(scale_test[,c("id","target")], file="save_csv/save_lightgbm_scale.csv", row.names = FALSE)

##############################
#           정규화           #
##############################
train_norm <- as.data.frame(apply(train[, 3:ncol(train)], 2, function(x) (x - min(x))/(max(x)-min(x))))
new_train<-cbind(train[,c("id","target")],train_norm)

test_norm<-as.data.frame(apply(test[, 2:ncol(test)], 2, function(x) (x - min(x))/(max(x)-min(x))))
id<-test$id
new_test<-cbind(id,test_norm)

new_test$target = NA

#Create LGB Dataset
new_train_sparse = Matrix(as.matrix(new_train[,varnames]), sparse=TRUE) #sparse matrix 형태로 변환
new_test_sparse  = Matrix(as.matrix(new_test[,varnames]), sparse=TRUE) #sparse matrix 형태로 변환

new_y_train  = new_train[,c("target")]
new_test_ids = new_test[,c("id")]

lgb.train = lgb.Dataset(data=new_train_sparse, label=new_y_train)


#Setting up LGBM Parameters
lgb.grid = list(objective = "binary",
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

# Setting up Gini Eval Function
# Gini for Lgb
lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}

#Cross Validation
#lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.02, num_leaves = 25,
#                   num_threads = 2 , nrounds = 7000, early_stopping_rounds = 50,
#                   eval_freq = 20, eval = lgb.normalizedgini,
#                   categorical_feature = categoricals.vec, nfold = 5, stratified = TRUE)

#best.iter = lgb.model.cv$best_iter
best.iter = 525

# Train final model
lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.02,
                      num_leaves = 25, num_threads = 2 , nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini,
                      categorical_feature = categoricals.vec)

#Create and Submit Predictions
result<-predict(lgb.model,new_test_sparse)
new_test$target<-result
write.csv(new_test[,c("id","target")], file="save_csv/save_lightgbm_normalize.csv", row.names = FALSE)


