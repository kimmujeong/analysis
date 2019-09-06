#xgboost
library(randomForest)
library(dplyr)
library(xgboost)
library(ggplot2)
library(mlr) #파리미터튜닝
library(ggmap)
library(rBayesianOptimization)
library(MlBayesOpt)
library(corrplot)

home_train<-read.csv("C:\\Users\\thgus\\Downloads\\2019-2nd-ml-month-with-kakr\\train.csv")
home_test<-read.csv("C:\\Users\\thgus\\Downloads\\2019-2nd-ml-month-with-kakr\\test.csv")

#train파일 전처리
#정규화
home_train$bedrooms<-log1p(home_train$bedrooms)
home_train$sqft_living<-log1p(home_train$sqft_living)
home_train$sqft_lot<-log1p(home_train$sqft_lot)
home_train$sqft_above<-log1p(home_train$sqft_above)
home_train$sqft_basement<-log1p(home_train$sqft_basement)
home_train<-subset(home_train,select = -c(date,floors))

#새로운 변수작업
home_train<-home_train %>%
  mutate(yr_diff=yr_renovated-yr_built) %>%
  mutate(sqft_living_diff=sqft_living15-sqft_living) %>%
  mutate(allgrade=grade+waterfront+view+condition) %>%
  mutate(grade_waterfront=grade+waterfront) %>%
  mutate(grade_view=grade+view) %>%
  mutate(grade_waterfront_view=grade+waterfront+view) %>%
  mutate(grade_waterfront_condition=grade+waterfront+condition)

#test파일 전처리(train 파일과 동일한 작업)
home_test$bedrooms<-log1p(home_test$bedrooms)
home_test$sqft_living<-log1p(home_test$sqft_living)
home_test$sqft_lot<-log1p(home_test$sqft_lot)
home_test$sqft_above<-log1p(home_test$sqft_above)
home_test$sqft_basement<-log1p(home_test$sqft_basement)
home_test<-subset(home_test,select = -c(date,floors))

home_test<-home_test %>%
  mutate(yr_diff=yr_renovated-yr_built) %>%
  mutate(sqft_living_diff=sqft_living15-sqft_living) %>%
  mutate(allgrade=grade+waterfront+view+condition) %>%
  mutate(grade_waterfront=grade+waterfront) %>%
  mutate(grade_view=grade+view) %>%
  mutate(grade_waterfront_view=grade+waterfront+view) %>%
  mutate(grade_waterfront_condition=grade+waterfront+condition)

home_train<-lapply(home_train, as.numeric)
home_test<-lapply(home_test,as.numeric)

#파라미터값 탐색
getParamSet("regr.xgboost")
xg_set <- makeLearner("regr.xgboost", predict.type = "response")
xg_set$par.vals <- list(
  objective = "reg:linear",
  eval_metric = "rmse"
)
xg_ps <- makeParamSet(
  makeIntegerParam("nrounds",lower=600,upper=1000), 
  makeIntegerParam("max_depth",lower=6,upper=15),
  makeNumericParam("eta", lower = 0.01, upper = 0.1), 
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
)
rancontrol <- makeTuneControlRandom(maxit = 10L) 
set_cv <- makeResampleDesc("CV",iters = 3L)
home_train<-subset(home_train,select=-c(id))
trainTask <- makeRegrTask(data = home_train,target = "price")
xg_tune <- tuneParams(learner = xg_set, task = trainTask, resampling = set_cv,measures = rmse, par.set = xg_ps, control = rancontrol)

#cross validation. 도출된 파라미터값을 토대로 지속적으로 비교시행하여 최적의 값 탐색
cv_train <- xgb.DMatrix(data = data.matrix(subset(home_train,select = -c(id,price))),label = home_train$price)
param_origin<-list(eta=도출값, subsample= 도출값, colsample_bytree= 도출값, max_depth= 도출값, eval_metric="rmse", objective="reg:linear")
set.seed(1)
temp<-xgb.cv(param_origin, cv_train, nrounds= 도출값, nfold= 도출값, metrics = {'rmse'})

#모델생성
xgb_train<-data.matrix(subset(home_train,select = -c(id,price)))
set.seed(1)
xgbmodel<-xgboost(data=xgb_train,
                  label=home_train$price,
                  eta = 0.0238,
                  nround = 932,
                  subsample = 0.636, #(0,1] default 1
                  colsample_bytree = 0.72,#(0,1] default 1
                  eval_metric = "rmse",
                  objective = "reg:linear",
                  #nthread = 3,
                  max_depth = 6)

#변수중요도 
var_importance<-xgb.importance(colnames(xgb_train),xgbmodel)
ggplot(data = var_importance, aes(x = reorder(Feature, Gain), y = Gain)) +geom_bar(stat = 'identity')+coord_flip()
xgb.plot.importance(var_importance) #xgb자체함수

#최종 예측 및 예측파일 생성
xgb_test<-data.matrix(subset(new_home_test, select = -c(id,price)))
new_home_test$price<-predict(xgbmodel, xgb_test)
write.csv(new_home_test[,c(“id”,”price”)],file=“home_xgb.csv",row.names = FALSE)