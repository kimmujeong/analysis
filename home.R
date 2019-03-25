library(randomForest)
library(dplyr)
library(xgboost)
library(ggplot2)
library(mlr) #파리미터튜닝
library(rBayesianOptimization)
library(MlBayesOpt)
library(corrplot)
#install.packages("corrplot")
#install.packages("ggmap")
home_train<-read.csv("C:\\Users\\thgus\\Downloads\\2019-2nd-ml-month-with-kakr\\train.csv")
home_test<-read.csv("C:\\Users\\thgus\\Downloads\\2019-2nd-ml-month-with-kakr\\test.csv")
rm(list=ls())

################################################
########################상관관계###############
# home_train<-home_train %>%
#   select(-c(id,date))
# home_train_cor<-cor(home_train)
# corrplot(home_train_cor, method="color")
# home_train_cor["price",]
# cor_df<-setNames(data.frame(home_train_cor["price",]),c("cor"))
# cor_df_order<-cor_df[order(cor_df$cor,decreasing = T), ,drop=FALSE]
# view(cor_df_order)

#구조확인
# sum(is.na(home_train))
# head(home_train)
# colnames(home_train)
# str(home_train)
# head(home_train$date,20)
# head(home_train$date)
################################################

#정규화
home_train$bedrooms<-log1p(home_train$bedrooms)
home_train$sqft_living<-log1p(home_train$sqft_living)
home_train$sqft_lot<-log1p(home_train$sqft_lot)
home_train$sqft_above<-log1p(home_train$sqft_above)
home_train$sqft_basement<-log1p(home_train$sqft_basement)

# home_train 날짜 전처리
home_train$year<-as.numeric(substr(home_train$date,1,4))
home_train$month<-as.numeric(substr(home_train$date,5,6))
home_train$day<-as.numeric(substr(home_train$date,7,8))
home_train$weekday<-as.integer(format(as.POSIXct(home_train$date,format="%Y%m%dT000000"),format="%u"))
home_train<-subset(home_train,select = -c(date,floors))


#새로운 변수작업 
home_train<-home_train %>%
  mutate(yr_diff=yr_renovated-yr_built) %>%
  mutate(sqft_living_diff=sqft_living15-sqft_living) %>%
  mutate(sqft_lot_diff=sqft_lot15-sqft_lot) %>%
  mutate(allgrade=grade+waterfront+view+condition) %>%
  mutate(grade_waterfront=grade+waterfront) %>%
  mutate(grade_view=grade+view) %>%
  mutate(grade_condition=grade+condition) %>%
  mutate(waterfront_view=waterfront+view) %>%
  mutate(view_condition=view+condition) %>%
  mutate(grade_waterfront_view=grade+waterfront+view) %>%
  mutate(grade_waterfront_condition=grade+waterfront+condition)



#############################################################
home_test$bedrooms<-log1p(home_test$bedrooms)
home_test$sqft_living<-log1p(home_test$sqft_living)
home_test$sqft_lot<-log1p(home_test$sqft_lot)
home_test$sqft_above<-log1p(home_test$sqft_above)
home_test$sqft_basement<-log1p(home_test$sqft_basement)

#날짜 전처리
home_test$year<-as.numeric(substr(home_test$date,1,4))
home_test$month<-as.numeric(substr(home_test$date,5,6))
home_test$day<-as.numeric(substr(home_test$date,7,8))
home_test$weekday<-as.integer(format(as.POSIXct(home_test$date,format="%Y%m%dT000000"),format="%u"))
home_test<-subset(home_test,select = -c(date,floors))


home_test<-home_test %>%
  mutate(yr_diff=yr_renovated-yr_built) %>%
  mutate(sqft_living_diff=sqft_living15-sqft_living) %>%
  mutate(sqft_lot_diff=sqft_lot15-sqft_lot) %>%
  mutate(allgrade=grade+waterfront+view+condition) %>%
  mutate(grade_waterfront=grade+waterfront) %>%
  mutate(grade_view=grade+view) %>%
  mutate(grade_condition=grade+condition) %>%
  mutate(waterfront_view=waterfront+view) %>%
  mutate(view_condition=view+condition) %>%
  mutate(grade_waterfront_view=grade+waterfront+view) %>%
  mutate(grade_waterfront_condition=grade+waterfront+condition) %>%

new_home_test<-home_test #계속 연습에 쓰일 new_home_test


#########################################################################################################
#########################################################################################################
#####################################################랜덤포레스트########################################
formula<-price~id+bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15+year+month+day
model<-randomForest(formula,data=home_train)

home_test2$price<-predict(model,home_test2)
head(home_test2$price)
model$importance
varImpPlot(model)
write.csv(home_test2[,c("id","price")],file="home_randomForest1.csv",row.names = FALSE)

formula2<-price~sqft_living+grade+lat
model2<-randomForest(formula2,home_train)
home_test3<-home_test2
home_test3$price<-predict(model2,home_test3)
write.csv(home_test3[,c("id","price")],file="home_randomForest2.csv",row.names = FALSE)
#########################################################################################################
#########################################################################################################


#########################################################################################################
#########################################################################################################
########################################xgb########################################

################################################################################
#########################################as.num 작업 ###########################
home_train$bedrooms<-as.numeric(home_train$bedrooms)
home_train$sqft_living<-as.numeric(home_train$sqft_living)
home_train$sqft_lot<-as.numeric(home_train$sqft_lot)
home_train$waterfront<-as.numeric(home_train$waterfront)
home_train$view<-as.numeric(home_train$view)
home_train$condition<-as.numeric(home_train$condition)
home_train$grade<-as.numeric(home_train$grade)
home_train$sqft_above<-as.numeric(home_train$sqft_above)
home_train$sqft_basement<-as.numeric(home_train$sqft_basement)
home_train$yr_built<-as.numeric(home_train$yr_built)
home_train$yr_renovated<-as.numeric(home_train$yr_renovated)
home_train$zipcode<-as.numeric(home_train$zipcode)
home_train$sqft_living15<-as.numeric(home_train$sqft_living15)
home_train$sqft_lot15<-as.numeric(home_train$sqft_lot15)
home_train$weekday<-as.numeric(home_train$weekday)
str(home_train)

new_home_test$bedrooms<-as.numeric(new_home_test$bedrooms)
new_home_test$sqft_living<-as.numeric(new_home_test$sqft_living)
new_home_test$sqft_lot<-as.numeric(new_home_test$sqft_lot)
new_home_test$waterfront<-as.numeric(new_home_test$waterfront)
new_home_test$view<-as.numeric(new_home_test$view)
new_home_test$condition<-as.numeric(new_home_test$condition)
new_home_test$grade<-as.numeric(new_home_test$grade)
new_home_test$sqft_above<-as.numeric(new_home_test$sqft_above)
new_home_test$sqft_basement<-as.numeric(new_home_test$sqft_basement)
new_home_test$yr_built<-as.numeric(new_home_test$yr_built)
new_home_test$yr_renovated<-as.numeric(new_home_test$yr_renovated)
new_home_test$zipcode<-as.numeric(new_home_test$zipcode)
new_home_test$sqft_living15<-as.numeric(new_home_test$sqft_living15)
new_home_test$sqft_lot15<-as.numeric(new_home_test$sqft_lot15)
#lapply(new_home_test,as.numeric) 써보기 
str(new_home_test)
################################################################################

xgb_train<-data.matrix(subset(home_train,select = -c(id,price)))

#원본 train-rmse:32xxx.xxxx
set.seed(1)
xgbmodel<-xgboost(data=xgb_train,
                         label=home_train$price,
                         eta = 0.0564, 
                         nround = 649, 
                         subsample = 0.621, #(0,1] default 1
                         colsample_bytree = 0.925,#(0,1] default 1
                         eval_metric = "rmse", 
                         objective = "reg:linear",
                         #nthread = 3, 
                         max_depth = 9)

var_importance<-xgb.importance(colnames(xgb_train),xgbmodel)
ggplot(data = var_importance, aes(x = reorder(Feature, Gain), y = Gain)) +geom_bar(stat = 'identity')+coord_flip()
xgb.plot.importance(var_importance)
#ggplot(data = var_importance, aes(x = reorder(Feature, Cover), y = Cover)) +geom_bar(stat = 'identity')+coord_flip()
options("scipen" = 100)
var_importance[order(var_importance$Gain,decreasing = T),]
var_importance$Frequency

#####xgboost cross validation
cv_train <- xgb.DMatrix(data = xgb_train,label = home_train$price)
param_origin<-list(eta=0.0564, subsample=0.621, colsample_bytree=0.925, max_depth=9, eval_metric="rmse", objective="reg:linear")
set.seed(1)
temp<-xgb.cv(param_origin, cv_train, nrounds=649, nfold=5, metrics = {'rmse'})
temp$best_iteration #early_stopping_rounds 줄때만가능

'
all변수(새로추가한거)
[Tune] Result: nrounds=793; max_depth=12; eta=0.0279; subsample=0.384; colsample_bytree=0.578 : rmse.test.rmse=127174.7663617
변수제거후
[Tune] Result: nrounds=712; max_depth=6; eta=0.0271; subsample=0.621; colsample_bytree=0.411 : rmse.test.rmse=122431.5425037
변수제거후2
[Tune] Result: nrounds=717; max_depth=6; eta=0.0234; subsample=0.34; colsample_bytree=0.509 : rmse.test.rmse=129140.1565169
'


########################
#####rmse 비교하기######
cv_train <- xgb.DMatrix(data = data.matrix(subset(home_train,select = -c(id,price))),label = home_train$price)
param_origin<-list(eta=0.0279, subsample=0.384, colsample_bytree=0.578, max_depth=12, eval_metric="rmse", objective="reg:linear")
set.seed(1)
temp<-xgb.cv(param_origin, cv_train, nrounds=793, nfold=5, metrics = {'rmse'})
#[793]	train-rmse:17889.676953+1236.593746	test-rmse:126051.956250+10985.884124
#제출결과 117652.86935


cv_train <- xgb.DMatrix(data = data.matrix(subset(home_train,select = -c(id,price,temp,yr_renovated_yn,waterfront_condition,sqft_basement_yn,year,waterfront_view_condition,view_condition))),label = home_train$price)
param_origin<-list(eta=0.0271, subsample=0.621, colsample_bytree=0.411, max_depth=6, eval_metric="rmse", objective="reg:linear")
set.seed(1)
temp<-xgb.cv(param_origin, cv_train, nrounds=712, nfold=5, metrics = {'rmse'})
#param_origin<-list(eta=0.0271, subsample=0.621, colsample_bytree=0.411, max_depth=6, eval_metric="rmse", objective="reg:linear")
#[712]	train-rmse:59035.739844+510.934671	test-rmse:123408.318750+11218.019143 
#제출결과 114644.16264

cv_train <- xgb.DMatrix(data = data.matrix(subset(home_train,select = -c(id,price,temp,yr_renovated_yn,waterfront_condition,sqft_basement_yn,year,waterfront_view_condition,view_condition,condition,weekday,yr_renovated,view,waterfront,bedrooms,day,month,sqft_lot_diff,waterfront_view))),label = home_train$price)
param_origin<-list(eta=0.0234, subsample=0.34, colsample_bytree=0.509, max_depth=6, eval_metric="rmse", objective="reg:linear")
set.seed(1)
temp<-xgb.cv(param_origin, cv_train, nrounds=717, nfold=5, metrics = {'rmse'})
#[717]	train-rmse:77620.051562+1371.319299	test-rmse:131382.737500+9469.940710 
#제출결과 118656.08708

#[649]	train-rmse:10935.682031+660.460854	test-rmse:126627.279688+15998.074166 
#제출결과 114361.07623

#영향력 낮은 변수제거 후... 제출파일은 안만들었음(시간오버) 
#[649]	train-rmse:10959.287110+457.780943	test-rmse:125801.221875+13574.826347
######################

##########################################
xgbmodel<-xgboost(data=xgb_train,
                  label=home_train$price,
                  eta = 0.104, #gradient descent 알고리즘에서의 learning rate 0.2
                  nround = 600, #maximum number of iterations (steps) required for gradient descent to converge 600
                  subsample = 0.765, #0.8
                  colsample_bytree = 0.652, #0.8
                  seed = 1,
                  eval_metric = "rmse", #회귀모델에서는 RMSE를 모델 accuracy 평가지표로 활용
                  objective = "reg:linear",
                  #nthread = 3, 동시 처리 수 이며, 시스템의 코어 수에 대응하여 입력되어야 한다. 모든 코어를 사용하려면, 값을 할당하지 않는다(알고리즘 감지한다).
                  max_depth = 5) #5
##########################################

xgb_test<-data.matrix(subset(new_home_test, select = -c(id)))
new_home_test$price<-predict(xgbmodel, xgb_test)
write.csv(new_home_test[,c("id","price")],file="home_xgb_rmse_log.csv",row.names = FALSE)

#변수중요도 
var_importance<-xgb.importance(colnames(xgb_train),xgbmodel)
ggplot(data = var_importance, aes(x = reorder(Feature, Gain), y = Gain)) +geom_bar(stat = 'identity')+coord_flip()
xgb.plot.importance(var_importance) #xgb자체함수





#############cv, 241이 나왓는데 결과가 더 안좋아짐 ###################
xgb_train_ex<-data.matrix(subset(home_train,select = -c(id,price)))
dtrain <- xgb.DMatrix(data = xgb_train_ex,label = home_train$price)
param <- list(eta = 0.2, subsample = 0.8, colsample_bytree = 0.8, seed = 1, eval_metric = "rmse", objective = "reg:linear", max_depth = 5)
######################################################################


##################################################################################################
################################################parameter1########################################
#xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 600, nfold = 5, showsd = T, stratified = T, print.every_n = 10, early_stopping_rounds = 20, maximize = F)
cv.nround=1000
cv.nfold=5
mdcv<-xgb.cv(data=dtrain, params = param, nfold=cv.nfold, nrounds = cv.nround, verbose = T)

min_rmse=min(mdcv$evaluation_log[, test_rmse_mean])
min_rmse_index = which.min(mdcv$evaluation_log[, test_rmse_mean])
min_rmse_index
##################################################################################################
##################################################################################################



##################################################################################################
#################################################parameter2#######################################
#https://datascience.stackexchange.com/questions/9364/hypertuning-xgboost-parameters

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.75, 1),
                                colsample_bytree = c(0.6, 0.8, 1))
ntrees <- 10

#Build a xgb.DMatrix object
xgb_train2<-data.matrix(subset(home_train,select = -c(id)))
DMMatrixTrain <- xgb.DMatrix(data = xgb_train2, label = home_train$price)

rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  
  xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = ntrees, nfold = 5, showsd = TRUE, 
                           metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                           "objective" = "reg:linear", "max.depth" = 15, "eta" = 2/ntrees,                               
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
  
  rmse <- tail(xgboostModelCV$evaluation_log[, test_rmse_mean], 1)
  
  return(c(rmse, currentSubsampleRate, currentColsampleRate))
  
})
rmseErrorsHyperparameters
##################################################################################################
##################################################################################################

##################################################################################################
#################################################parameter3#######################################
getParamSet("regr.xgboost")
xg_set <- makeLearner("regr.xgboost", predict.type = "response")
xg_set$par.vals <- list(
  objective = "reg:linear",
  eval_metric = "rmse"
)
xg_ps <- makeParamSet(
  makeIntegerParam("nrounds",lower=600,upper=1000), #그냥 고정값으로 하는게 나을거같기도 함 
  makeIntegerParam("max_depth",lower=6,upper=15), #maxdepth는 높이는게 좋을거같다. 
  makeNumericParam("eta", lower = 0.01, upper = 0.1), #eta를 낮추는게 좋을거같다.
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
)
rancontrol <- makeTuneControlRandom(maxit = 10L) #40L부터 시간좀걸림
set_cv <- makeResampleDesc("CV",iters = 3L)
home_train<-subset(home_train,select=-c(id))
trainTask <- makeRegrTask(data = home_train,target = "price")
#trainTask <- normalizeFeatures(trainTask,method = "standardize")
set.seed(1)
xg_tune <- tuneParams(learner = xg_set, task = trainTask, resampling = set_cv,measures = rmse, par.set = xg_ps, control = rancontrol)









##################################################################################################
##################################################################################################
"

https://3months.tistory.com/118
validation 하기...

중요변수(3~4개)를 다 합쳐보기

predict할때 컬럼이 맞아야
xgb모델에서 eval_metric을 mae로 했었다가 rmse로 바꾸니 훨씬 좋아졌다. 맞는 방법으로 해야된다..

"