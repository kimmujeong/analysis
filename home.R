library(randomForest)
library(dplyr)
library(xgboost)
library(ggplot2)
library(mlr) #파리미터튜닝
library(ggmap)
library(rBayesianOptimization)
library(MlBayesOpt)
library(corrplot)
#install.packages("corrplot")
#install.packages("ggmap")
install.packages("tibble")
home_train<-read.csv("C:\\Users\\thgus\\Downloads\\2019-2nd-ml-month-with-kakr\\train.csv")
home_test<-read.csv("C:\\Users\\thgus\\Downloads\\2019-2nd-ml-month-with-kakr\\test.csv")
rm(list=ls())

#상관관계 
home_train<-home_train %>%
  select(-c(id,date))
home_train_cor<-cor(home_train)
corrplot(home_train_cor, method="color")
home_train_cor["price",]
cor_df<-setNames(data.frame(home_train_cor["price",]),c("cor"))
cor_df_order<-cor_df[order(cor_df$cor,decreasing = T), ,drop=FALSE]
view(cor_df_order)

#구조확인
# sum(is.na(home_train))
# head(home_train)
# colnames(home_train)
# str(home_train)
# head(home_train$date,20)
# head(home_train$date)

# home_train 날짜 전처리
home_train$year<-as.numeric(substr(home_train$date,1,4))
home_train$month<-as.numeric(substr(home_train$date,5,6))
home_train$day<-as.numeric(substr(home_train$date,7,8))
home_train$weekday<-as.integer(format(as.POSIXct(home_train$date,format="%Y%m%dT000000"),format="%u"))
home_train<-subset(home_train,select = -c(date,floors))


#새로운 변수작업 
home_train<-home_train %>%
  mutate(yr_renovated_yn=ifelse(yr_renovated!=0,1,0)) %>%
  # mutate(sqft_basement_yn=ifelse(sqft_basement!=0,1,0)) %>%
  # mutate(allbathrooms=bedrooms*bathrooms) %>%
  # mutate(temp=sqft_living/bedrooms) %>%
  mutate(yr_diff=yr_renovated-yr_built) %>%
  # mutate(sqft_living_diff=sqft_living15-sqft_living) %>%
  # mutate(sqft_lot_diff=sqft_lot15-sqft_lot) %>%
  # mutate(allgrade=grade+waterfront+view+condition) %>%
  # mutate(grade_waterfront=grade+waterfront) %>%
  # mutate(grade_view=grade+view) %>%
  # mutate(grade_condition=grade+condition) %>%
  # mutate(waterfront_view=waterfront+view) %>%
  # mutate(waterfront_condition=waterfront+condition) %>%
  # mutate(view_condition=view+condition) %>%
  # mutate(grade_waterfront_view=grade+waterfront+view) %>%
  mutate(grade_waterfront_condition=grade+waterfront+condition)
  # mutate(waterfront_view_condition=waterfront+view+condition)



#############################################################
home_test #날짜 전처리
home_test$year<-as.numeric(substr(home_test$date,1,4))
home_test$month<-as.numeric(substr(home_test$date,5,6))
home_test$day<-as.numeric(substr(home_test$date,7,8))
home_test$weekday<-as.integer(format(as.POSIXct(home_test$date,format="%Y%m%dT000000"),format="%u"))
home_test<-subset(home_test,select = -c(date,floors))


home_test<-home_test %>%
  mutate(sqft_basement_yn=ifelse(sqft_basement!=0,1,0)) %>%
  mutate(allbathrooms=bedrooms*bathrooms) %>%
  mutate(temp=sqft_living/bedrooms) %>%
  mutate(yr_diff=yr_renovated-yr_built) %>% 
  mutate(sqft_living_diff=sqft_living15-sqft_living) %>%
  mutate(sqft_lot_diff=sqft_lot15-sqft_lot) %>%
  mutate(allgrade=grade+waterfront+view+condition) %>%
  #mutate(grade_waterfront=grade+waterfront) %>%
  #mutate(grade_view=grade+view) %>%
  #mutate(grade_condition=grade+condition) %>%
  #mutate(waterfront_view=waterfront+view) %>%
  #mutate(waterfront_condition=waterfront+condition) %>%
  #mutate(view_condition=view+condition) %>%
  #mutate(grade_waterfront_view=grade+waterfront+view) %>%
  mutate(grade_waterfront_condition=grade+waterfront+condition)
  #mutate(waterfront_view_condition=waterfront+view+condition)


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
set.seed(1234)
xgbmodel<-xgboost(data=xgb_train,
                         label=home_train$price,
                         eta = 0.2, 
                         nround = 300, 
                         subsample = 1, #(0,1] default 1
                         colsample_bytree = 0.8,#(0,1] default 1
                         eval_metric = "rmse", 
                         objective = "reg:linear",
                         #nthread = 3, 
                         max_depth = 11)

var_importance<-xgb.importance(colnames(xgb_train),xgbmodel)
ggplot(data = var_importance, aes(x = reorder(Feature, Gain), y = Gain)) +geom_bar(stat = 'identity')+coord_flip()
xgb.plot.importance(var_importance)
#ggplot(data = var_importance, aes(x = reorder(Feature, Cover), y = Cover)) +geom_bar(stat = 'identity')+coord_flip()
options("scipen" = 100)
var_importance[order(var_importance$Gain,decreasing = T),]
var_importance$Frequency

#####xgboost cross validation
cv_train <- xgb.DMatrix(data = xgb_train,label = home_train$price)
param_origin<-list(eta=0.2, subsample=1, colsample_bytree=0.8, max_depth=11, eval_metric="rmse", objective="reg:linear")
set.seed(1)
temp<-xgb.cv(param_origin, cv_train, nrounds=30, nfold=5, metrics = {'rmse'})

#cv_train2<-xgb.DMatrix(data=subset(xgb_train,select = c(grade_waterfront_condition,sqft_living,lat)),label=home_train$price)

temp$best_iteration #early_stopping_rounds 줄때만가능
home_train$bathrooms

####################cv2####################################
cv_train2<-xgb.DMatrix(data=data.matrix(subset(home_train,select = c("grade","sqft_living","lat","sqft_above","long","sqft_living15",
                                                                    "view","yr_built","zipcode","waterfront","bathrooms","sqft_lot"))),label=home_train$price)
hist(home_train$waterfront)
set.seed(1)
xgb.cv(param_origin,cv_train2,nrounds = 1000,nfold = 5,metrics = {'rmse'},early_stopping_rounds = 700)
############################################################




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



xgb_test<-data.matrix(subset(new_home_test, select = -c(id)))
new_home_test$price<-predict(xgbmodel, xgb_test)
#head(new_home_test$price)
write.csv(new_home_test[,c("id","price")],file="home_xgb_rmse_0322.csv",row.names = FALSE)
#getParma2:eta = 0.104,nround = 600,subsample = 0.765, colsample_bytree = 0.652, seed = 1,eval_metric ="rmse" objective = "reg:linear",max_depth = 5 

#변수중요도 
var_importance<-xgb.importance(colnames(xgb_train),xgbmodel)
ggplot(data = var_importance, aes(x = reorder(Feature, Gain), y = Gain)) +geom_bar(stat = 'identity')+coord_flip()
xgb.plot.importance(var_importance) #xgb자체함수

'grade, sqft_living, lat, sqft_above, long, sqft_living15, view, yr_built, zipcode(?), waterfront
정도까지 의미있는 것으로 파악됨
'

#변수중요도 토대로 다시 xgboost
home_train_modify<-subset(home_train,select = c("grade","sqft_living","lat","sqft_above","long","sqft_living15",
                                                "view","yr_built","zipcode","waterfront","bathrooms","sqft_lot"))
xgb_train_modify<-data.matrix(home_train_modify)
xgbmodel_modify<-xgboost(data=xgb_train_modify,
                          label = home_train$price,
                          eta = 0.2,
                          nround = 600, 
                          subsample = 0.8, 
                          colsample_bytree = 0.8, 
                          seed = 1,
                          eval_metric = "rmse", 
                          objective = "reg:linear",
                          #nthread = 3, 
                          max_depth = 9)

xgb_test_modify<-data.matrix(subset(new_home_test, select = c(grade, sqft_living, lat, sqft_above, long, sqft_living15, view, yr_built, zipcode, waterfront,bathrooms,sqft_lot)))
new_home_test$price<-predict(xgbmodel_modify, xgb_test_modify)
write.csv(new_home_test[,c("id","price")],file="home_xgb_rmse_modify2_getParam2.csv",row.names = FALSE)


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
  makeNumericParam("subsample", lower = 0, upper = 1),
  makeNumericParam("colsample_bytree",lower = 0,upper = 1)
)
rancontrol <- makeTuneControlRandom(maxit = 30L) #40L부터 시간좀걸림
set_cv <- makeResampleDesc("CV",iters = 3L)
home_train<-subset(home_train,select = -c(date))
trainTask <- makeRegrTask(data = home_train,target = "price")
#trainTask <- normalizeFeatures(trainTask,method = "standardize")
set.seed(1234)
xg_tune <- tuneParams(learner = xg_set, task = trainTask, resampling = set_cv,measures = rmse, par.set = xg_ps, control = rancontrol)




####변수재지정 파라미터설정
home_train_modify_temp<-subset(home_train,select = c("grade","sqft_living","lat","sqft_above","long","sqft_living15",
                                                "view","yr_built","zipcode","waterfront","price"))
trainTask_modify<-makeRegrTask(data=home_train_modify_temp,target = "price")
xg_tune <- tuneParams(learner = xg_set, task = trainTask_modify, resampling = set_cv,measures = rmse, par.set = xg_ps, control = rancontrol)
"
[Tune-y] 30: rmse.test.rmse=154751.4441560; time: 0.2 min
[Tune] Result: nrounds=569; max_depth=7; eta=0.074; subsample=0.666; colsample_bytree=0.468 : rmse.test.rmse=125862.8476825
"








'
https://3months.tistory.com/118
validation 하기...
'



##################################################################################################
##################################################################################################
#중요변수(3~4개)를 다 합쳐보기


"
predict할때 컬럼이 맞아야 
xgb모델에서 eval_metric을 mae로 했었다가 rmse로 바꾸니 훨씬 좋아졌다. 맞는 방법으로 해야된다..
"