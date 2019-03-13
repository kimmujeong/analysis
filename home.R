library(randomForest)
library(dplyr)
library(xgboost)
library(ggplot2)
home_train<-read.csv("C:\\Users\\thgus\\Downloads\\2019-2nd-ml-month-with-kakr\\train.csv")
home_test<-read.csv("C:\\Users\\thgus\\Downloads\\2019-2nd-ml-month-with-kakr\\test.csv")

sum(is.na(home_train))
head(home_train)
colnames(home_train)
str(home_train)
head(home_train$date,20)

#home_train 날짜 전처리
home_train$year<-as.numeric(substr(home_train$date,1,4))
home_train$month<-as.numeric(substr(home_train$date,5,6))
home_train$day<-as.numeric(substr(home_train$date,7,8))

#home_test 날짜 전처리
home_test
home_test$year<-as.numeric(substr(home_test$date,1,4))
home_test$month<-as.numeric(substr(home_test$date,5,6))
home_test$day<-as.numeric(substr(home_test$date,7,8))
home_test2<-subset(home_test,select = -c(date))
new_home_test<-home_test2 #계속 연습에 쓰일 new_home_test


#########################################################################################################
#########################################################################################################v
#####################################################랜덤포레스트
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
########################################xgb
xgb_train<-data.matrix(subset(home_train,select = -c(id,date,price)))
home_xgb<-xgboost(data=xgb_train,
                  label=home_train$price,
                  eta = 0.2, #gradient descent 알고리즘에서의 learning rate
                  nround = 600, #maximum number of iterations (steps) required for gradient descent to converge
                  subsample = 0.8,
                  colsample_bytree = 0.8,
                  seed = 1,
                  eval_metric = "rmse", #회귀모델에서는 RMSE를 모델 accuracy 평가지표로 활용
                  objective = "reg:linear",
                  nthread = 3,
                  max_depth = 5)

xgb_test<-data.matrix(subset(new_home_test, select = -c(id)))
new_home_test$price<-predict(home_xgb, xgb_test)

write.csv(new_home_test[,c("id","price")],file="home_xgb_rmse.csv",row.names = FALSE)

#변수중요도 
var_importance<-xgb.importance(colnames(xgb_train),home_xgb)
ggplot(data = var_importance, aes(x = reorder(Feature, Gain), y = Gain)) +geom_bar(stat = 'identity')+coord_flip()
xgb.plot.importance(var_importance) #xgb자체함수


#cv, 241이 나왓는데 결과가 더 안좋아짐 
xgb_train_ex<-data.matrix(subset(home_train,select = -c(id,date,price)))
dtrain <- xgb.DMatrix(data = xgb_train_ex,label = home_train$price)
params <- list(booster = "gbtree", eta = 0.2, subsample = 0.8, colsample_bytree = 0.8, seed = 1, eval_metric = "rmse", objective = "reg:linear", nthread = 3, max_depth = 5)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 600, nfold = 5, showsd = T, stratified = T, print.every_n = 10, early_stopping_rounds = 20, maximize = F)







"
predict할때 컬럼이 맞아야 
xgb모델에서 eval_metric을 mae로 했었다가 rmse로 바꾸니 훨씬 좋아졌다. 맞는 방법으로 해야된다..
"