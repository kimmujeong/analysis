library(ggplot2)
library(randomForest)
library(mlr)
bike_train<-read.csv("C:\\Users\\thgus\\Downloads\\bike-sharing-demand\\train.csv",encoding="UTf-8")
bike_test<-read.csv("C:\\Users\\thgus\\Downloads\\bike-sharing-demand\\test.csv",encoding="UTf-8")
head(bike_train)
#샘플작업 bike_train_sample<-bike_train[sample(1:nrow(bike_train),1000),]
bike_train$year<-substr(bike_train$datetime,1,4) #그냥 바로 컬럼추가됨
bike_train$month<-substr(bike_train$datetime,6,7)
bike_train$day<-substr(bike_train$datetime,9,10)
bike_train$time<-substr(bike_train$datetime,12,13)
bike_train$weekday<-as.integer(format(as.POSIXct(bike_train$datetime,format="%Y-%m-%d %H:%M:%OS"),format="%u")) #요일 다시해보기
# %u 숫자 요일(1~7:월~일)

# head(bike_train$year)
# head(bike_train$month)
# head(bike_train,n=30)

###############################################################################

#시간대별 대여분포
ggplot(bike_train,aes(x=time,y=count))+geom_boxplot()

#+workinday
ggplot(bike_train,aes(x=time,y=count))+geom_boxplot()+facet_grid(~workingday)+ggtitle("workingday")+theme(plot.title = element_text(hjust=0.5))

#+미등록자
ggplot(bike_train,aes(x=time,y=casual))+geom_boxplot()

#+등록자
ggplot(bike_train,aes(x=time,y=registered))+geom_boxplot()

###############################################################################

#요일별 대여분포
ggplot(bike_train,aes(x=weekday))+geom_bar()

###############################################################################

#월별 대여분포
ggplot(bike_train,aes(x=month,y=count))+geom_boxplot()

#+미등록자
ggplot(bike_train,aes(x=month,y=casual))+geom_boxplot()

#+등록자
ggplot(bike_train,aes(x=month,y=registered))+geom_boxplot()

###############################################################################
###############################################################################
###############################################################################
###############################################################################
#랜덤포레스트

formula=count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+year+month+weekday+time
model<-randomForest(formula,data=bike_train)

#bike_test 데이터전처리
bike_test$year<-substr(bike_test$datetime,1,4) #그냥 바로 컬럼추가됨
bike_test$month<-substr(bike_test$datetime,6,7)
bike_test$day<-substr(bike_test$datetime,9,10)
bike_test$time<-substr(bike_test$datetime,12,13)
bike_test$weekday<-as.integer(format(as.POSIXct(bike_test$datetime,format="%Y-%m-%d %H:%M:%OS"),format="%u"))

bike_test$count<-predict(model,bike_test)
write.csv(bike_test[,c("datetime","count")],file="result.csv",row.names = FALSE)

#####향상 코드
bike_train$casual.log<-log(bike_train$casual+1)
bike_train$registered.log<-log(bike_train$registered+1)

casual.formula<-casual.log~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+year+month+weekday+time
registered.formula<-registered.log~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+year+month+weekday+time

casual.model<-randomForest(casual.formula,data=bike_train)
registered.model<-randomForest(registered.formula,data=bike_train)

bike_test$casual.log<-predict(casual.model,bike_test)
bike_test$registered.log<-predict(registered.model,bike_test)
bike_test$count2<-exp(bike_test$casual.log)+exp(bike_test$registered.log)-2

head(bike_test)
bike_test<-subset(bike_test,select = -c(count2)) #컬럼제거방법
head(bike_test)

#향상된 count를 count2로 df에 저장했고 이를 그대로 write.csv로 하려했는데 컬럼이름지정이 안됨.
#write.csv자체 함수에서 이용할수없는거같고 따라서 write.table함수를 이용해야함
#write.csv(bike_test[,c("datetime","count2")],file="result2.csv",row.names = FALSE,col.names = c("datetime","count")) 오류코드
#write.table 할때는 sep 꼭해야함
write.table(bike_test[,c("datetime","count2")],file="result2.csv",row.names = FALSE,col.names = c("datetime","count"),sep=",")
write.table(bike_test[,c("datetime","count2")],file="result2.csv",row.names = FALSE,col.names = c("datetime","count"),sep=",")

#measuring Root Mean Square Error(RMSE)
rmse<-function(data,predict,ground.truth){
  N<-nrow(data)
  err<-(data[,predict]-data[,ground.truth])^2
  sqrt(sum(err))/N
}

#data permutation
permutate<-function(data){
  size<-nrow(data)
  shuffling<-sample(1:size,size)
  data[shuffling,]
}

#cross-validation
cv<-function(formula,data,FUN,k=5){
  temp<-permutate(data)
  N<-nrow(data)
  unit<-N/k
  scores<-vector(length=k)
  for(i in 1:k){
    from<-unit*(i-1)
    to<-from+unit
    train<-data[-(from:to),]
    test<-data[from:to,]
    model<-FUN(formula,train)
    test$pre<-predict(model,test)
    scores[i]<-rmse(test,'pre',as.character(formula)[2])
  }
  scores
}
result<-cv(formula,train,FUN=randomForest,k=5)
result
mean(result)

#############파라미터 튜닝
getParamSet("regr.randomForest")
rf <- makeLearner("regr.randomForest", predict.type = "response")
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 0, upper = 500),
  makeIntegerParam("nodesize", lower = 1, upper = 50)
)
rancontrol <- makeTuneControlRandom(maxit = 100L)
set_cv <- makeResampleDesc("CV",iters = 3L)
new_bike_train<-subset(bike_train,select = -c(datetime))
new_bike_train$year<-as.numeric(new_bike_train$year)
new_bike_train$month<-as.numeric(new_bike_train$month)
new_bike_train$day<-as.numeric(new_bike_train$day)
new_bike_train$time<-as.numeric(new_bike_train$time)
bike_trainTask<-makeRegrTask(data=new_bike_train,target = "count")
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = bike_trainTask, par.set = rf_param, control = rancontrol, measures = rmse)
########################################

str(new_bike_train)

casual.model<-randomForest(casual.formula,data=bike_train,ntree=460,nodesize=6)
registered.model<-randomForest(registered.formula,data=bike_train,ntree=460,nodesize=6)
str(bike_test)
bike_test
bike_test$casual.log<-predict(casual.model,bike_test)
bike_test$registered.log<-predict(registered.model,bike_test)
bike_test$count<-exp(bike_test$casual.log)+exp(bike_test$registered.log)-2
write.csv(bike_test[,c("datetime","count")],file="result4_param.csv",row.names = FALSE)
q()
