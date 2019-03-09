library(ggplot2)
library(randomForest)
train<-read.csv("C:\\Users\\thgus\\Downloads\\bike-sharing-demand\\train.csv",encoding="UTf-8")
test<-read.csv("C:\\Users\\thgus\\Downloads\\bike-sharing-demand\\test.csv",encoding="UTf-8")
head(train)

train$year<-substr(train$datetime,1,4) #그냥 바로 컬럼추가됨
train$month<-substr(train$datetime,6,7)
train$day<-substr(train$datetime,9,10)
train$time<-substr(train$datetime,12,13)
train$weekday<-as.integer(format(as.POSIXct(train$datetime,format="%Y-%m-%d %H:%M:%OS"),format="%u")) #요일 다시해보기
# %u 숫자 요일(1~7:월~일)

# head(train$year)
# head(train$month)
# head(train,n=30)

###############################################################################

#시간대별 대여분포
ggplot(train,aes(x=time,y=count))+geom_boxplot()

#+workinday
ggplot(train,aes(x=time,y=count))+geom_boxplot()+facet_grid(~workingday)+ggtitle("workingday")+theme(plot.title = element_text(hjust=0.5))

#+미등록자
ggplot(train,aes(x=time,y=casual))+geom_boxplot()

#+등록자
ggplot(train,aes(x=time,y=registered))+geom_boxplot()

###############################################################################

#요일별 대여분포
ggplot(train,aes(x=weekday))+geom_bar()

###############################################################################

#월별 대여분포
ggplot(train,aes(x=month,y=count))+geom_boxplot()

#+미등록자
ggplot(train,aes(x=month,y=casual))+geom_boxplot()

#+등록자
ggplot(train,aes(x=month,y=registered))+geom_boxplot()

###############################################################################
###############################################################################
###############################################################################
###############################################################################
#랜덤포레스트

formula=count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+year+month+weekday+time
model<-randomForest(formula,data=train)

#test 데이터전처리
test$year<-substr(test$datetime,1,4) #그냥 바로 컬럼추가됨
test$month<-substr(test$datetime,6,7)
test$day<-substr(test$datetime,9,10)
test$time<-substr(test$datetime,12,13)
test$weekday<-as.integer(format(as.POSIXct(test$datetime,format="%Y-%m-%d %H:%M:%OS"),format="%u"))

test$count<-predict(model,test)
write.csv(test[,c("datetime","count")],file="result.csv",row.names = FALSE)

#####향상 코드
train$casual.log<-log(train$casual+1)
train$registered.log<-log(train$registered+1)

casual.formula<-casual.log~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+year+month+weekday+time
registered.formula<-registered.log~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+year+month+weekday+time

casual.model<-randomForest(casual.formula,data=train)
registered.model<-randomForest(registered.formula,data=train)

test$casual.log<-predict(casual.model,test)
test$registered.log<-predict(registered.model,test)
test$count2<-exp(test$casual.log)+exp(test$registered.log)-2

head(test)
test<-subset(test,select = -c(count2)) #컬럼제거방법
head(test)

#향상된 count를 count2로 df에 저장했고 이를 그대로 write.csv로 하려했는데 컬럼이름지정이 안됨.
#write.csv자체 함수에서 이용할수없는거같고 따라서 write.table함수를 이용해야함
#write.csv(test[,c("datetime","count2")],file="result2.csv",row.names = FALSE,col.names = c("datetime","count")) 오류코드
write.table(test[,c("datetime","count2")],file="result2.csv",row.names = FALSE,col.names = c("datetime","count"),sep=",")
