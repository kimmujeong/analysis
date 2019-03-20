library(e1071) #svm
otto_train<-read.csv("C:\\Users\\thgus\\Downloads\\otto-group-product-classification-challenge\\train.csv")
otto_test<-read.csv("C:\\Users\\thgus\\Downloads\\otto-group-product-classification-challenge\\test.csv")
head(otto_train)
head(otto_test)
otto_train$target #Class_1 ~ Class_9
str(otto_train)
sum(is.na(otto_train))

otto_train<-subset(otto_train,select = -c(id))
otto_test<-subset(otto_test,select = -c(id))

svm_model<-svm(target~., data=otto_train)
pred<-predict(svm_model,otto_test)
otto_test$target<-pred

#svm_tune<-tune(svm, target~., data=otto_train, gamma=10^(-2:1), cost=10^(-2:1)) 잘못 구분한 점으로 인한 cost를 최소화
svm_tune<-tune.svm(target~., data=otto_train, gamma=10^(-2:1), cost=10^(-2:1)) #잘못 구분한 점으로 인한 cost를 최소화
#gamma와 cost는 항상 양수
#cost값이 크면 train에서는 높은 예측력을 보여줄 수 있지만 test에서는 예측력이 떨어질 수 있음. overfitting
#cost값이 작으면 과소적합을 초래할 수있음. 과소적합(underfitting)=너무 간단한 모델이 선택되는 것





#################결과작업
id<-1:144368
col<-c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7"
       ,"Class_8","Class_9")
df<-data.frame(id)
df[,col]<-0
head(df)

for(i in 1:144368){
  if(otto_test[i,c("target")] %in% colnames(df)){
    colname<-otto_test[i,c("target")]
    colname<-as.character(colname)
    df[i,colname]<-1
  }
}
head(df)

write.csv(df,file="otto_submission.csv",row.names = FALSE)
' 변환작업 테스트
#df아이디하고 otto아이디하고 같을때 otto아이디의 타겟값의 컬럼을 1로 바꾼다
colname<-otto_test[1,c("target")]
colname<-as.character(colname)
typeof(colname)
df[1,colname]

if(otto_test[1,c("target")] %in% colnames(df)){
  colname<-otto_test[1,c("target")]
  colname<-as.character(colname)
  df[1,colname]<-1
}
df[1,]
'


