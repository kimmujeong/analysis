library(dplyr)
library(fastAdaboost)
train<-read.csv("santander-customer-transaction-prediction\\train.csv")
test<-read.csv("santander-customer-transaction-prediction\\test.csv")

head(train) #ID_code, target, var_0 ~ var_199

var<-paste("var_",0:199,sep="")
formula<-as.formula(paste("target~", paste(var, collapse = "+")))
formula

fit=adaboost(formula,data=train, nIter = 1)

############################################################################
############################################################################
train<-read.csv("porto/train.csv")
test<-read.csv("porto/test.csv")
submisson<-read.csv("porto/sample_submission.csv")

head(train,100)
head(submisson) #id, target
head(test)

train2<-train[1:100000,] #adaboost 십만개부터 시간 조금 걸림 

fit<-adaboost(target~.-id-target, data=train, nIter = 2)
predict_result<-predict(fit, newdata = test)

test$target<-predict_result$class

test$id
write.csv(test[,c("id","target")], file="save3.csv", row.names = FALSE)

save<-read.csv("save.csv")
head(save,100)
