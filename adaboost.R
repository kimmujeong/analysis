library(dplyr)

train<-read.csv("santander-customer-transaction-prediction\\train.csv")
test<-read.csv("santander-customer-transaction-prediction\\test.csv")

head(train) #ID_code, target, var_0 ~ var_199

var<-paste("var_",0:199,sep="")
formula<-as.formula(paste("target~", paste(var, collapse = "+")))
formula

fit=adaboost(formula,data=train, nIter = 1)

############################################################################
############################################################################
library(fastAdaboost)
train<-read.csv("porto/train.csv")
test<-read.csv("porto/test.csv")
submisson<-read.csv("porto/sample_submission.csv")

head(train,100)
head(submisson) #id, target
head(test)

train2<-train[1:100000,] #adaboost 십만개부터 시간 조금 걸림 

fit<-adaboost(target~.-id-target, data=train, nIter = 2)
result<-predict(fit, newdata = test)
test$target<-result$class

print(fit)
get_tree(fit$trees)

write.csv(test[,c("id","target")], file="save3.csv", row.names = FALSE)


#############adabag 라이브러리
install.packages("adabag")
library(adabag)
train$target<-as.factor(train$target)
adabag_fit<-boosting(target~.-id-target,data=train,mfinal = 3)

