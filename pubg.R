library(dplyr)
library(ggplot2)
library(xgboost)
pubg_train<-read.csv("C:\\Users\\thgus\\Downloads\\pubg-finish-placement-prediction\\train_V2.csv")
pubg_test<-read.csv("C:\\Users\\thgus\\Downloads\\pubg-finish-placement-prediction\\test_V2.csv")
save(pubg_train,file="./pubg_train.RData")

# head(pubg_train,20)
# head(pubg_train[order(pubg_train$groupId,decreasing = T),])
# str(pubg_train)
# nrow(pubg_train)
# 
# pubg_train %>%
#   count(pubg_train$groupId)

#pubg_train$groupId
#pubg_train[!duplicated(pubg_train$groupId),]
#distinct(pubg_train,groupId)

# rankPoints -1의 경우 None, 제외
# rankPoints의 값이 -1이 아닌 경우(rankPoints의 의미있는 경우) -> killPoints가 0이라면 None, 제외
# rankPoints의 값이 -1이 아닌 경우(rankPoints의 의미있는 경우) -> winPoints가 0이라면 None, 제외

# new_pubg_train<-filter(pubg_train, rankPoints!=-1, rankPoints!=-1&killPoints!=0, rankPoints!=-1&winPoints!=0)
# head(new_pubg_train[order(new_pubg_train$rankPoints,decreasing = FALSE),])

#전처리
new_pubg_train<-pubg_train %>%
  filter(rankPoints!=-1) %>% #랭킹포인트가 -1이 아닌 애들만 남기기
  filter(killPoints!=0) %>% #킬포인트가 0이 아닌 애들만 남기기
  filter(winPoints!=0) #윈포인트가 0이 아닌 애들만 남기기
  #pubg_train[order(pubg_train$rankPoints,decreasing=FALSE),] dplyr쓸때 이렇게쓰면 안됨
  #arrange(rankPoints) %>%
  #head(5)

nrow(new_pubg_train) #89509
nrow(pubg_train) #4446966

# #킬포인트 윈포인트 랭크포인트 상관관계 분석
# cor(pubg_train$killPoints,pubg_train$winPoints) #0.9834167 강한 양의 관계
# cor(pubg_train$rankPoints,pubg_train$killPoints) #-0.975555 강한 음의 관계
# cor(pubg_train$rankPoints,pubg_train$winPoints) #-0.9938454 강한 음의 관계
# 
# #킬포인트와 윈포인트로 최종결과 관계 보기 위해서
# head(new_pubg_train[order(new_pubg_train$killPoints,decreasing = TRUE),])
# head(new_pubg_train[order(new_pubg_train$winPoints,decreasing = TRUE),])
# 
# #최종결과에 보다 높은 상관관계를 갖는 것은 윈포인트.. 당연한 것
# cor(new_pubg_train$killPoints, new_pubg_train$winPlacePerc) #0.0898857
# cor(new_pubg_train$winPoints, new_pubg_train$winPlacePerc) #0.2046172

############################################################################################
############################################################################################
############################################################################################
# ###클러스터링
# ###kmeans 클러스터링의 경우 평균값을 이용하기 때문에 컬럼은 모두 숫자로 구성되어 있어야함
# ###data에 NA값 없어야함
# cluster.train.data<-subset(new_pubg_train,select = -c(Id,groupId,matchId,matchType))
# str(cluster.train.data)
# 
# #numeric 작업(클러스터링때문에-scale작업위해서), 
# cluster.train.data$assists<-as.numeric(cluster.train.data$assists)
# cluster.train.data$boosts<-as.numeric(cluster.train.data$boosts)
# cluster.train.data$DBNOs<-as.numeric(cluster.train.data$DBNOs)
# cluster.train.data$headshotKills<-as.numeric(cluster.train.data$headshotKills)
# cluster.train.data$heals<-as.numeric(cluster.train.data$heals)
# cluster.train.data$killPlace<-as.numeric(cluster.train.data$killPlace)
# cluster.train.data$killPoints<-as.numeric(cluster.train.data$killPoints)
# cluster.train.data$kills<-as.numeric(cluster.train.data$kills)
# cluster.train.data$killStreaks<-as.numeric(cluster.train.data$killStreaks)
# cluster.train.data$matchDuration<-as.numeric(cluster.train.data$matchDuration)
# cluster.train.data$maxPlace<-as.numeric(cluster.train.data$maxPlace)
# cluster.train.data$numGroups<-as.numeric(cluster.train.data$numGroups)
# cluster.train.data$rankPoints<-as.numeric(cluster.train.data$rankPoints)
# cluster.train.data$revives<-as.numeric(cluster.train.data$revives)
# cluster.train.data$roadKills<-as.numeric(cluster.train.data$roadKills)
# cluster.train.data$teamKills<-as.numeric(cluster.train.data$teamKills)
# cluster.train.data$vehicleDestroys<-as.numeric(cluster.train.data$vehicleDestroys)
# cluster.train.data$weaponsAcquired<-as.numeric(cluster.train.data$weaponsAcquired)
# cluster.train.data$winPoints<-as.numeric(cluster.train.data$winPoints)
# str(cluster.train.data)
# head(cluster.train.data)
# 
# scaled_data<-scale(cluster.train.data)
# is.na(scaled_data)
# sum(is.na(scaled_data)) #결측치몇개있는지 
# scaled_data<-subset(scaled_data,select = -c(rankPoints)) #NA값인 rankpoints 제거
# head(scaled_data)
# 

# #############클러스터 k 값 찾는 과정
# set.seed(109)
# wss<-0
# for (j in 1:30) {
#   km.out <- kmeans(scaled_data, centers = j, nstart = 20)
#   wss[j] <- km.out$tot.withinss
# }
# 
# wss_df<-data.frame(num_cluster=1:14,wss=wss)
# ggplot(data = wss_df, aes(x=num_cluster, y= wss))+geom_line(color = "lightgrey", size = 2) +
#   geom_point(color = "green", size = 4)
# 
# #클러스터 감소추이를 알아보기 위해서. 감소폭이 더이상 작아지지않을떄가 최적. 11
# for(i in 1:14){
#   wss_df$gap[i]<-wss_df$wss[i]-wss_df$wss[i+1]
# }
# wss_df
# ###########################################################################################
# 
# k<-11
# kmeans.model<-kmeans(scaled_data,centers = k,nstart=20) #nstart:시행횟수
# 
# cluster.train.data$cluster<-kmeans.model$cluster
# 
# table(cluster.train.data$winPlacePerc,kmeans.model$cluster)
# 
# cluster.train.data$winPlacePerc
# qplot(killPoints,winPlacePerc,colour=cluster,data=cluster.train.data)
############################################################################################
############################################################################################
############################################################################################

############################################################################################
############################################################################################
############################################################################################
###XGB 1

#install.packages("xgboost")
# colnames(new_pubg_train)
xgboost_pubg_train<-data.matrix(subset(new_pubg_train,select = -c(Id,groupId,matchId,matchType,winPlacePerc)))
xgb<-xgboost(data=xgboost_pubg_train,label=new_pubg_train$winPlacePerc,
             eta = 0.2,
             nround = 600,
             subsample = 0.8,
             colsample_bytree = 0.8,
             seed = 1,
             eval_metric = "mae",
             objective = "reg:linear",
             nthread = 3,
             max_depth = 5)

xgboost_pubg_test<-data.matrix(subset(pubg_test,select = -c(Id,groupId,matchId,matchType)))
xgb_pred<-predict(xgb,xgboost_pubg_test)
# head(xgb_pred)
# head(pubg_test)
pubg_test$winPlacePerc<-xgb_pred # pubg_test$winPlacePerc<-predict(xgb,xgboost_pubg_test) 한방에 
# head(pubg_test)
write.csv(pubg_test[,c("Id","winPlacePerc")],file="submission.csv",row.names = FALSE)
#write.table(pubg_test[,c("Id","winPlacePerc")],file="submission.csv",row.names = FALSE)

#############################################################################################
####XGB 2......시간이 너무 오래걸림
sum(is.na(pubg_train))
pubg_train[!complete.cases(pubg_train),] #NA값있는 데이터 찾기
pubg_train<-na.omit(pubg_train) #NA값 제거

xgb_pubg_train2<-data.matrix(subset(pubg_train,select = -c(Id,groupId,matchId,matchType,winPlacePerc)))
#############################################################################################


