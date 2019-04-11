#고객이 구매한 목록을 보고 재구매할 의사가 보이는 물건을 찾아 추천해주는 시스템

library(dplyr)
library(ggplot2)
library(arules) #연관분석
library(arulesSequences)
library(Matrix)
library(arulesViz)
library(tidyr)
library(tibble) #remove_rownames
library(gridExtra)
library(recommenderlab) #realRatingMatrix
library(RColorBrewer)
library(splitstackshape)
#install.packages("arules")
#install.packages("tidyverse")
#install.packages("arulesViz")
#install.packages("RColorBrewer")
#install.packages("splitstackshape")
#install.packages("arulesSequences")
#install.packages("recommenderlab")
#install.packages("gridExtra")
aisles<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\aisles.csv")
departments<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\departments.csv")
order_products__prior<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\order_products__prior.csv")
order_products__train<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\order_products__train.csv")
orders<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\orders.csv")
products<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\products.csv",encoding = "UTF-8")

sample<-order_products__prior[1:20000,]
head(products)

rm(list=ls())

head(departments) #대분류 21행
head(aisles,20) #중분류 134행
head(order_products__prior,15) #prior 정보, add_to_cart_order:장바구니 추가순서 reordered:재구매여부 
head(order_products__train) #train 정보, add_to_cart_order:장바구니 추가순서, reordered:재구매여부
head(orders,15) #prior, train, test 정보 모두 포함 #order_number:주문갯수, order_dow:요일 order_hour_of_day:시간 days_since_prior_order:재주문에 걸린 일수 
head(products) #상품정보

#################EDA과정##########################

####order_products_train에서 재구매 횟수가 높은 상품확인####
train_reordered_df<-order_products__train %>%
  group_by(product_id) %>%
  filter(reordered==1) %>%
  count(reordered) %>%
  arrange(desc(n))

train_reordered_df$product_id
train_reordered_top10<-head(train_reordered_df$product_id,10)
products[train_reordered_top10,]
filter(products, product_id %in% train_reordered_top10) #filter를 사용해서도 할 수 있음. 내가 원하는 순서대로 정렬은 못함 
####################################################


####order_products_train에서 재구매 횟수가 낮은 상품확인####
train_reordered_no_df<-order_products__train %>%
  group_by(product_id) %>%
  filter(reordered==0) %>%
  count(reordered) %>%
  arrange(desc(n))

train_reordered_no_top10<-head(train_reordered_no_df$product_id,10)
products[train_reordered_no_top10,]
####################################################


#####order_products_train에서 재구매 확률이 높은 상품확인####
head(order_products__train[order(order_products__train$product_id),])
order_products__train %>%
  group_by(product_id) %>%
  summarise(prob=mean(reordered),n=n()) %>% #n추가
  filter(n>40) %>% #40회 이상 
  arrange(desc(prob)) %>%
  left_join(products,by="product_id")
####################################################


#####어느 요일에 제일 많이 구매하는지####
options("scipen" = 100)
orders %>%
  group_by(order_dow) %>%
  ggplot(aes(x=order_dow))+geom_bar()
####################################################


#####어느 시간에 제일 많이 구매하는지####
orders %>%
  ggplot(aes(x=order_hour_of_day))+geom_bar()
####################################################


#############요일별 구매시간 패턴###################
par(mfrow=c(4,2))
p1<-orders %>%
  filter(order_dow==0) %>%
  ggplot(aes(x=order_hour_of_day))+geom_bar()+ggtitle("p1")

p2<-orders %>%
  filter(order_dow==1) %>%
  ggplot(aes(x=order_hour_of_day))+geom_bar()+ggtitle("p2")

p3<-orders %>%
  filter(order_dow==2) %>%
  ggplot(aes(x=order_hour_of_day))+geom_bar()+ggtitle("p3")

p4<-orders %>%
  filter(order_dow==3) %>%
  ggplot(aes(x=order_hour_of_day))+geom_bar()+ggtitle("p4")

p5<-orders %>%
  filter(order_dow==4) %>%
  ggplot(aes(x=order_hour_of_day))+geom_bar()+ggtitle("p5")

p6<-orders %>%
  filter(order_dow==5) %>%
  ggplot(aes(x=order_hour_of_day))+geom_bar()+ggtitle("p6")

p7<-orders %>%
  filter(order_dow==6) %>%
  ggplot(aes(x=order_hour_of_day))+geom_bar()+ggtitle("p7")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol=4)
####################################################


#############첫 구매에 많이 선택하는 목록#############
head(order_products__prior)
order_products__prior<-order_products__prior %>%
  left_join(products,by="product_id")
order_products__prior %>%
  filter(add_to_cart_order==1) %>%
  count(product_name) %>%
  arrange(desc(n))
####################################################

#첫 구매에 많이 선택하는 목록하고 총구매 목록하고 비교..
#############
####################################################


#############
####################################################


#############
####################################################


#####연관분석####
#train파일로 하면 규칙이 안나온다.. prior파일로 실행했음..
sample<-order_products__prior[1:20000,]
sample_join<-sample %>%
  left_join(products,by="product_id")
sample_join_split<-split(sample_join$product_name, sample_join$order_id)
sample_join_transactions<-as(sample_join_split,"transactions")
rules<-apriori(sample_join_transactions, parameter=list(support=0.001, confidence=0.8, minlen = 3))
summary(rules)
inspect(rules[1:50])

inspect(subset(rules, subset = rhs %pin% "Banana")[1:10])
lift_top5<-head(sort(rules, by ="lift"),5) #연관성이 강한 5개 뽑기
inspect(lift_top5)
plot(lift_top5, method = "graph",control=list(type="items",main="")) #plot 찍을 때는 inspect 없는걸로 해야됨


#######kaggle
rules<-apriori(sample_join_split, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="Banana"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])
plot(rules,control=list(col=brewer.pal(11,"Spectral")),main="")
subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph",control=list(type="items",main=""))


#####kaggle 연습
rules_test<-apriori(sample_join_transactions, parameter=list(support=0.001, confidence=0.8),
                    appearance = list(default="lhs",rhs="Banana"),
                    control = list(verbose=F))
rules_test<-sort(rules_test, decreasing=TRUE,by="confidence")
plot_data<-head(sort(rules_test, by ="lift"),5)
plot(plot_data, method = "graph",control=list(type="items",main=""))

#####재주문했을 경우에 규칙찾기
####group_by에서 reordered ==1 일경우를 추가해서 규칙찾고 이전 규칙하고 비교해보기
head(sample_join)
reordered_sample<-sample_join %>%
  filter(reordered==1)
head(reordered_sample)
reordered_sample_split<-split(reordered_sample$product_name,reordered_sample$order_id)
reordered_sample_transations<-as(reordered_sample_split,"transactions")
reordered_rule<-apriori(reordered_sample_transations,parameter = list(support=0.001, confidence=0.8, minlen = 3))
inspect(reordered_rule[1:10])
######################################

######################################
##############협업필터링##############
order_products__prior<-order_products__prior %>%
  arrange(order_id)

join_df<-order_products__prior %>%
  left_join(orders,by="order_id") %>%
  left_join(products, by="product_id")

join_df2<-join_df %>%
  select(order_id,product_id,reordered,user_id,product_name)

#카운트 기반 협업필터링..
count_df<-join_df2 %>%
  group_by(user_id) %>%
  count(product_name)

count_df2<-count_df %>%
  filter(n>50)

head(count_df2)

# grep("Speculoos",count_df2$product_name)
# count_df2[1996,]

######
"
1:5,325,258
2:3,120,658
3:2,091,509
4:1,512,047
5:1,147,284
6:898,431
7:720,377
8;588,304
9:487,653
15:190,877
20:100,537
23:71,008
24:63,485
25:56,835
30:32,941
50:3,946
"
######
count_df2
spread(count_df2,product_name,n)
count_mat <- spread(count_df2, product_name, n) %>%
  remove_rownames() %>%
  column_to_rownames(var="user_id")

count_rrm <- as(as(count_mat, "matrix"), "realRatingMatrix")
rating_eval <- evaluationScheme(count_rrm, method="split", train=0.7, given=1) #given은 count_rrm에서 모든 유저아이디에서 아이템 갯수가 2개이상일 경우에 그것보다 작은수치부터 되는 거 같음

as(count_rrm,"list")
rating_eval

#######################################################
#####1 UBCF#####
ubcf_rmse <- Recommender(getData(rating_eval, "train"), method = "UBCF", 
                         param=list(normalize = "center", method="Cosine")) #method:cosine, pearson 유사도 선정방식
ubcf_pred <- predict(ubcf_rmse, getData(rating_eval, "known"), type="ratings")
options("scipen" = 100)
calcPredictionAccuracy(ubcf_pred, getData(rating_eval, "unknown"))

ubcf_pred <- predict(object = ubcf_rmse, newdata = count_rrm,  n = 5)
ubcf_pred_list<-as(ubcf_pred,"list")

####data table로 보여주는 작업
df<-data.frame(matrix(nrow=2169,ncol=5)) #빈 df생성 

for(i in 1:2169){
  if(identical(ubcf_pred_list[[i]],character(0))){ #character(0)인 부분이 있어서 판별
    df[i,]<-NA
  }
  else{
    df[i,]<-unlist(ubcf_pred_list[i],use.names = FALSE) #unlist를 이용해서 product_name 각각 하나하나씩 나누기
  }
}

user_id<-names(ubcf_pred_list) #리스트 이름 가져오기=user_id 가져오기
rownames(df)=user_id

df %>%
  DT::datatable()
#######################################################

#######################################################
#####1 IBCF#####
ibcf_rmse <- Recommender(getData(rating_eval, "train"), method = "IBCF", param=list(k=30)) #k:유사도 값을 계산하는데 고려되는 이웃의 수
ibcf_pred <- predict(ibcf_rmse, getData(rating_eval, "known"), type="ratings")
options("scipen" = 100)
calcPredictionAccuracy(ibcf_pred, getData(rating_eval, "unknown"))

ibcf_pred <- predict(object = ibcf_rmse, newdata = count_rrm,  n = 5) #type="topNList" 속성도 있는듯.. https://www.kaggle.com/hendraherviawan/itembased-collaborative-filter-recommendation-r
ibcf_pred_list<-as(ibcf_pred,"list")
####data table로 보여주는 작업.. 이것이 최종본 
##에러.. 5개추천하는데 1게,2개,4개 추천하는 것이 있네.. ibcf_pred_list[80]에 3개만 들어있음

user_id<-names(ibcf_pred_list) #리스트 이름 가져오기=user_id 가져오기
ibcf_df<-data.frame(matrix(nrow=2169,ncol=5)) #빈 df생성 

for(i in 1:2169){
  for(j in 1:5){
    if(identical(ibcf_pred_list[[i]],character(0))){ #character(0)인 부분이 있어서 판별
      ibcf_df[i,]<-NA
    } 
    else{
      ibcf_df[i,j]<-unlist(ibcf_pred_list[i],use.names = FALSE)[j]
    }
  }
}

rownames(ibcf_df)=user_id

ibcf_df %>%
  DT::datatable()

#ibcf_pred@items
#######################################################

#######################################################
#####1 POPULAR#####
popular_rmse<-Recommender(getData(rating_eval,"train"), method="POPULAR")
popular_pred<-predict(popular_rmse,newdata=count_rrm,n=5)
popular_pred_list<-as(popular_pred,"list")

user_id<-names(popular_pred_list) #리스트 이름 가져오기=user_id 가져오기
popular_df<-data.frame(matrix(nrow=2169,ncol=5)) #빈 df생성 

for(i in 1:2169){
  for(j in 1:5){
    if(identical(popular_pred_list[[i]],character(0))){ #character(0)인 부분이 있어서 판별
      popular_df[i,]<-NA
    } 
    else{
      popular_df[i,j]<-unlist(popular_pred_list[i],use.names = FALSE)[j]
    }
  }
}

rownames(popular_df)=user_id

popular_df %>%
  DT::datatable()
#######################################################

#######################################################
######2######
trainingData<-sample(977,879)
trainingSet<-reorder_rrm[trainingData]
scheme <- evaluationScheme(trainingSet,method="split", train = .8,
                           given = 2, 
                           goodRating = 4,
                           k = 3)
mUBCF<-Recommender(trainingSet,method="UBCF",parameter="Cosine")
recommenderUserList<-reorder_rrm[-trainingData]
UBCFlist<-predict(mUBCF,recommenderUserList,n=5)
as(UBCFlist,"list")

head(products)
typeof(products$product_name)
products$product_name<-as.character(products$product_name)
#######################################################

#####모형비교#####
#### 모형평가 설정####
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models #추천모델가능한 모든 것 #ALS, ALS_implicit, IBCF, POPULAR, RANDOM, RECOMMEND, SVD, SVDF, UBCF

rating_eval_scheme <- evaluationScheme(count_rrm, method="cross-validation",k=3, given=1, goodRating=4)

## 다수 모형 비교 평가
"
RANDOM 모형이 예측모형 평가를 위한 기본이 되고 이를 바탕으로 POPULAR, IBCF, UBCF, SVD 추천 모형을 개발하고 나서, 
“RMSE”, “MSE”, “MAE” 3가지 측도를 바탕으로 가장 오차가 작은 모형을 선택한다.
"
rcmm_algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL),
  IBCF = list(name = "IBCF", param = NULL),
  UBCF = list(name = "UBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL)
)

rcmm_eval <- evaluate(rating_eval_scheme, rcmm_algorithms, type="ratings")
avg(rcmm_eval)
#######################################################

################
###순차분석#####
head(sample,10)
sample<-rename(sample,sequenceID=add_to_cart_order)
sample<-rename(sample,eventID=order_id)
sample_split<-split(sample$product_id,sample$sequenceID)
sample_transactions<-as(sample_split,"transactions")
inspect(sample_transactions[60])
nrow(sample_transactions)
inspect(sample_transactions[61])

sample_transactions<-rename(sample_transactions,sequenceID=transactionID)
transaction_df<-as(sample_transactions,"data.frame")
transaction_df[60,]
transaction_df<-rename(transaction_df,sequenceID=transactionID)
re_trans<-as(transaction_df,"transactions")
inspect(re_trans[60])

df<-data.frame(matrix(nrow=61,ncol = 2))
df$X1<-1:61
df$X2<-transaction_df$items
df<-rename(df,sequenceID=X1,items=X2)

write.table(df, file="myTemp.csv", sep = ",", row.names = F, col.names = F, quote=F)
t <- read_baskets("myTemp.csv", sep=",",info = c("sequenceID"))
inspect(t[60])
seq_rule_1 <- cspade(t, parameter=list(support=0.3, maxsize=5, maxlen=4), control=list(verbose=TRUE))

data(zaki)
inspect(zaki)
inspect(sample_transactions[60])
head(sample)
################

###################잡##########
# test<-data.frame(matrix(nrow=131209,ncol=39123))
# row<-distinct(order_products__train,order_id) #중복값 제거
# col<-distinct(order_products__train,product_id) #중복값 제거

# order_baskets$product_name<-as.vector(list(order_baskets$product_name))
# transactions <- as(order_baskets$product_name, "transactions")
# inspect(transactions[1])



########## kaggle ##########
# head(mydata)
# mydata<-mydata[,1:2] #picking up first two rows
# mydata<-merge(mydata,products,by="product_id") #merging
# mydata<-arrange(mydata, order_id) # ascending order
# mydata<-mydata[,c(2,3)] #dropping other columns
# mydata[1:10,] # sneak peek
# dt <- split(mydata$product_name, mydata$order_id)
# dt2 = as(dt,"transactions")
# rules = apriori(dt2, parameter=list(support=0.001, confidence=0.8, minlen = 3))
# rules