#고객이 구매한 목록을 보고 재구매할 의사가 보이는 물건을 찾아 추천해주는 시스템

library(dplyr)
library(ggplot2)
library(arules) #연관분석
library(Matrix)
library(arulesViz)
library(RColorBrewer)
library(splitstackshape)
#install.packages("arules")
#install.packages("tidyverse")
#install.packages("arulesViz")
#install.packages("RColorBrewer")
install.packages("splitstackshape")
aisles<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\aisles.csv")
departments<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\departments.csv")
order_products__prior<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\order_products__prior.csv")
order_products__train<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\order_products__train.csv")
orders<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\orders.csv")
products<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\products.csv")


mydata<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\order_products__prior.csv",nrow=20000)

rm(list=ls())

head(departments) #대분류 21행
head(aisles,20) #중분류 134행
head(order_products__prior,20) #prior 정보, add_to_cart_order:장바구니 추가순서 reordered:재구매여부 
head(order_products__train) #train 정보, add_to_cart_order:장바구니 추가순서, reordered:재구매여부
head(orders,20) #prior, train, test 정보 모두 포함 #order_number:주문갯수, order_dow:요일 order_hour_of_day:시간 days_since_prior_order:재주문에 걸린 일수 
head(products) #상품정보

#EDA과정

####################################################
#order_products_train에서 재구매 횟수가 높은 상품확인
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

####################################################
#order_products_train에서 재구매 횟수가 낮은 상품확인
train_reordered_no_df<-order_products__train %>%
  group_by(product_id) %>%
  filter(reordered==0) %>%
  count(reordered) %>%
  arrange(desc(n))

train_reordered_no_top10<-head(train_reordered_no_df$product_id,10)
products[train_reordered_no_top10,]
####################################################

####################################################
#order_products_train에서 재구매 확률이 높은 상품확인
head(order_products__train[order(order_products__train$product_id),])
order_products__train %>%
  group_by(product_id) %>%
  summarise(prob=mean(reordered),n=n()) %>% #n추가
  filter(n>40) %>% #40회 이상 
  arrange(desc(prob)) %>%
  left_join(products,by="product_id")
####################################################

####################################################
#어느 요일에 제일 많이 구매하는지
options("scipen" = 100)
orders %>%
  group_by(order_dow) %>%
  ggplot(aes(x=order_dow))+geom_bar()
####################################################

####################################################
#어느 시간에 제일 많이 구매하는지
orders %>%
  group_by(order_hour_of_day) %>%
  ggplot(aes(x=order_hour_of_day))+geom_bar()
####################################################


#알고리즘
#train파일로 하면 규칙이 안나온다.. prior파일로 실행했음..
sample<-order_products__prior[1:20000,]
sample_join<-sample %>%
  left_join(products,by="product_id")
#sample_join  
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
rules_test<-apriori(sample_join_transactions, parameter=list(support=0.001, confidence=0.8),appearance = list(default="lhs",rhs="Banana"),control = list(verbose=F))
rules_test<-sort(rules_test, decreasing=TRUE,by="confidence")
plot_data<-head(sort(rules_test, by ="lift"),5)
plot(plot_data, method = "graph",control=list(type="items",main=""))


###순차분석



# test<-data.frame(matrix(nrow=131209,ncol=39123))
# row<-distinct(order_products__train,order_id) #중복값 제거
# col<-distinct(order_products__train,product_id) #중복값 제거

# order_baskets$product_name<-as.vector(list(order_baskets$product_name))
# transactions <- as(order_baskets$product_name, "transactions")
# inspect(transactions[1])



########################## kaggle
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
