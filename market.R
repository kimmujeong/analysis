#고객이 구매한 목록을 보고 재구매할 의사가 보이는 물건을 찾아 추천해주는 시스템

library(dplyr)
library(ggplot2)
library(arules) #연관분석
#install.packages("arules")

aisles<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\aisles.csv")
departments<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\departments.csv")
order_products__prior<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\order_products__prior.csv")
order_products__train<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\order_products__train.csv")
orders<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\orders.csv")
products<-read.csv("C:\\Users\\thgus\\Downloads\\instacart-market-basket-analysis\\products.csv")

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
  filter(n>40) %>% #40횟 이상 
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
#order_products__train에서 재구매여부 연관성 아이템 찾아보기
#데이터 형태를 바꿔야할것같음 희소행렬로..
head(order_products__train,20)
#데이터변환
order_baskets<-order_products__train %>% 
  inner_join(products, by="product_id") %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))

# test<-data.frame()
# row<-distinct(order_products__train,order_id) #중복값 제거
# col<-distinct(order_products__train,product_id) #중복값 제거


# order_products__train_list<-split(order_products__train$order_id,order_products__train$product_id,order_products__train$add_to_cart_order,order_products__train$reordered)
# order_products__train_trans<-as(order_products__train_list,"transactions")
# rules <- apriori(data = order_products__train_trans, parameter = list(support = 0.01, confidence = 0.8, minlen = 1))
# summary(rules)
