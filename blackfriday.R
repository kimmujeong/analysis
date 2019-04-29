library(dplyr)
library(tidygraph)
library(ggraph)
library(igraph)
install.packages("igraph")
bf<-read.csv("C:\\Users\\thgus\\Downloads\\black-friday\\BlackFriday.csv")
head(bf)
#각 유저별 총 사용금액
#가장 많이 구매된 제품(product_id)
#남자가 주로 구매한, 여자가 주로 구매한
#나이대별 주로 구매한


##############각 유저별 총 사용금액############
user_bf<-bf %>%
  group_by(User_ID) %>%
  summarise(tot=sum(Purchase))
head(user_bf)

#탑 5
top5_user<-user_bf %>%
  arrange(desc(tot)) %>%
  head()

'
  User_ID      tot
    <int>    <int>
1 1004277 10536783 : 남자 36-45 나이대 
2 1001680  8699232 : 남자 26-35 나이대
3 1002909  7577505 : 남자 26-35
4 1001941  6817493 : 남자 36-45
5 1000424  6573609 : 남자 26-35
6 1004448  6565878 : 남자 26-35
'

##############가장 많이 구매된 제품(product_id)##############
product_count<-bf %>%
  group_by(Product_ID) %>%
  count()

product_count %>%
  arrange(desc(n))


##########남자가 주로 구매한, 여자가 주로 구매한##########
bf %>%
  filter(Gender=='M') %>%
  group_by(Product_ID) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

bf %>%
  filter(Gender=='F') %>%
  group_by(Product_ID) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##########나이대별 주로 구매한#########
bf %>%
  filter(Age=='0-17') %>% #18-25, 26-35, 36-45, 46-50, 51-55, 55+
  group_by(Product_ID) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

########나이대별 총 구매액비교########
bf %>%
  group_by(Age) %>%
  summarise(tot=sum(Purchase)) %>%
  arrange(desc(tot))


#######나이와 성별을 포함한 총 구매액비교#####
bf %>%
  group_by(Age,Gender) %>%
  summarise(tot=sum(Purchase)) %>%
  arrange(desc(tot))

#####도시별 총 구매액비교#######
bf %>%
  group_by(City_Category) %>%
  summarise(tot=sum(Purchase)) %>%
  arrange(desc(tot))

####도시, 나이, 성별을 포함한 총 구매액비교######
bf %>%
  group_by(City_Category,Age,Gender) %>%
  summarise(tot=sum(Purchase)) %>%
  arrange(desc(tot))


test<-bf %>%
  select(Product_ID,Age)

g<-graph.data.frame(test)
plot(g)
