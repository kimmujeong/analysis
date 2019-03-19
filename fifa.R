library(ggplot2) #그래프 라이브러리
library(dplyr)
library(stringr) #str_detect
library(scales) #달러표시
#install.packages("stringr")
fifa<-read.csv("C:\\Users\\thgus\\Desktop\\fifa.csv",header=T,encoding = "UTF-8")
#유로표시가 있어서 인식이 처음에 안되었다. 콘솔창에 Sys.setlocale(category="LC_ALL",locale="us") 입력 후 해결
Sys.setlocale(category="LC_ALL",locale="us")
head(fifa)
fifa$ag
###############################################################################

#전체 오버롤 분포도
ggplot(fifa,aes(x=Overall))+geom_histogram(color = "white", fill = "darkgrey")+
  ggtitle("오버롤 전체 분포도", subtitle = "서브타이틀")

###############################################################################

#나이대별 오버롤 평균
overall_age_mean<-aggregate(Overall~Age,fifa,mean)
overall_age_mean

#나이대별 오버롤 그래프
fifa %>%
  group_by(Age) %>%
  summarise(overall_age_mean2=mean(Overall)) %>%
  ggplot(aes(x=Age,y=overall_age_mean2))+geom_line(size=1)

###############################################################################

#국가별 오버롤 평균
nation_overall_mean<-aggregate(Overall~Nationality,fifa,mean)
nation_overall_mean[order(nation_overall_mean$Overall,decreasing = T),]

#국가갯수카운트
nation_count<-fifa %>%
  count(Nationality) %>%
  print(n=Inf)

#오버롤 평균하고 갯수카운트 합치기
merge_nation<-transform(nation_overall_mean,count=nation_count$n)

#정렬
merge_nation[order(merge_nation$Overall,decreasing = T),] #오버롤 정렬
merge_nation[order(merge_nation$count,decreasing = T),] #국적수 정렬

###############################################################################

#팀별오버롤 상위20개[1]
team_overall<-aggregate(Overall~Club,fifa,mean)
team_overall[order(team_overall$Overall,decreasing = T),]
team_overall_top20<-head(team_overall[order(team_overall$Overall,decreasing = T),],20)
ggplot(team_overall_top20,aes(x=Overall,y=Club))+geom_point(size=3)

###############################################################################

#팀별 주급
fifa2<-fifa %>% 
  mutate(wagemulti=ifelse(str_detect(Wage,"K"),1000,1)) %>%
  mutate(new_wage=as.numeric(str_extract(Wage,"[0-9]+"))*wagemulti) #Wage에서 0부터9까지 한번이상 반복되는거 모두 가져오기

#\\d : 숫자를 가져와라 [a-z]+ : 문자를 가져와라 http://www.datamarket.kr/xe/board_BoGi29/12682
#https://blog.naver.com/PostView.nhn?blogId=liberty264&logNo=221084279451&categoryNo=50&parentCategoryNo=0&viewDate=&currentPage=1&postListTopCurrentPage=1&from=postView

#탑20 확인
wage_data<-subset(fifa2,select = c(Name,new_wage))
head(wage_data[order(wage_data$new_wage,decreasing = T),],20)

#테스트
fifa2 %>%
  group_by(Club) %>%
  summarise(total_wage=sum(new_wage)) %>%
  arrange(desc(total_wage)) %>%
  #print(n=20) %>%
  ggplot(aes(x=Club,y=total_wage))+geom_bar(stat = "identity", color = "black")+
  coord_flip() #수평으로 바 그리기 

#팀별 주급 상위 20개팀
team_wage_top20<-fifa2 %>%
  group_by(Club) %>%
  summarise(total_wage=sum(new_wage)) %>%
  arrange(desc(total_wage)) %>%
  head(n=20)

#팀별 주급 상위 20개팀 그래프
team_wage_top20 %>%
  ggplot(aes(x=Club,y=total_wage))+geom_bar(stat = "identity", color = "black", fill="green")+
  coord_flip()

#팀별 주급 상위20개팀 그래프 [정렬]
team_wage_top20 %>%
  ggplot(aes(x=reorder(Club,total_wage),y=total_wage))+geom_bar(stat = "identity", color = "black", fill="green")+
  scale_y_continuous(labels = dollar_format(prefix = "€"))+
  coord_flip()+
  theme(axis.title = element_blank())

###############################################################################

#그냥
ggplot(fifa,aes(x=Overall))+geom_histogram(binwidth = 5,fill="blue",color="black")+facet_grid(Age~.)
ggplot(fifa,aes(x=Overall))+geom_histogram(binwidth = 5,fill="blue",color="black")+facet_grid(.~Age)

###############################################################################
###############################################################################
###############################################################################
#이해하기
fifa %>% 
  mutate(new_wage=as.numeric(str_extract(Wage,"\\d"))) %>%
  mutate(new_wage2=as.numeric(str_extract(Wage,"[0-9]+"))) %>%
  mutate(new_wage3=as.numeric(str_extract(Wage,"[a-z]+")))
