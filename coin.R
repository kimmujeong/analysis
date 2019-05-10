install.packages("quantmod") #증시라이브러리
source("https://install-github.me/mrchypark/ubci")
library(quantmod) #증시라이브러리
library(ubci)
library(dplyr)
library(ggplot2)
library(proxy) #유사도 측정 라이브러리 
library(lsa) #코사인.. 코사인 거리 구하기 위해서 
library(highcharter) # 그래프 
library(dygraphs)
library(xts)
ubci_index_options()

coin<-ubci_get_options()
btc<-coin$code[1]
ripple<-coin$code[268]
bch<-coin$code[408]

coin %>%
  print(n=10)
unique(coin$exchange)
which(coin$code=="UPBIT.KRW-BCH")


btc_test<-ubci_get(btc, from="2019-05-01")
ripple_test<-ubci_get(ripple, from="2019-05-01")
bch_test<-ubci_get(bch,from="2019-05-01")

btc_test<-btc_test %>%
  select(date,low,high)
ripple_test<-ripple_test %>%
  select(date,low,high)
bch_test<-bch_test %>%
  select(date,low,high)

ggplot(data=ripple_test, aes(x=date,y=low))+geom_line()+scale_x_date(date_breaks = "1 day", date_labels=(date_format="%Y-%m-%d"))+geom_point()+
  geom_text(aes(label=low),vjust=-0.2,hjust=-0.2)

#btc_test 그래프 그리기 
#https://rpubs.com/MarkusLoew/226759
#sacle_x_date : https://codeday.me/ko/qa/20190313/60035.html
ggplot(data=btc_test, aes(x=date))+ #ggplot
  geom_line(data=btc_test,aes(y=low, colour="low"))+ #low 라인 그리기
  scale_x_date(date_breaks = "1 day", date_labels=(date_format="%Y-%m-%d"))+ #date형태로 출력
  geom_point(data=btc_test,aes(y=low, colour="low"))+ #point 표시 
  geom_text(data=btc_test,aes(y=low,label=low),vjust=-0.2,hjust=-0.2)+ #라벨표시, 라벨위치 조정
  
  geom_line(data=btc_test,aes(x=date, y=high, colour="high"))+ #high 라인 그리기
  geom_point(data=btc_test, aes(y=high, colour="high"))+ #point 표시 
  geom_text(data=btc_test,aes(y=high,label=high),vjust=-0.2,hjust=-0.2)+ #라벨표시, 라벨위치 조정
  scale_y_continuous(sec.axis = sec_axis(~., name = "high"))+ #high 축 표시
  
  scale_colour_manual(values = c("blue", "black"))+ #색깔구분 
  labs(y = "low", x = "Date", colour = "Parameter")+ 
  theme(legend.position = c(0.8, 0.9)) #파라미터 위치

cosine(btc_test$high, ripple_test$high) #코사인 유사도
cosine(btc_test$high, bch_test$high) #코사인 유사도


##################################################
#####################미국증시#####################
##################################################

#https://statkclee.github.io/statistics/stat-fe-import.html
#http://henryquant.blogspot.com/2019/01/r-ggplot.html
getSymbols("^GSPC") #s&p 500 지수 가져오기 class : "xts" "zoo"

#############단일종목일 경우 dygraph, highcharter를 이용해서 동적으로 가능######
#dygrpah
dygraph(GSPC) %>%
  dyRangeSelector()

dygraph(GSPC$GSPC.Close) %>%
  dyRangeSelector()

#highcharter
'
highcharter
https://statkclee.github.io/finance/finance-viz-hichart.html
https://www.highcharts.com/blog/data-science/highcharts-for-r-users/
https://www.highcharts.com/stock/demo 
' 
highchart(type = 'stock') %>%
  hc_add_series(GSPC$GSPC.Close)


###################여러개를 비교할 때는 ggplot으로##########################
GSPC_df<-as.data.frame(GSPC) #data frame 형식으로 변환
GSPC_df<-GSPC_df %>% #row로 있던 Date를 컬럼으로 변환하고, Date 형식으로 저장 
  mutate(Date=rownames(.),Date=as.Date(Date))

#GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted
ggplot(GSPC_df,aes(x=Date,y=GSPC.Close))+geom_line()+
  scale_x_date(date_breaks = "6 month", date_labels=(date_format="%Y-%m-%d"))
