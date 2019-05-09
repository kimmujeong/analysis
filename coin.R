source("https://install-github.me/mrchypark/ubci")
library(ubci)
library(dplyr)
library(ggplot2)
ubci_index_options()
coin<-ubci_get_options()
coin %>%
  print(n=10)
unique(coin$exchange)
btc<-coin$code[1]
ripple<-coin$code[268]
bch<-coin$code[408]
which(coin$code=="UPBIT.KRW-BCH")


btc_test<-ubci_get(btc, from="2019-05-01")
ripple_test<-ubci_get(ripple, from="2019-05-01")
bch_test<-ubci_get(bch,from="2019-05-01")

btc_test<-btc_test %>%
  select(date,low,high)
ripple_test<-ripple_test %>%
  select(date,low,high)

ggplot(data=btc_test, aes(x=date))+geom_line()+scale_x_date(date_breaks = "1 day", date_labels=(date_format="%Y-%m-%d"))+geom_point()+geom_text(aes(label=low),vjust=-0.2,hjust=-0.2)+
  geom_line(aes(x=date, y=high),color="blue")+geom_point(aes(y=high))+geom_text(aes(label=low))

ggplot(data=ripple_test, aes(x=date,y=low))+geom_line()+scale_x_date(date_breaks = "1 day", date_labels=(date_format="%Y-%m-%d"))+geom_point()+
  geom_text(aes(label=low),vjust=-0.2,hjust=-0.2)

ggplot(data=btc_test, aes(x=date))+geom_line(data=btc_test,aes(y=low))+scale_x_date(date_breaks = "1 day", date_labels=(date_format="%Y-%m-%d"))+geom_point(data=btc_test,aes(y=low))+geom_text(data=btc_test,aes(y=low,label=low),vjust=-0.2,hjust=-0.2)+
                                    geom_line(data=btc_test,aes(x=date, y=high),color="blue")+geom_point(data=btc_test, aes(y=high))+geom_text(data=btc_test,aes(y=high,label=high),vjust=-0.2,hjust=-0.2)
