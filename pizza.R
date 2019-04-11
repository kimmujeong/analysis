library(dplyr)

#한글인식이 안되서 us로 설정하고 파일읽어온 다음에 다시 korean으로 설정하면 됨
pizza<-read.csv("C:\\Users\\thgus\\Downloads\\pizza.csv",header = FALSE,encoding = "UTF-8")
Sys.setlocale(category="LC_ALL",locale="us")
Sys.setlocale("LC_ALL", "korean")
head(pizza)
colnames(pizza)<-c("category","year","month","day","dow","area","gu","dong","sex","age","rate")
pizza<-pizza[-c(1),]
view(pizza)

pizza %>%
  filter(area=="인천광역시") %>%
  count(gu) %>%
  arrange(desc(n))

pizza %>%
  filter(area=="인천광역시") %>%
  count(dong) %>%
  arrange(desc(n)) %>%
  print(n=Inf)

pizza %>%
  filter(area=="인천광역시") %>%
  arrange(desc(rate))