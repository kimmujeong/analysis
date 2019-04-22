###EDA
library(dplyr)
library(stringr)

bike_file<-read.csv("C:\\Users\\thgus\\NaverCloud\\Downloads\\교통사고 스팟\\도로교통공단_전국_자전거사고다발지(2017년).csv")
older_file<-read.csv("C:\\Users\\thgus\\NaverCloud\\Downloads\\교통사고 스팟\\도로교통공단_전국_보행노인사고다발지(2017년).csv")
death_file<-read.csv("C:\\Users\\thgus\\NaverCloud\\Downloads\\교통사고 스팟\\도로교통공단_전국_사망교통사고_2017.csv")

'
강북구:11305
종로구:11110
관악구:11620
은평구:11380
강동구:11740
금천구:11545
서초구:11650
서대문구:11410
'
bike_eda<-bike_file %>% 
  filter(str_detect(법정동코드, "^11410")) %>%
  select(발생건수,사상자수,사망자수,중상자수,경상자수,부상신고자수) #전부 숫자형이라 all sum 가능 
bike_eda %>%
  summarise_all(funs(sum))

older_file %>%
  filter(str_detect(법정동코드, "^11410")) %>%
  select(발생건수,사상자수,사망자수,중상자수,경상자수,부상신고자수) %>%
  summarise_all(funs(sum))

death_file %>%
  filter(발생지시군구=="서대문구") %>%
  select(사상자수,사망자수,중상자수,경상자수,부상신고자수) %>%
  summarise_all(funs(sum))

bike_gangbuk_eda<-bike_file %>% 
  filter(str_detect(법정동코드, "^11305")) %>%
  select(관할경찰서,다발지명,발생건수,사상자수,사망자수,중상자수,경상자수,부상신고자수) #문자형이 포함되어 있어 각각 따로 sum 해줘야 함 

bike_gangbuk_eda %>%
  summarise(sum(발생건수),sum(사상자수),sum(사망자수),sum(중상자수),sum(경상자수),sum(부상신고자수))