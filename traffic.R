library(ggmap)
library(dplyr)
register_google(key='AIzaSyD1GC3vww5-oJCOJ0J0YFsmc5WohCVLxO0')

############구글지도 테스트######
getmap <- get_googlemap("seoul")
loc<-c(37.612849,127.030592)
locationInfo <- data.frame(
  lat = 37.612849,
  lon = 127.030592
)
ggmap(getmap)+geom_point(data=locationInfo, aes(x=lon,y=lat))
get_googlemap("seoul",maptype = "roadmap") %>% 
  ggmap+geom_point(data=locationInfo, aes(x=lon,y=lat))
get_googlemap("gangbukgu", maptype="roadmap", zoom=12) %>% 
  ggmap+geom_point(data=locationInfo, aes(x=lon,y=lat))
####################################

#loc파일 읽어오기
#lon:경도(x축),lat:위도(y축)
gangbuk_file<-read.csv("C:\\Users\\thgus\\Downloads\\서울특별시_강북구_무인교통단속카메라_20180801.csv")
gangbuk<-gangbuk_file %>%
  select("위도","경도")
gangbuk<-rename(gangbuk,lon="경도",lat="위도")

get_googlemap("gangbukgu", maptype="roadmap", zoom=13) %>% 
  ggmap+geom_point(data=gangbuk, aes(x=lon,y=lat))