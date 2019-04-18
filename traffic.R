library(ggmap)
library(dplyr)
library(ggplot2)
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
gangbuk_file<-read.csv("C:\\Users\\thgus\\Downloads\\서울특별시_강북구_무인교통단속카메라.csv")
gwanak_file<-read.csv("C:\\Users\\thgus\\Downloads\\서울특별시_관악구_무인교통단속카메라.csv")
eunpyeong_file<-read.csv("C:\\Users\\thgus\\Downloads\\서울특별시_은평구_무인교통단속카메라.csv")
gangdong_file<-read.csv("C:\\Users\\thgus\\Downloads\\서울특별시_강동구_무인교통단속카메라.csv")
geumcheon_file<-read.csv("C:\\Users\\thgus\\Downloads\\서울특별시_금천구_무인교통단속카메라.csv")
jongno_file<-read.csv("C:\\Users\\thgus\\Downloads\\서울특별시_종로구_무인교통단속카메라.csv")
seocho_file<-read.csv("C:\\Users\\thgus\\Downloads\\서울특별시_서초구_무인교통단속카메라.csv")
seodaemun_file<-read.csv("C:\\Users\\thgus\\Downloads\\서울특별시_서대문구_무인교통단속카메라.csv")

#######전처리########
gangbuk<-gangbuk_file %>%
  select("위도","경도")
gangbuk<-rename(gangbuk,lon="경도",lat="위도")

gwanak<-gwanak_file %>%
  select("위도","경도")
gwanak<-rename(gwanak,lon="경도",lat="위도")

eunpyeong<-eunpyeong_file %>%
  select("위도","경도")
eunpyeong<-rename(eunpyeong,lon="경도",lat="위도")

gangdong<-gangdong_file %>%
  select("위도","경도")
gangdong<-rename(gangdong,lon="경도",lat="위도")

geumcheon<-geumcheon_file %>%
  select("위도","경도")
geumcheon<-rename(geumcheon,lon="경도",lat="위도")

jongno<-jongno_file %>%
  select("위도","경도")
jongno<-rename(jongno,lon="경도",lat="위도")

seocho<-seocho_file %>%
  select("위도","경도")
seocho<-rename(seocho,lon="경도",lat="위도")

seodaemun<-seodaemun_file %>%
  select("위도","경도")
seodaemun<-rename(seodaemun,lon="경도",lat="위도")
###########################################


#######시각화#########
#지역구 zoom13 / 서울전지역 zoom11
###전체표시 
get_googlemap("seoul", maptype="roadmap", zoom=11) %>%
  ggmap+geom_point(data=gangbuk, aes(x=lon,y=lat))+
  geom_point(data=jongno,aes(x=lon,y=lat),color='blue')+
  geom_point(data=gwanak,aes(x=lon,y=lat),color='yellow')+
  geom_point(data=eunpyeong,aes(x=lon,y=lat),color='gray')+
  geom_point(data=gangdong,aes(x=lon,y=lat),color='orange')+
  geom_point(data=geumcheon,aes(x=lon,y=lat),color='pink')+
  geom_point(data=seocho,aes(x=lon,y=lat),color='green')+
  geom_point(data=seodaemun,aes(x=lon,y=lat),color='violet')+
  geom_point(data=ma_gangbuk_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gangbuk_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gangbuk_loc[[3]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_jongno_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_jongno_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_jongno_loc[[3]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gwanak_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gwanak_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gwanak_loc[[3]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_eunpyeong_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_eunpyeong_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_eunpyeong_loc[[3]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gangdong_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gangdong_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gangdong_loc[[3]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_geumcheon_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_geumcheon_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_geumcheon_loc[[3]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seocho_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seocho_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seocho_loc[[3]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seodaemun_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seodaemun_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seodaemun_loc[[3]],aes(x=lon, y = lat), color='red')

#강북구
get_googlemap("gangbukgu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=gangbuk, aes(x=lon,y=lat))+
  geom_point(data=ma_gangbuk_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gangbuk_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gangbuk_loc[[3]],aes(x=lon, y = lat), color='red')

#종로구
get_googlemap("jongnogu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=jongno, aes(x=lon,y=lat))+
  geom_point(data=ma_jongno_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_jongno_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_jongno_loc[[3]],aes(x=lon, y = lat), color='red')

#관악구
get_googlemap("gwanakgu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=gwanak, aes(x=lon,y=lat))+
  geom_point(data=ma_gwanak_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gwanak_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gwanak_loc[[3]],aes(x=lon, y = lat), color='red')

#은평구
get_googlemap("eunpyeonggu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=eunpyeong, aes(x=lon,y=lat))+
  geom_point(data=ma_eunpyeong_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_eunpyeong_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_eunpyeong_loc[[3]],aes(x=lon, y = lat), color='red')

#강동구
get_googlemap("gangdonggu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=gangdong, aes(x=lon,y=lat))+
  geom_point(data=ma_gangdong_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gangdong_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_gangdong_loc[[3]],aes(x=lon, y = lat), color='red')

#금천구
get_googlemap("geumcheongu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=geumcheon, aes(x=lon,y=lat))+
  geom_point(data=ma_geumcheon_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_geumcheon_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_geumcheon_loc[[3]],aes(x=lon, y = lat), color='red')

#서초구
get_googlemap("seochogu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=seocho, aes(x=lon,y=lat))+
  geom_point(data=ma_seocho_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seocho_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seocho_loc[[3]],aes(x=lon, y = lat), color='red')

#서대문구
get_googlemap("seodaemungu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=seodaemun, aes(x=lon,y=lat))+
  geom_point(data=ma_seodaemun_loc[[1]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seodaemun_loc[[2]],aes(x=lon, y = lat), color='red')+
  geom_point(data=ma_seodaemun_loc[[3]],aes(x=lon, y = lat), color='red')
###################################################################