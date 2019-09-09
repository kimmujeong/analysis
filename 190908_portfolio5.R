library(XML)
library(data.table)
library(stringr) #str_extract_all
library(dplyr)
library(ggplot2)
library(ggmap)
api_url<-"http://apis.data.go.kr/B552061/frequentzoneLg/getRestFrequentzoneLg"
servicekey<-"oxrnAfkJN4PXQnHw7cNJANXNzXxIz6B6ebL8oWffyvb9nb5mqpQX4ANZBzFnUmhG2XhKl9PQ8TDQ7u5xY974WQ%3D%3D"
register_google(key='AIzaSyD1GC3vww5-oJCOJ0J0YFsmc5WohCVLxO0')
'
강북구:305
종로구:110
관악구:620
은평구:380
강동구:740
금천구:545
서초구:650
서대문구:410
gangbuk
jongno
gwanak
eunpyeong
gangdong
geumcheon
seocho
seodaemun
'

#######################function만들기####################
many_accident_function<-function(x){
  if(x=="gangbuk") #switch문 switch('gangbuk','gangbuk'=code<-20)
    code<-305
  else if(x=="jongno")
    code<-110
  else if(x=="gwanak")
    code<-620
  else if(x=="eunpyeong")
    code<-380
  else if(x=="gangdong")
    code<-740
  else if(x=="geumcheon")
    code<-545
  else if(x=="seocho")
    code<-650
  else if(x=="seodaemun")
    code<-410
  
  url<-paste0(api_url,"?","serviceKey=",servicekey,"&searchYearCd=",2017,"&siDo=",11,"&guGun=",code,"&type=","xml","&numOfRows=",10,"&pageNo=",1)
  raw.data <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "utf-8")
  rootNode <- xmlRoot(raw.data)
  items <- rootNode[[2]][['items']]
  
  size<-xmlSize(items)
  item<-list()
  x<-list()
  
  for (i in 1:size){
    item_temp <- xmlSApply(items[[i]],xmlValue)
    item_temp <- item_temp['geom_json']
    item[[i]] <- item_temp
    
    item[[i]]<-substr(item[[i]],33+2,nchar(item[[i]])-3)
    item[[i]]<-str_extract_all(item[[i]],'(?<=\\[)[0-9]+[.?0-9]+,[0-9]+[.0-9]+')
    row_length=length(item[[i]][[1]])
    location_df<-data.frame(matrix(nrow=row_length, ncol=2))
    for (a in 1:row_length){
      for (b in 1:2){
        tmp<-str_split(item[[i]][[1]][a],',')
        location_df[a,b]<-tmp[[1]][b]
      }
    }
    location_df<-lapply(location_df,as.double) #문자형태를 숫자형태로 변환 
    location_df<-data.frame(location_df$X1,location_df$X2) #다시 데이터 프레임 형태로 
    location_df<-rename(location_df,"lon"=location_df.X1,"lat"=location_df.X2)
    x[[i]]<-location_df
  }
  return(x)
}
#########################################################

ma_gangbuk_loc<-many_accident_function("gangbuk")
ma_jongno_loc<-many_accident_function("jongno")
ma_gwanak_loc<-many_accident_function("gwanak")
ma_eunpyeong_loc<-many_accident_function("eunpyeong")
ma_gangdong_loc<-many_accident_function("gangdong")
ma_geumcheon_loc<-many_accident_function("geumcheon")
ma_seocho_loc<-many_accident_function("seocho")
ma_seodaemun_loc<-many_accident_function("seodaemun")



###########################################################
###########################################################
########자전거 사고 다발지역
bike_file<-read.csv("/home/kmj/spot/도로교통공단_전국_자전거사고다발지_2017년.csv", fileEncoding = "UTF-8")
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
#####데이터탐색
bike_gangbuk_eda<-bike_file %>% 
  filter(str_detect(법정동코드, "^11305")) %>%
  select(발생건수,사상자수,사망자수,중상자수,경상자수,부상신고자수)
knitr::kable(bike_gangbuk_eda)
bike_gangbuk_eda %>%
  summarise_all(funs(sum))
bike_gangbuk_eda %>%
  summarise(funs(sum))


############function 만들기############
bike_function<-function(x){
  if(x=="gangbuk") #switch문 switch('gangbuk','gangbuk'=code<-20)
    code<-"^11305"
  else if(x=="jongno")
    code<-"^11110"
  else if(x=="gwanak")
    code<-"^11620"
  else if(x=="eunpyeong")
    code<-"^11380"
  else if(x=="gangdong")
    code<-"^11740"
  else if(x=="geumcheon")
    code<-"^11545"
  else if(x=="seocho")
    code<-"^11650"
  else if(x=="seodaemun")
    code<-"^11410"
  
  bike_df<-bike_file %>% 
    filter(str_detect(법정동코드, code))
  
  count<-nrow(bike_df) #총 폴리곤의 갯수..
  final_list<-list() #최종적으로 데이터를 담을 리스트 
  
  for(a in 1:count){
    polygon<-bike_df$다발지역폴리곤[a]
    str<-as.character(polygon) #문자열로 타입 변환
    str<-substr(str,33+2,nchar(str)-2) #쓸데없는 표현정리
    str_list<-str_extract_all(str,'(?<=\\[)[0-9]+[.?0-9]+,[0-9]+[.0-9]+')
    row_length=length(str_list[[1]]) #데이터 갯수를 로우숫자로 지정 
    
    bike_location<-data.frame(matrix(nrow=row_length, ncol=2))
    
    for (i in 1:row_length){
      for (j in 1:2){
        tmp<-str_split(str_list[[1]][i],',')
        bike_location[i,j]<-tmp[[1]][j]
      }
    }
    
    bike_location<-lapply(bike_location,as.double) #문자형태를 숫자형태로 변환 
    bike_location<-data.frame(bike_location$X1,bike_location$X2) #다시 데이터 프레임 형태로 , v인지 x인지 확인해야 함 
    bike_location<-setNames(data.frame(bike_location),c("lon","lat"))
    final_list[[a]]<-bike_location
  }
  
  return(final_list)
}
####################################

bike_gangbuk_loc<-bike_function("gangbuk") #9
bike_jongno_loc<-bike_function("jongno") #3
bike_gwanak_loc<-bike_function("gwanak") #2
bike_eunpyeong_loc<-bike_function("eunpyeong") #3
bike_gangdong_loc<-bike_function("gangdong") #7
bike_geumcheon_loc<-bike_function("geumcheon") #3
#bike_seocho_loc<-bike_function("seocho") #파일에 서초구 존재하지 않음
bike_seodaemun_loc<-bike_function("seodaemun") #1

########################################################################
########################################################################
########보행노인 사고 다발지역
older_file<-read.csv("/home/kmj/spot/도로교통공단_전국_보행노인사고다발지_2017년.csv")

#######함수만들기######
older_function<-function(x){
  if(x=="gangbuk") #switch문 switch('gangbuk','gangbuk'=code<-20)
    code<-"^11305"
  else if(x=="jongno")
    code<-"^11110"
  else if(x=="gwanak")
    code<-"^11620"
  else if(x=="eunpyeong")
    code<-"^11380"
  else if(x=="gangdong")
    code<-"^11740"
  else if(x=="geumcheon")
    code<-"^11545"
  else if(x=="seocho")
    code<-"^11650"
  else if(x=="seodaemun")
    code<-"^11410"
  
  older_df<-older_file %>% 
    filter(str_detect(법정동코드, code))
  
  count<-nrow(older_df) #총 폴리곤의 갯수..
  final_list<-list() #최종적으로 데이터를 담을 리스트 
  
  for(a in 1:count){
    polygon<-older_df$다발지역폴리곤[a]
    str<-as.character(polygon) #문자열로 타입 변환
    str<-substr(str,33+2,nchar(str)-2) #쓸데없는 표현정리
    str_list<-str_extract_all(str,'(?<=\\[)[0-9]+[.?0-9]+,[0-9]+[.0-9]+')
    row_length=length(str_list[[1]]) #데이터 갯수를 로우숫자로 지정 
    
    older_location<-data.frame(matrix(nrow=row_length, ncol=2))
    
    for (i in 1:row_length){
      for (j in 1:2){
        tmp<-str_split(str_list[[1]][i],',')
        older_location[i,j]<-tmp[[1]][j]
      }
    }
    
    older_location<-lapply(older_location,as.double) #문자형태를 숫자형태로 변환 
    older_location<-data.frame(older_location$X1,older_location$X2) #다시 데이터 프레임 형태로 , v인지 x인지 확인해야 함 
    older_location<-setNames(data.frame(older_location),c("lon","lat"))
    
    final_list[[a]]<-older_location
  }
  
  return(final_list)
}

older_gangbuk_loc<-older_function("gangbuk") #6:원소갯수
older_jongno_loc<-older_function("jongno") #7:원소갯수
older_gwanak_loc<-older_function("gwanak") #7:원소갯수
older_eunpyeong_loc<-older_function("eunpyeong") #7:원소갯수
older_gangdong_loc<-older_function("gangdong") #7:원소갯수
older_geumcheon_loc<-older_function("geumcheon") #3:원소갯수
older_seocho_loc<-older_function("seocho") #2:원소갯수
older_seodaemun_loc<-older_function("seodaemun") #7:원소갯수


########################################################################
########사망교통사고
death_file<-read.csv("/home/kmj/spot/도로교통공단_전국_사망교통사고_2017년.csv")
death_gangbuk_loc<-death_file %>%
  filter(발생지시군구=="강북구") %>%
  select(경도,위도)
death_jongno_loc<-death_file %>%
  filter(발생지시군구=="종로구") %>%
  select(경도,위도)
death_gwanak_loc<-death_file %>%
  filter(발생지시군구=="관악구") %>%
  select(경도,위도)
death_eunpyeong_loc<-death_file %>%
  filter(발생지시군구=="은평구") %>%
  select(경도,위도)
death_gangdong_loc<-death_file %>%
  filter(발생지시군구=="강동구") %>%
  select(경도,위도)
death_geumcheon_loc<-death_file %>%
  filter(발생지시군구=="금천구") %>%
  select(경도,위도)
death_seocho_loc<-death_file %>%
  filter(발생지시군구=="서초구") %>%
  select(경도,위도)
death_seodaemun_loc<-death_file %>%
  filter(발생지시군구=="서대문구") %>%
  select(경도,위도)


########################################################################
########서울시 무인교통단속카메라 위치 
#loc파일 읽어오기
#lon:경도(x축),lat:위도(y축)
gangbuk_file<-read.csv("/home/kmj/spot/서울특별시_강북구_무인교통단속카메라.csv")
gwanak_file<-read.csv("/home/kmj/spot/서울특별시_관악구_무인교통단속카메라.csv")
eunpyeong_file<-read.csv("/home/kmj/spot/서울특별시_은평구_무인교통단속카메라.csv")
gangdong_file<-read.csv("/home/kmj/spot/서울특별시_강동구_무인교통단속카메라.csv")
geumcheon_file<-read.csv("/home/kmj/spot/서울특별시_금천구_무인교통단속카메라.csv")
jongno_file<-read.csv("/home/kmj/spot/서울특별시_종로구_무인교통단속카메라.csv")
seocho_file<-read.csv("/home/kmj/spot/서울특별시_서초구_무인교통단속카메라.csv")
seodaemun_file<-read.csv("/home/kmj/spot/서울특별시_서대문구_무인교통단속카메라.csv")

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

##############################
#           시각화           #
##############################

#지역구 zoom13 / 서울전지역 zoom11
############################## 
# 전 지역-사고다발 지역 표시 #  
##############################
get_googlemap("seoul", maptype="roadmap", zoom=11) %>%
  ggmap+geom_point(data=gangbuk, aes(x=lon,y=lat,color="강북구 무인교통단속카메라 위치"))+
  geom_point(data=jongno,aes(x=lon,y=lat,color="종로구 무인교통단속카메라 위치"))+
  geom_point(data=gwanak,aes(x=lon,y=lat,color="관악구 무인교통단속카메라 위치"))+
  geom_point(data=eunpyeong,aes(x=lon,y=lat,color="은평구 무인교통단속카메라 위치"))+
  geom_point(data=gangdong,aes(x=lon,y=lat,color="강동구 무인교통단속카메라 위치"))+
  geom_point(data=geumcheon,aes(x=lon,y=lat,color="금천구 무인교통단속카메라 위치"))+
  geom_point(data=seocho,aes(x=lon,y=lat,color="서초구 무인교통단속카메라 위치"))+
  geom_point(data=seodaemun,aes(x=lon,y=lat,color="서대문구 무인교통단속카메라 위치"))+
  geom_point(data=ma_gangbuk_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_gangbuk_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_gangbuk_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_jongno_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_jongno_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_jongno_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_gwanak_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_gwanak_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_gwanak_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_eunpyeong_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_eunpyeong_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_eunpyeong_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_gangdong_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_gangdong_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_gangdong_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_geumcheon_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_geumcheon_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_geumcheon_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_seocho_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_seocho_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_seocho_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_seodaemun_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_seodaemun_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역"))+
  geom_point(data=ma_seodaemun_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역"))+
  scale_color_manual("범례",limits=c("강북구 무인교통단속카메라 위치","종로구 무인교통단속카메라 위치","관악구 무인교통단속카메라 위치","은평구 무인교통단속카메라 위치","강동구 무인교통단속카메라 위치","금천구 무인교통단속카메라 위치","서초구 무인교통단속카메라 위치","서대문구 무인교통단속카메라 위치","사고다발 지역"), values=c("black","blue","yellow","gray","orange","pink","green","violet","red"))


############################## 
#           강북구           #  
##############################
get_googlemap("gangbukgu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=gangbuk, aes(x=lon,y=lat,color="강북구 무인교통단속카메라 위치",shape="강북구 무인교통단속카메라 위치"))+
  geom_point(data=ma_gangbuk_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+ #사고다발 지역
  geom_point(data=ma_gangbuk_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_gangbuk_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=bike_gangbuk_loc[[1]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+ #자전거
  geom_point(data=bike_gangbuk_loc[[2]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangbuk_loc[[3]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangbuk_loc[[4]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangbuk_loc[[5]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangbuk_loc[[6]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangbuk_loc[[7]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangbuk_loc[[8]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangbuk_loc[[9]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=older_gangbuk_loc[[1]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+ # 노인보행사고 지역
  geom_point(data=older_gangbuk_loc[[2]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangbuk_loc[[3]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangbuk_loc[[4]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangbuk_loc[[5]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangbuk_loc[[6]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=death_gangbuk_loc,aes(x=경도, y=위도,color="사망교통사고 지역",shape="사망교통사고 지역"),fill="red",size=3)+ #사망교통사고 지역
  scale_shape_manual(name="범례",limits=c("강북구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"),values=c(16,16,16,16,25))+
  scale_color_manual(name="범례",limits=c("강북구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"), values=c("black","red","orange","blue","black"))


############################## 
#           종로구           #  
##############################
get_googlemap("jongnogu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=jongno, aes(x=lon,y=lat,color="종로구 무인교통단속카메라 위치",shape="종로구 무인교통단속카메라 위치"))+
  geom_point(data=ma_jongno_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_jongno_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_jongno_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=bike_jongno_loc[[1]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_jongno_loc[[2]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_jongno_loc[[3]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=older_jongno_loc[[1]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_jongno_loc[[2]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_jongno_loc[[3]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_jongno_loc[[4]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_jongno_loc[[5]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_jongno_loc[[6]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_jongno_loc[[7]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=death_jongno_loc,aes(x=경도, y=위도,color="사망교통사고 지역",shape="사망교통사고 지역"),fill="red",size=3)+
  scale_shape_manual(name="범례",limits=c("종로구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"),values=c(16,16,16,16,25))+
  scale_color_manual(name="범례",limits=c("종로구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"), values=c("black","red","orange","blue","black"))


############################## 
#           관악구           #  
##############################
get_googlemap("gwanakgu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=gwanak, aes(x=lon,y=lat,color="관악구 무인교통단속카메라 위치",shape="관악구 무인교통단속카메라 위치"))+
  geom_point(data=ma_gwanak_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_gwanak_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_gwanak_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=bike_gwanak_loc[[1]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gwanak_loc[[2]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=older_gwanak_loc[[1]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gwanak_loc[[2]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gwanak_loc[[3]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gwanak_loc[[4]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gwanak_loc[[5]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gwanak_loc[[6]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gwanak_loc[[7]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=death_gwanak_loc,aes(x=경도, y=위도,color="사망교통사고 지역",shape="사망교통사고 지역"),fill="red",size=3)+
  scale_shape_manual(name="범례",limits=c("관악구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"),values=c(16,16,16,16,25))+
  scale_color_manual(name="범례",limits=c("관악구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"), values=c("black","red","orange","blue","black"))

############################## 
#           은평구           #  
##############################
get_googlemap("eunpyeonggu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=eunpyeong, aes(x=lon,y=lat,color="은평구 무인교통단속카메라 위치",shape="은평구 무인교통단속카메라 위치"))+
  geom_point(data=ma_eunpyeong_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_eunpyeong_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_eunpyeong_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=bike_eunpyeong_loc[[1]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_eunpyeong_loc[[2]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_eunpyeong_loc[[3]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=older_eunpyeong_loc[[1]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_eunpyeong_loc[[2]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_eunpyeong_loc[[3]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_eunpyeong_loc[[4]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_eunpyeong_loc[[5]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_eunpyeong_loc[[6]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_eunpyeong_loc[[7]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=death_eunpyeong_loc,aes(x=경도, y=위도,color="사망교통사고 지역",shape="사망교통사고 지역"),fill="red",size=3)+
  scale_shape_manual(name="범례",limits=c("은평구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"),values=c(16,16,16,16,25))+
  scale_color_manual(name="범례",limits=c("은평구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"), values=c("black","red","orange","blue","black"))


############################## 
#           강동구           #  
##############################
get_googlemap("gangdonggu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=gangdong, aes(x=lon,y=lat,color="강동구 무인교통단속카메라 위치",shape="강동구 무인교통단속카메라 위치"))+
  geom_point(data=ma_gangdong_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_gangdong_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_gangdong_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=bike_gangdong_loc[[1]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangdong_loc[[2]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangdong_loc[[3]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangdong_loc[[4]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangdong_loc[[5]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangdong_loc[[6]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_gangdong_loc[[7]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=older_gangdong_loc[[1]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangdong_loc[[2]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangdong_loc[[3]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangdong_loc[[4]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangdong_loc[[5]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangdong_loc[[6]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_gangdong_loc[[7]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=death_gangdong_loc,aes(x=경도, y=위도,color="사망교통사고 지역",shape="사망교통사고 지역"),fill="red",size=3)+
  scale_shape_manual(name="범례",limits=c("강동구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"),values=c(16,16,16,16,25))+
  scale_color_manual(name="범례",limits=c("강동구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"), values=c("black","red","orange","blue","black"))


############################## 
#           금천구           #  
##############################
get_googlemap("geumcheongu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=geumcheon, aes(x=lon,y=lat,color="금천구 무인교통단속카메라 위치",shape="금천구 무인교통단속카메라 위치"))+
  geom_point(data=ma_geumcheon_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_geumcheon_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_geumcheon_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=bike_geumcheon_loc[[1]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_geumcheon_loc[[2]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=bike_geumcheon_loc[[3]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=older_geumcheon_loc[[1]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_geumcheon_loc[[2]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_geumcheon_loc[[3]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=death_geumcheon_loc,aes(x=경도, y=위도,color="사망교통사고 지역",shape="사망교통사고 지역"),fill="red",size=3)+
  scale_shape_manual(name="범례",limits=c("금천구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"),values=c(16,16,16,16,25))+
  scale_color_manual(name="범례",limits=c("금천구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"), values=c("black","red","orange","blue","black"))


############################## 
#           서초구           # #자전거데이터없음 
##############################  
get_googlemap("seochogu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=seocho, aes(x=lon,y=lat,color="서초구 무인교통단속카메라 위치",shape="서초구 무인교통단속카메라 위치"))+
  geom_point(data=ma_seocho_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_seocho_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_seocho_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=older_seocho_loc[[1]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_seocho_loc[[2]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=death_seocho_loc,aes(x=경도, y=위도,color="사망교통사고 지역",shape="사망교통사고 지역"),fill="red",size=3)+
  scale_shape_manual(name="범례",limits=c("서초구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"),values=c(16,16,16,16,25))+
  scale_color_manual(name="범례",limits=c("서초구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"), values=c("black","red","orange","blue","black"))


############################## 
#         서대문구           #  
##############################
get_googlemap("seodaemungu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=seodaemun, aes(x=lon,y=lat,color="서대문구 무인교통단속카메라 위치",shape="서대문구 무인교통단속카메라 위치"))+
  geom_point(data=ma_seodaemun_loc[[1]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_seodaemun_loc[[2]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=ma_seodaemun_loc[[3]],aes(x=lon, y = lat,color="사고다발 지역",shape="사고다발 지역"))+
  geom_point(data=bike_seodaemun_loc[[1]],aes(x=lon, y=lat,color="자전거사고 지역",shape="자전거사고 지역"))+
  geom_point(data=older_seodaemun_loc[[1]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_seodaemun_loc[[2]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_seodaemun_loc[[3]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_seodaemun_loc[[4]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_seodaemun_loc[[5]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_seodaemun_loc[[6]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=older_seodaemun_loc[[7]],aes(x=lon,y=lat,color="노인보행사고 지역",shape="노인보행사고 지역"))+
  geom_point(data=death_seodaemun_loc,aes(x=경도, y=위도,color="사망교통사고 지역",shape="사망교통사고 지역"),fill="red",size=3)+
  scale_shape_manual(name="범례",limits=c("서대문구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"),values=c(16,16,16,16,25))+
  scale_color_manual(name="범례",limits=c("서대문구 무인교통단속카메라 위치","사고다발 지역","자전거사고 지역","노인보행사고 지역","사망교통사고 지역"), values=c("black","red","orange","blue","black"))
