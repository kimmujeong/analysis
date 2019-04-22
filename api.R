library(XML)
library(data.table)
library(stringr) #str_extract_all
library(dplyr)
api_url<-"http://apis.data.go.kr/B552061/frequentzoneLg/getRestFrequentzoneLg"
servicekey<-"oxrnAfkJN4PXQnHw7cNJANXNzXxIz6B6ebL8oWffyvb9nb5mqpQX4ANZBzFnUmhG2XhKl9PQ8TDQ7u5xY974WQ%3D%3D"
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
#########################################################
###################단위테스트##########################
url<-paste0(api_url,"?","serviceKey=",servicekey,"&searchYearCd=",2017,"&siDo=",11,"&guGun=",305,"&type=","xml","&numOfRows=",10,"&pageNo=",1)
raw.data <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "utf-8")
rootNode <- xmlRoot(raw.data)
items <- rootNode[[2]][['items']]
item_temp <- xmlSApply(items[[3]],xmlValue) #지금 테스트는 총 3개의 아이템으로 이루어져 있음 각각접근해야됨 [[1]], [[2]], [[3]]
# item_temp[1]
# item_temp['afos_fid'] #번호로 찾기 힘들 때 이렇게 하면 됨
loc_200<-item_temp['geom_json'] #item_temp[13]
loc_200<-substr(loc_200,33+2,nchar(loc_200)-3) #쓸데없는 문자열 삭제
# strsplit(loc_200,'[[]]') #그냥 공백사라짐..
loc_200<-str_extract_all(loc_200,'(?<=\\[)[0-9]+[.?0-9]+,[0-9]+[.0-9]+')

loc_200[[1]] #1~33까지
length(loc_200[[1]]) #데이터갯수확인 
row_length=length(loc_200[[1]]) #데이터 갯수를 로우숫자로 지정 
location_200<-data.frame(matrix(nrow=row_length, ncol=2))

for (i in 1:row_length){
  for (j in 1:2){
    tmp<-str_split(loc_200[[1]][i],',')
    location_200[i,j]<-tmp[[1]][j]
  }
}

location_200
location_200<-lapply(location_200,as.double) #문자형태를 숫자형태로 변환 
location_200<-data.frame(location_200$V1,location_200$V2) #다시 데이터 프레임 형태로 , v인지 x인지 확인해야 함 
location_200<-setNames(data.frame(location_200),c("lon","lat")) #dplyr 이용해서 rename(,"lon"=V1,"lat"=V2)가능
#########################################################

#########################################################
###########################for문 처리 완료#####################################
url<-paste0(api_url,"?","serviceKey=",servicekey,"&searchYearCd=",2017,"&siDo=",11,"&guGun=",305,"&type=","xml","&numOfRows=",10,"&pageNo=",1)
raw.data <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "utf-8")
rootNode <- xmlRoot(raw.data)
items <- rootNode[[2]][['items']]

size<-xmlSize(items)
item<-list()
loc<-list()

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
  loc[[i]]<-location_df
}
#########################################################


##################function 테스트 힌트...##############################
test_function(1)  
test_function("gangbuk") #function으로 만들어서 loc->"입력 스트링"
#########################################################

#########################################################
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
bike_file<-read.csv("C:\\Users\\thgus\\NaverCloud\\Downloads\\교통사고 스팟\\도로교통공단_전국_자전거사고다발지(2017년).csv")
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
#####

############단위테스트############
grep('^11305', bike_file$법정동코드)
bike_file[grep('^11305', bike_file$법정동코드),]

bike_file %>%
  filter(grepl('^11305',법정동코드)) #grep, grepl 차이... 
'grep을 썼을때는 행번호를 출력하고 grepl은 논리값(true, false)을 출력한다
필터에서는 논리값을 필요로하므로 grepl을 써야함!!!'

bike_gangbuk<-bike_file %>% 
  filter(str_detect(법정동코드, "^11305"))
bike_gangbuk$다발지역폴리곤[10]
polygon<-bike_gangbuk$다발지역폴리곤[1] #폴리곤 nrow(bike_gangbuk)만큼 있음
str<-as.character(polygon) #문자열로 타입 변환
str<-substr(str,33+2,nchar(str)-2) #쓸데없는 표현정리
str_list<-str_extract_all(str,'(?<=\\[)[0-9]+[.?0-9]+,[0-9]+[.0-9]+')
str_list[[1]]
length(str_list[[1]]) #데이터갯수확인 
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
bike_location<-setNames(data.frame(bike_location),c("lon","lat")) #dplyr 이용해서 rename(,"lon"=V1,"lat"=V2)가능
bike_location

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
bike_seocho_loc<-bike_function("seocho") #파일에 서초구 존재하지 않음
bike_seodaemun_loc<-bike_function("seodaemun") #1

########################################################################
########################################################################
########보행노인 사고 다발지역
older_file<-read.csv("C:\\Users\\thgus\\NaverCloud\\Downloads\\교통사고 스팟\\도로교통공단_전국_보행노인사고다발지(2017년).csv")

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
######################
older_gangbuk_loc<-older_function("gangbuk") #6
older_jongno_loc<-older_function("jongno") #7
older_gwanak_loc<-older_function("gwanak") #7
older_eunpyeong_loc<-older_function("eunpyeong") #7
older_gangdong_loc<-older_function("gangdong") #7
older_geumcheon_loc<-older_function("geumcheon") #3
older_seocho_loc<-older_function("seocho") #2
older_seodaemun_loc<-older_function("seodaemun") #7



########################################################################
########################################################################
########사망교통사고
death_file<-read.csv("C:\\Users\\thgus\\NaverCloud\\Downloads\\교통사고 스팟\\도로교통공단_전국_사망교통사고_2017.csv")
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

