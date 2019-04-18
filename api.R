library(XML)
library(data.table)
library(stringr)
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
'
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


###############################################################################
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
###############################################################################


##################function 테스트 힌트...##############################
test_function(1)  
test_function("gangbuk") #function으로 만들어서 loc->"입력 스트링"
########################################################################

########################################################################
#######################function만들기##################################
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
########################################################################

ma_gangbuk_loc<-many_accident_function("gangbuk")
ma_jongno_loc<-many_accident_function("jongno")
ma_gwanak_loc<-many_accident_function("gwanak")
ma_eunpyeong_loc<-many_accident_function("eunpyeong")
ma_gangdong_loc<-many_accident_function("gangdong")
ma_geumcheon_loc<-many_accident_function("geumcheon")
ma_seocho_loc<-many_accident_function("seocho")
ma_seodaemun_loc<-many_accident_function("seodaemun")


