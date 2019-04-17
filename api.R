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
location_200<-data.frame(location_200$V1,location_200$V2) #다시 데이터 프레임 형태로 
location_200<-setNames(data.frame(location_200),c("lon","lat")) #dplyr 이용해서 rename(t1,"lon"=V1,"lat"=V2)가능

################
location_200_1<-location_200
location_200_2<-location_200
location_200_3<-location_200

get_googlemap("sungdonggu", maptype="roadmap", zoom=13) %>%
  ggmap+geom_point(data=location_200,aes(x=lon, y = lat), color='red')

head(location_200_1)
head(location_200_2)
head(location_200_3)
