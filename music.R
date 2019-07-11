library(spotifyr)
library(dplyr)
library(ggplot2)
library(tidyr) #gather
library(reshape2)
library(proxy) #코사인거리 구하기
Sys.setenv(SPOTIFY_CLIENT_ID = 'f563448409d34e63ba3ed22cf6c710b0')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '17b11a571b4c4be9baa1a805bb1bb0c5')
access_token <- get_spotify_access_token()

#함수정리
#get_track_audio_features(곡 id) : 곡 id를 기준으로 곡 특성가져옴, 최대 100개까지 가져올수있음
#get_artist_audio_features(아티스트) : 아티스트 정규앨범만 가져옴. 곡에 대한 피쳐도 가져오는데 정규앨범만 가져오는게 문제
#get_artist_albums(앨범 id) : 앨범 정보 가져오기
#get_track(곡 id) : 곡 id 정보 가져오기

#아티스트코드
#넬:5WY88tCMFA6J6vqSN3MmDZ
#bts:3Nrfpe0tUJi4K4DXYWgMUX
#바이브:68ym0sOo7MazZxScbm1wtI+1pjLQCQ1ck57Ml3lcoZ2Xi
#레드벨벳:1z4g3DjTBBZKhvAroFlhOM
#블랙핑크 41MozSoPIsD1dJM0CLPjZF
#exo 3cjEqqelV9zb4BYE3qDQ4O
#박효신 57htMBtzpppc1yoXgjbslj
#엠씨더맥스 3MaRWfwKpbYnkYHC5gRKYo
#잔나비 2SY6OktZyMLdOnscX3DCyS

#################하드코딩#########
#get_artist_audio_fratures : 정규앨범만 가져옴..
bts <- get_artist_audio_features('bts')
nell <- get_artist_audio_features('nell')

bts %>%
  select(album_name,track_name, danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo) %>%
  head(20)

nell %>%
  select(album_name,track_name, danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo) %>%
  head(20)

nell_album<-get_artist_albums("5WY88tCMFA6J6vqSN3MmDZ") #넬 앨범 가져오기
nell_album$name
nell_album_ids<-nell_album$id #넬 앨범id 값 가져오기

#앨범별로 구분해서 넣기
list<-list()
k=1
for (i in nell_album_ids){
  list[[k]]<-get_album_tracks(i) %>%
    select(id,name)
  k=k+1
}
list

#전체를 하나의 변수로 합치기, songid만
nell_all_song_id<-append(list[[1]]$id,list[[2]]$id)
for(i in 3:length(list)){
  nell_all_song_id<-append(nell_all_song_id,list[[i]]$id)
}

#get_track_audio_features
order_1<-nell_all_song_id[1:100]
order_2<-nell_all_song_id[101:length(nell_all_song_id)]
df1<-get_track_audio_features(order_1)
df2<-get_track_audio_features(order_2)
df<-rbind(df1,df2)
df<-df %>%
  select(-c(type,uri,track_href,analysis_url))

#넬 노래의 특성 평균값
df %>%
  select(-(id)) %>%
  summarise_all(funs(mean))

song_df<-data.frame()
for(i in nell_feature$id){
  tmp<-bind_cols(as.data.frame(get_track(i)$name), as.data.frame(get_track(i)$id))
  colnames(tmp)<-c("name","id")
  tmp$name<-as.character(tmp$name)
  tmp$id<-as.character(tmp$id)
  song_df<-bind_rows(song_df,tmp)
}
song_df

get_album_tracks("43xVSlBAB2WgX7d0R4uQG2")

##### 위와 동일한 결과값
# song_df<-data.frame()
# for(i in nell_feature$id){
#   tmp<-cbind(get_track(i)$name, get_track(i)$id)
#   song_df<-rbind(song_df,tmp)
# }


###########################################

################분석 1#####################
################함수만들기#################
get_music_feature<-function(artistid){
  album<-get_artist_albums(artistid) #앨범 가져오기
  album_ids<-album$id #앨범id 값 가져오기

    #앨범별로 구분해서 넣기
  list<-list()
  k=1
  for (i in album_ids){
    list[[k]]<-get_album_tracks(i) %>%
      select(id,name)
    k=k+1
  }
  
  #전체를 하나의 변수로 합치기, songid만 
  all_song_id<-append(list[[1]]$id,list[[2]]$id)
  for(i in 3:length(list)){
    all_song_id<-append(all_song_id,list[[i]]$id)
  }
  
  if(length(all_song_id)%%100==0) #곡 수가 100 단위면 딱 떨어짐
    number<-length(all_song_id)%/%100
  else                            #곡 수가 100단위가 아니면 몫+1해줘야함
    number<-(length(all_song_id)%/%100)+1
  
  tmp<-list()
  if(number==1) #전체 곡 수가 100곡이 안되는 경우
    tmp[[1]]<-all_song_id[1:length(all_song_id)]
  
  else{ #전체 곡 수가 100곡이 넘는 경우 
    for(j in 1:(number-1)){
        tmp[[j]]<-all_song_id[(1+100*(j-1)):(100*j)]
    }
    tmp[[number]]<-all_song_id[(1+100*(number-1)):length(all_song_id)]
  }
  
  result<-data.frame()
  for(k in 1:number){
    result<-rbind(result,get_track_audio_features(tmp[[k]])) #rbind도 가능하고 bind_rows도 가능 
  }
  
  result<-result %>%
    select(-c(type,uri,track_href,analysis_url))
  
  return(result)
}

###############함수만들기2################
#곡 이름까지 가져오는 함수, 조금 시간이 걸림
get_music_feature2<-function(artistid){
  album<-get_artist_albums(artistid) #앨범 가져오기
  album_ids<-album$id #앨범id 값 가져오기
  
  #앨범별로 구분해서 넣기
  list<-list()
  k=1
  for (i in album_ids){
    list[[k]]<-get_album_tracks(i) %>%
      select(id,name)
    k=k+1
  }
  
  #전체를 하나의 변수로 합치기, songid만 
  all_song_id<-append(list[[1]]$id,list[[2]]$id)
  for(i in 3:length(list)){
    all_song_id<-append(all_song_id,list[[i]]$id)
  }
  
  if(length(all_song_id)%%100==0) #곡 수가 100 단위면 딱 떨어짐
    number<-length(all_song_id)%/%100
  else                            #곡 수가 100단위가 아니면 몫+1해줘야함
    number<-(length(all_song_id)%/%100)+1
  
  tmp<-list()
  if(number==1) #전체 곡 수가 100곡이 안되는 경우
    tmp[[1]]<-all_song_id[1:length(all_song_id)]
  
  else{ #전체 곡 수가 100곡이 넘는 경우 
    for(j in 1:(number-1)){
      tmp[[j]]<-all_song_id[(1+100*(j-1)):(100*j)]
    }
    tmp[[number]]<-all_song_id[(1+100*(number-1)):length(all_song_id)]
  }
  
  result<-data.frame()
  for(k in 1:number){
    result<-rbind(result,get_track_audio_features(tmp[[k]])) #rbind도 가능하고 bind_rows도 가능 
  }
  
  result<-result %>%
    select(-c(type,uri,track_href,analysis_url))
  
  #곡 이름 가져오기
  song_nameid<-data.frame()
  for(i in result$id){
    tmp<-bind_cols(as.data.frame(get_track(i)$name), as.data.frame(get_track(i)$id))
    colnames(tmp)<-c("name","id")
    tmp$name<-as.character(tmp$name)
    tmp$id<-as.character(tmp$id)
    song_nameid<-bind_rows(song_nameid,tmp)
  }
  
  result<-result %>%
    inner_join(song_nameid,by="id")
  
  return(result)
}
###########################################

############분석 1코드#############
nell_feature<-get_music_feature("5WY88tCMFA6J6vqSN3MmDZ")
bts_feature<-get_music_feature("3Nrfpe0tUJi4K4DXYWgMUX")
vibe_feature<-get_music_feature("68ym0sOo7MazZxScbm1wtI")
vibe_feature2<-get_music_feature("1pjLQCQ1ck57Ml3lcoZ2Xi")
red_feature<-get_music_feature("1z4g3DjTBBZKhvAroFlhOM")
black_feature<-get_music_feature("41MozSoPIsD1dJM0CLPjZF")
exo_feature<-get_music_feature("3cjEqqelV9zb4BYE3qDQ4O")
bhs_feature<-get_music_feature("57htMBtzpppc1yoXgjbslj")
mc_feature<-get_music_feature("3MaRWfwKpbYnkYHC5gRKYo")
jan_feature<-get_music_feature("2SY6OktZyMLdOnscX3DCyS")

nell_feature<-nell_feature[1:(nrow(nell_feature)-12),]
vibe_feature<-rbind(vibe_feature,vibe_feature2)
bhs_feature<-bhs_feature[1:119,]
mc_feature<-mc_feature[1:227,]
jan_feature<-jan_feature[1:48,]

#feature 평균 함수 
feature_mean<-function(artist_feature){
  artist_feature %>%
    select(-c(id,loudness,tempo,duration_ms,time_signature,key)) %>%
    summarise_all(funs(mean))
}

nell_mean<-feature_mean(nell_feature)
bts_mean<-feature_mean(bts_feature)
vibe_mean<-feature_mean(vibe_feature)
red_mean<-feature_mean(red_feature)
black_mean<-feature_mean(black_feature)
exo_mean<-feature_mean(exo_feature)
bhs_mean<-feature_mean(bhs_feature)
mc_mean<-feature_mean(mc_feature)
jan_mean<-feature_mean(jan_feature)

#gather
gather_nell_feature<-gather(nell_mean,feature, value)
gather_bts_feature<-gather(bts_mean,feature,value)
gather_vibe_feature<-gather(vibe_mean,feature,value)
gather_red_feature<-gather(red_mean,feature,value)
gather_black_feature<-gather(black_mean,feature,value)
gather_exo_feature<-gather(exo_mean,feature,value)
gather_bhs_feature<-gather(bhs_mean,feature,value)
gather_mc_feature<-gather(mc_mean,feature,value)
gather_jan_feature<-gather(jan_mean,feature,value)


#그래프
merge_data<-rbind(gather_nell_feature,gather_bts_feature,gather_vibe_feature)
merge_data$data<-c(rep("nell", nrow(gather_nell_feature)), rep("bts",nrow(gather_bts_feature)), rep("vibe", nrow(gather_vibe_feature)))
ggplot(merge_data,aes(x=feature, y=value, fill=data))+geom_col(position = "dodge")

merge_data2<-rbind(gather_nell_feature,gather_bts_feature,gather_vibe_feature,gather_red_feature,gather_black_feature,gather_exo_feature,
                   gather_bhs_feature,gather_mc_feature,gather_jan_feature)
merge_data2$data<-c(rep("nell", nrow(gather_nell_feature)), rep("bts",nrow(gather_bts_feature)), rep("vibe", nrow(gather_vibe_feature)),
                    rep("redverbet",nrow(gather_red_feature)), rep("blackpink", gather_black_feature), rep("exo" ,gather_exo_feature), 
                    rep("박효신", gather_bhs_feature), rep("mcthemax", gather_mc_feature), rep("잔나비", gather_jan_feature))
ggplot(merge_data2,aes(x=feature, y=value, fill=data))+geom_col(position = "dodge")


###########클러스터링
###계층적 군집분석
#클러스터링에 맞는 데이터프레임으로 고치기
cluster_df<-as.data.frame(rbind(nell_mean,bts_mean,vibe_mean,red_mean,black_mean,exo_mean,bhs_mean,mc_mean,jan_mean))
rownames(cluster_df)<-c("nell","bts","vibe","redvelvet","blackpink","exo","박효신","mc the max","잔나비")

d<-dist(cluster_df) #거리계산

fit.average<-hclust(d, method = "average")
plot(fit.average, hang=-1, cex=.8, main="Average Linkage Clustering")

#최종 군집 방안 구하기
clusters<-cutree(fit.average,k=3) #클러스터 갯수:k 
table(clusters)
aggregate(cluster_df, by=list(cluster=clusters), median)
plot(fit.average, hang=-1, cex=.8)
rect.hclust(fit.average,k=3) #네모박스로 구별해줌 
###############################


#############분석 2############
#####곡정보####
#1 우린너무멀리있다 https://open.spotify.com/track/48cc6ddojyx8EPlDrIeHlp
#2 나만 안되는 연애 https://open.spotify.com/track/2uaC8QIz0OocPl8bFcNFt0
#3 기다린 만큼 더 https://open.spotify.com/track/03YtYSkpHqqY3EBmHJkTjP
#4 영원+1 https://open.spotify.com/track/0cI7LmBgwXN4Sv7W2zWsD3
#5 착각 https://open.spotify.com/track/7c8cPVLWvtZwxDxA3KkWFP
#6 신청곡 https://open.spotify.com/track/57jBcQUCJaIUTRbqBmW7D1
#7 하루 https://open.spotify.com/track/3jEEkgLOxZLg2mcteCLTwA
#8 눈의 꽃 https://open.spotify.com/track/2fRFwWwZG7Qfkui7GcxTMy
#9 not going anywhere https://open.spotify.com/track/1lv17MSSDr1iBS2vp6UoRt
#10 미워하자 https://open.spotify.com/track/6udwOVrx63wvimCGhc89fU
#11 오래된 노래 https://open.spotify.com/track/4zgZ7dq1BcrhmLRA8bPF4c
#12 물론 https://open.spotify.com/track/1S3Qtj2QGy4KQKbtOZZQ7d
#13 슬픔 속에 그댈 지워야만 해 https://open.spotify.com/track/4nb9rDtnLe51Z2Nzy0liQH
#14 니가 빈 자리 https://open.spotify.com/track/5X3BdhFqPDptGjIwaCUs38
###############

############분석 2코드#########
songlist<-c("48cc6ddojyx8EPlDrIeHlp","2uaC8QIz0OocPl8bFcNFt0","03YtYSkpHqqY3EBmHJkTjP","0cI7LmBgwXN4Sv7W2zWsD3","7c8cPVLWvtZwxDxA3KkWFP",
            "57jBcQUCJaIUTRbqBmW7D1","3jEEkgLOxZLg2mcteCLTwA","2fRFwWwZG7Qfkui7GcxTMy","1lv17MSSDr1iBS2vp6UoRt","6udwOVrx63wvimCGhc89fU",
            "4zgZ7dq1BcrhmLRA8bPF4c","1S3Qtj2QGy4KQKbtOZZQ7d","4nb9rDtnLe51Z2Nzy0liQH","5X3BdhFqPDptGjIwaCUs38")

#필요없는 속성 버리기
song_delete_feature<-function(song_feature){
  song_feature %>%
    select(-c(id,loudness,duration_ms,time_signature,key,id,uri,track_href,analysis_url,duration_ms,type,mode,tempo,instrumentalness))
}

#변수 할당, song[i]_feature, song_total
song_total<-data.frame()
for(i in 1:length(songlist)){
  var<-paste("song",i,"_feature",sep="")
  assign(var,song_delete_feature(get_track_audio_features(songlist[i])))
  song_total<-rbind(song_total, song_delete_feature(get_track_audio_features(songlist[i])))
}

#gather_song
for(i in 1:nrow(song_total)){
  gather_var<-paste("gather_song",i,sep="")
  assign(gather_var,gather(song_total[i,], feature, value))
}

#그래프
song_df_graph<-data.frame()
gather_song_df<-rbind(gather_song1,gather_song2,gather_song3, gather_song4, gather_song5, gather_song6, gather_song7, gather_song8, gather_song9,
      gather_song10,gather_song11,gather_song12,gather_song13,gather_song14)
gather_song_df$data<-c(rep("song1", nrow(gather_song1)),rep("song2", nrow(gather_song2)),rep("song3", nrow(gather_song3)),
                       rep("song4", nrow(gather_song4)),rep("song5", nrow(gather_song5)),rep("song6", nrow(gather_song6)),
                       rep("song7", nrow(gather_song7)),rep("song8", nrow(gather_song8)),rep("song9", nrow(gather_song9)),
                       rep("song10", nrow(gather_song10)),rep("song11", nrow(gather_song11)),rep("song12", nrow(gather_song12)),
                       rep("song13", nrow(gather_song13)),rep("song14", nrow(gather_song14)))
gather_song_df$data<-factor(gather_song_df$data, levels=c("song1","song2","song3","song4","song5","song6","song7","song8","song9",
                                                          "song10","song11","song12","song13","song14"))
ggplot(gather_song_df)+geom_col(aes(x=feature,y=value,fill=data),position = "dodge")

########코사인 거리구하기, 0에 가까울수록 비슷함.
song_total_df<-as.data.frame(song_total)
song_dist<-as.matrix(dist(song_total_df,method = "cosine")) #대체로 0에 가깝게 나옴. 추천에 적용할 수 있을 것으로 판단됨

#신나는 노래와 비교해보기
bts_title<-get_track_audio_features("5KawlOMHjWeUjQtnuRs22c")
bts_title<-bts_title %>%
  select(-c(id,loudness,duration_ms,time_signature,key,id,uri,track_href,analysis_url,duration_ms,type,mode,tempo,instrumentalness))

test<-rbind(bts_title,song_total_df[13,]) #슬픔속에그댈지워야만해
as.matrix(dist(test, method = "cosine"))
######################################

