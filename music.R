library(spotifyr)
library(dplyr)
library(stringr)
Sys.setenv(SPOTIFY_CLIENT_ID = 'f563448409d34e63ba3ed22cf6c710b0')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '17b11a571b4c4be9baa1a805bb1bb0c5')
access_token <- get_spotify_access_token()

#함수정리
#get_track_audio_features(곡 id) : 곡 id를 기준으로 곡 특성가져옴, 최대 100개까지 가져올수있ㅇ
#get_artist_audio_features(아티스트) : 아티스트 정규앨범만 가져옴. 곡에 대한 피쳐도 가져오는데 정규앨범만 가져오는게 문제
#get_artist_albums(앨범 id) : 앨범 정보 가져오기

#아티스트코드
넬:5WY88tCMFA6J6vqSN3MmDZ
bts:3Nrfpe0tUJi4K4DXYWgMUX

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

###########################################
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

nell_feature<-get_music_feature("5WY88tCMFA6J6vqSN3MmDZ")
bts_feature<-get_music_feature("3Nrfpe0tUJi4K4DXYWgMUX")
vibe_feature<-get_music_feature("68ym0sOo7MazZxScbm1wtI")
nell_feature %>%
  select(-(id)) %>%
  summarise_all(funs(mean))
bts_feature %>%
  select(-id) %>%
  summarise_all(funs(mean))
vibe_feature %>%
  select(-id) %>%
  summarise_all(funs(mean))
  