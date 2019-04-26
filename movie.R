library(dplyr)
library(tidyr)
library(tibble)
library(recommenderlab)
library(reshape2)
library(data.table)
library(ggplot2)
rating<-read.csv("C:\\Users\\thgus\\Downloads\\movie\\ratings.csv") #500100
movies<-read.csv("C:\\Users\\thgus\\Downloads\\movie\\movies.csv",header = TRUE)
#users<-read.csv("C:\\Users\\thgus\\Downloads\\movie\\users.dat",header=FALSE)
#tags 데이터 활용해서 tf-idf로 추천가능해보임
#추천모델가능한 모든 것 #ALS, ALS_implicit, IBCF, POPULAR, RANDOM, RECOMMEND, SVD, SVDF, UBCF
head(rating,20)
head(movies)
# which(movies$title=="[REC]쨀 3 G챕nesis (2012)") 오류값 찾기
head(movies)
genres<-as.data.frame(movies$genres)
head(genres)

####################################################################
##############장르매트릭스 만들기######
#https://rpubs.com/jeknov/movieRec
genres2<-as.data.frame(tstrsplit(genres[,1], '[|]', 
                                 type.convert=TRUE), 
                       stringsAsFactors=FALSE)
head(genres2)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_matrix <- matrix(0,10330,18) #empty matrix, 10330=no of movies+1, 18=no of genres
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers
genre_matrix2
#################################################################


movie<-rating %>%
  left_join(movies,by="movieId") %>%
  select(userId,rating,title)

# movie_sample<-movie[sample(1:nrow(movie),5000),] #용량문제때문에 샘플링
# movie_sample<-movie_sample %>%
#   arrange(userId)
# head(movie_sample,20)

movie<-movie[-c(17820,80597,4748,11452),] #중복값제거 

movie_mat<-spread(movie,title,rating) %>%
  remove_rownames() %>%
  column_to_rownames(var="userId")
# list형태임 movie_mat[[1]] 이런 식으로 확인 가능 
# movie_mat <- dcast(movie_sample, userId~title, value.var = "rating", na.rm=FALSE) 위와 동일한 작업 

movie_rrm <- as(as(movie_mat, "matrix"), "realRatingMatrix") #610x9719 610명의 사용자, 9719개의 영화 
# as(movie_rrm,"list")
# as(movie_rrm[1:5],"list")

similarity_users<-similarity(movie_rrm[1:5],method="cosine",which="users") #처음 다섯명의 사용자간 유사성
similarity_items<-similarity(movie_rrm[,1:5],method = "cosine",which="items") #처음 다섯개의 아이템간 유사성. items라고 명시해야함 
as.matrix(similarity_users)
as.matrix(similarity_items)

#평점 값 탐구
vector_rating<-as.vector(movie_rrm@data)
unique(vector_rating)
table_ratings<-table(vector_rating)
typeof(table_ratings)
df_ratings<-as.data.frame(table_ratings)
df_ratings<-subset(df_ratings,vector_rating!=0) #0값 제거
options("scipen" = 100)
ggplot(data=df_ratings,aes(x=vector_rating,y=Freq))+geom_bar(stat="identity") #분포 확인

#영화 별 관람횟수 조회 
view_per_movie<-colCounts(movie_rrm)
df_view_per_movie<-data.frame(movie=names(view_per_movie),views=view_per_movie)
rownames(df_view_per_movie)<-NULL #로우이름제거 
head(df_view_per_movie)
df_view_per_movie<-df_view_per_movie %>% #가장많은 본 횟수대로 정렬
  arrange(desc(views))
df_view_per_movie %>% #하나의 영화별 관람횟수 평균 약 10회
  summarise(mean(views))
ggplot(data=df_view_per_movie[1:5,],aes(x=movie,y=views))+geom_bar(stat="identity")

#평균 평점 탐색
avg_rating<-colMeans(movie_rrm)

table_avg_rating<-table(avg_rating) #평균 평점 그 자체의 분포확인
df_table_avg_rating<-as.data.frame(table_avg_rating)
df_table_avg_rating<-df_table_avg_rating %>%
  arrange(desc(Freq))

df_avg_rating<-data.frame(movie=names(avg_rating),avg=avg_rating) #각 영화의 평균 평점
rownames(df_avg_rating)<-NULL
df_avg_rating<-df_avg_rating %>%
  arrange(desc(avg))
head(df_avg_rating)
ggplot(data=df_avg_rating,aes(x=avg))+geom_bar(stat="identity")
ggplot(data=df_avg_rating[1:5,],aes(x=movie,y=avg))+geom_bar(stat="identity")

#사용자별 영화관람 횟수
view_per_user<-rowCounts(movie_rrm)
df_view_per_user<-data.frame(userid=names(view_per_user),view=view_per_user)
row.names(df_view_per_user)<-NULL
head(df_view_per_user)
df_view_per_user %>% #사용자별 평균 관람횟수.. 약 165회
  summarise(mean(view))
  
#영화를 165편 이상 평가한 사용자, 적어도 10회 이상 시청된 영화로 데이터처리
movie_rrm<-movie_rrm[rowCounts(movie_rrm)>=165, colCounts(movie_rrm)>=10] #159x2269 159명의 사용자, 2269개의 영화


rating_eval <- evaluationScheme(movie_rrm, method="split", train=0.7, given=1)

#####1 UBCF#####
ubcf_model <- Recommender(getData(rating_eval, "train"), method = "UBCF", 
                         param=list(normalize = "center", method="Cosine")) #method:cosine, pearson 유사도 선정방식
ubcf_pred <- predict(object = ubcf_model, newdata = movie_rrm,  n = 5) #여기서 내가 잘못한거같은데.. movie_rrm이 아니라 getData "known"값을 줘서 가져와야하는것같다 
ubcf_pred_list<-as(ubcf_pred,"list")
ubcf_pred_list

#data table 형태로
user_id<-names(ubcf_pred_list) #리스트 이름 가져오기=user_id 가져오기
ubcf_df<-data.frame(matrix(nrow=544,ncol=5)) #빈 df생성 

for(i in 1:544){
  for(j in 1:5){
    if(identical(ubcf_pred_list[[i]],character(0))){ #character(0)인 부분이 있어서 판별
      ubcf_df[i,]<-NA
    } 
    else{
      ubcf_df[i,j]<-unlist(ubcf_pred_list[i],use.names = FALSE)[j]
    }
  }
}

ubcf_df %>%
  DT::datatable()

#############################################################################################################
#############################################################################################################
#############################################################################################################
######책

movie_rrm_norm<-normalize(movie_rrm) # 정규화
movie_rrm_watched<-binarize(movie_rrm, minRating=0.5) "최저평점인 0.5이상인 경우를 모두 1로 정의하는 매트릭스
                                                        =즉 평가한 모든 영화에 대해서는 1로 정의함"

rating_eval <- evaluationScheme(movie_rrm, method="split", train=0.7, given=5)
##############
#####IBCF#####
##############
ibcf_model <- Recommender(getData(rating_eval, "train"), method = "IBCF", param=list(k=30))
ibcf_pred <- predict(ibcf_model, getData(rating_eval, "known"), n=6) #6개를 추천해주겠다. 책에서는 type이 디폴트로
recc_user_1<-ibcf_pred@items[[1]] #사용자1에게 추천된 아이템
movie_user_1<-ibcf_pred@itemLabels[recc_user_1] #사용자1에게 추천된 영화추출
as(ibcf_pred,"list")

####DT####
ibcf_pred_list<-as(ibcf_pred,"list")
user_id<-names(ibcf_pred_list) #리스트 이름 가져오기=user_id 가져오기
ibcf_df<-data.frame(matrix(nrow=48,ncol=6)) #빈 df생성 

for(i in 1:48){
  for(j in 1:6){
    if(identical(ibcf_pred_list[[i]],character(0))){ #character(0)인 부분이 있어서 판별
      ibcf_df[i,]<-NA
    } 
    else{
      ibcf_df[i,j]<-unlist(ibcf_pred_list[i],use.names = FALSE)[j]
    }
  }
}
rownames(ibcf_df)=user_id
ibcf_df %>%
  DT::datatable()
################

ibcf_df
trans_ibcf_df<-t(ibcf_df)
table(trans_ibcf_df)
ibcf_item_sort<-sort(factor(table(trans_ibcf_df)),decreasing = TRUE) #많은 추천된 영화순으로 정렬..
data.frame(ibcf_item_sort)
ibcf_sort_df<-data.frame(movie=names(ibcf_item_sort),count=ibcf_item_sort)
row.names(ibcf_sort_df)<-NULL

##############
#####UBCF#####
##############
ubcf_model<-Recommender(getData(rating_eval,"train"),method="UBCF")
ubcf_pred<-predict(ubcf_model, getData(rating_eval,"known"),n=6)

####DT####
ubcf_pred_list<-as(ubcf_pred,"list")
user_id<-names(ubcf_pred_list) #리스트 이름 가져오기=user_id 가져오기
ubcf_df<-data.frame(matrix(nrow=48,ncol=6)) #빈 df생성 

for(i in 1:48){
  for(j in 1:6){
    if(identical(ubcf_pred_list[[i]],character(0))){ #character(0)인 부분이 있어서 판별
      ubcf_df[i,]<-NA
    } 
    else{
      ubcf_df[i,j]<-unlist(ubcf_pred_list[i],use.names = FALSE)[j]
    }
  }
}
rownames(ubcf_df)=user_id
ubcf_df %>%
  DT::datatable()
################

ubcf_df
trans_ubcf_df<-t(ubcf_df)
table(trans_ubcf_df)
ubcf_item_sort<-sort(factor(table(trans_ubcf_df)),decreasing = TRUE)
ubcf_sort_df<-data.frame(movie=names(ubcf_item_sort),count=ubcf_item_sort)
row.names(ubcf_sort_df)<-NULL

##############################################################################################
#############################################모델평가#########################################
##############################################################################################




