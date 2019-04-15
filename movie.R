library(dplyr)
library(tidyr)
library(tibble)
library(recommenderlab)
library(reshape2)
library(data.table)
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

movie_sample<-movie[sample(1:nrow(movie),5000),] #용량문제때문에 샘플링
movie_sample<-movie_sample %>%
  arrange(userId)
head(movie_sample,20)

movie_mat<-spread(movie_sample,title,rating) %>%
  remove_rownames() %>%
  column_to_rownames(var="userId")
# list형태임 movie_mat[[1]] 이런 식으로 확인 가능 
# movie_mat <- dcast(movie_sample, userId~title, value.var = "rating", na.rm=FALSE) 위와 동일한 작업 

movie_rrm <- as(as(movie_mat, "matrix"), "realRatingMatrix")
# as(movie_rrm,"list")
# as(movie_rrm[1:5],"list")
rating_eval <- evaluationScheme(movie_rrm, method="split", train=0.7, given=1)

#####1 UBCF#####
ubcf_rmse <- Recommender(getData(rating_eval, "train"), method = "UBCF", 
                         param=list(normalize = "center", method="Cosine")) #method:cosine, pearson 유사도 선정방식
ubcf_pred <- predict(object = ubcf_rmse, newdata = movie_rrm,  n = 5)
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


