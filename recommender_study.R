x1<-rnorm(30)
x2<-rnorm(30)
Euc_dist=dist(rbind(x1,x2), method="euclidean")

vec1=c(1,1,1,0,0,0,0,0,0,0,0,0)
vec2=c(0,0,1,1,1,1,1,0,1,0,0,0)
library(lsa)
cosine(vec1,vec2)

Coef=cor(mtcars, method="pearson")

#미국 주별 범죄 데이터
rownames(USArrests)
names(USArrests)
apply(USArrests,2,var) #분산 확인
pca<- prcomp(USArrests,scale=TRUE) #정규화
names(pca)

#biplot의 방향을 변경하기 위해서 
pca$rotation<--pca$rotation
pca$x<--pca$x
biplot(pca,scale = 0)

data("MovieLense")
as(MovieLense[1:5],"list")
dimnames(MovieLense)
similarity_users2<-similarity(MovieLense[1:5,],method="cosine",which="users")
as.matrix(similarity_users2)
similarity_items2<-similarity(MovieLense[,1:5],method="cosine",which="items")
as.matrix(similarity_items2)

rating_movies<-MovieLense[rowCounts(MovieLense)>50, colCounts(MovieLense)>100]
which_train<-sample(x=c(TRUE,FALSE),size=nrow(rating_movies),replace=TRUE,prob=c(0.8,0.2))
recc_data_train<-rating_movies[which_train,]
recc_data_test<-rating_movies[!which_train,]
recc_model<-Recommender(data=recc_data_train,method="IBCF",parameter=list(k=30))
recc_predicted<-predict(recc_model,recc_data_test,n=6)
recc_user_1_book<-recc_predicted@items[[1]]
movies_user_1_book<-recc_predicted@itemLabels[recc_user_1_book]
recc_matrix_book<-sapply(recc_predicted@items, function(x){colnames(rating_movies)[x]})
recc_matrix_book[,1:4]
table(recc_matrix_book)
typeof(recc_matrix_book)
class(recc_matrix_book)
rowCounts(rating_movies)
df<-data.frame(rowCounts(rating_movies))
df %>%
  arrange(rowCounts.rating_movies.)
