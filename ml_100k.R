library(recommenderlab)
library(ggplot2)
library(reshape)
library(stringr)
library(stats4)


setwd("~/Downloads/我爱学习/Lancaster/term3/data/movie_lens/ml-100k")
ml100k <- read.table("u.data", header = F, stringsAsFactors = T)
movie_item<-u[,-4]
colnames(movie_item)<-c('movie_id',"title","release_date","URL","unknown","Action","Adventure","Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy","Film-Noir","Horror","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western")
movie_item[1]<-paste0('m',seq(nrow(movie_item)))
View(movie_item)
str_sub(a,-6)
movie_item$year<-str_sub(as.character(movie_item$title),-5)
movie_item$year<-str_sub(movie_item$year,1,4)


str_locate(a,"1")-2
str_extract("({*})",a)
a


movie_users<-u
View(movie_users)
colnames(movie_users)<-c('user_id','age','gender','occupation','ZipCode')
movie_users[1]<-paste0('u',seq(nrow(movie_users)))
View(ml100k)
#col1： user id，第二列是 item id，第三列是 rating，第四列是时间戳，时间戳useless here
ml100k <- ml100k[, -4]

#rating 的分布情况
prop.table(table(ml100k[, 3]))
summary(ml100k[, 3])

#转换成user-item ratingMatrix格式
ml.useritem <- cast(ml100k, V1 ~ V2, value = "V3")[,-1]
ml.useritem[1:3, 1:6]

#预处理完了，开始分析了！！！！小板凳排排坐
class(ml.useritem)
#"cast_df"    "data.frame"
class(ml.useritem) <- "data.frame" ##只保留data.frame的类属性
ml.useritem <- as.matrix(ml.useritem)
ml.ratingMatrix <- as(ml.useritem, "realRatingMatrix")  ##转换为realRatingMatrix
#943 x 1683 rating matrix of class ‘realRatingMatrix’ with 100943 ratings.

as(ml.ratingMatrix , "matrix")[1:3, 1:10]
as(ml.ratingMatrix , "list")[[1]][1:10]


#六种推荐方法介绍
recommenderRegistry$get_entries(dataType = "realRatingMatrix")


#建立推荐系统
#recommender() 是 recommenderlab 包中用于建立模型的函数，用法也相当简单
#!!!!!注意在调用 recommender() 之前需给矩阵的所有列按照 itemlabels 进行列命名!!!!!!!
colnames(ml.ratingMatrix) <- paste("M", 1:1682, sep = "")
as(ml.ratingMatrix[1,1:10], "list")


ml.recommModel <- Recommender(ml.ratingMatrix[1:800], method = "IBCF")
ml.recommModel

ml.predict1 <- predict(ml.recommModel, ml.ratingMatrix[801:803], n = 5)
ml.predict1
as( ml.predict1, "list")

##用户对item的评分预测
ml.predict2 <- predict(ml.recommModel, ml.ratingMatrix[801:803], type = "ratings")
ml.predict2
### 查看三个用于对M1-6的预测评分
## 注意：实际的预测评分还要在此基础上加上用户的平均评分
as(ml.predict2, "matrix")[1:3, 1:5]



#模型评估
rmse <- function(actuals, predicts) {
  sqrt(mean((actuals - predicts)^2, na.rm = T))
}

model.eval <- evaluationScheme(ml.ratingMatrix[1:943], method = "split", train = 0.9, given = 15, goodRating = 5)
model.eval

##分别用RANDOM、UBCF、IBCF建立预测模型
model.random <- Recommender(getData(model.eval, "train"), method = "RANDOM")
model.ubcf <- Recommender(getData(model.eval, "train"), method = "UBCF")
model.ibcf <- Recommender(getData(model.eval, "train"), method = "IBCF")

##分别根据每个模型预测评分
predict.random <- predict(model.random, getData(model.eval, "known"), type = "ratings")
predict.ubcf <- predict(model.ubcf, getData(model.eval, "known"), type = "ratings")
predict.ibcf <- predict(model.ibcf, getData(model.eval, "known"), type = "ratings")


error <- rbind(
  calcPredictionAccuracy(predict.random, getData(model.eval, "unknown")),
  calcPredictionAccuracy(predict.ubcf, getData(model.eval, "unknown")),
  calcPredictionAccuracy(predict.ibcf, getData(model.eval, "unknown")))
rownames(error) <- c("RANDOM", "UBCF", "IBCF")
error







