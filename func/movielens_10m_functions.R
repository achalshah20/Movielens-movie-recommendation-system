#Author: Achal SHah
#Date: 02/04/2016
#Dataset: Movielens data set - 100k/10M data
#Project: Movie recommendation system
#Description: This is the file which provides functionalities to support main movie recommendation system for 10m dataset.


find.avg.rating.by.muv.10m <- function(muv.id.1)
{
  mean(filter(alldata.10m,item.id == muv.id.1)$rating)
  
}

predict.over.testdata.10m <- function(testdata,traindata.euc.corr,traindata.man.corr,traindata.lmax.corr,traindata)
{
  testdata.pred <- testdata
  
  #Eucidean
  print.time("Euc 35 - 10m")
  testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(euc.pred.10m.rating35 = predict.rating(user.id,item.id,35,traindata.euc.corr,traindata))
  
  #Manhattan
  print.time("Man 35 - 10m")
  testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(man.pred.10m.rating35 = predict.rating(user.id,item.id,35,traindata.man.corr,traindata))
  
  #Lmax
  print.time("Lmax 35- 10m")
  testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(lmax.pred.10m.rating35 = predict.rating(user.id,item.id,35,traindata.lmax.corr,traindata))
  
  testdata.pred
  
}

find.corr.user.10m <- function(x,y,data,type)
{
  
  reviews1 <- filter(data,user.id == x)
  reviews2 <- filter(data,user.id == y)
  common.movies <- intersect(reviews1$item.id, reviews2$item.id)
  if(length(common.movies) > 4)
  {
    user1.reviews <- filter(data,user.id == x,item.id %in% common.movies)
    user2.reviews <- filter(data,user.id == y,item.id %in% common.movies)
    
    user.corr <- dist(rbind(user1.reviews$rating,user2.reviews$rating),method = type)
    return(user.corr)
  }
  else
  {
    return(999)
  }
}


find.corr.over.train.data.10m <- function(trainData10m,type)
{        
  
  print.time(paste("Find corr.",type,"for 10m"))
  train.corr.10m <- NULL
  #10m data is not supported by R. 71567 X 71567 correlation will take 20 GB in R and 40 GB by matlab. so grouping data and finding correlation
  
  user.pairs <- expand.grid(user1 = unique(trainData10m$user.id), user2 = unique(trainData10m$user.id))
  dim(user.pairs)
  
  user.pairs.corr <- user.pairs %>% group_by(user1,user2) %>% mutate(corr.value = find.corr.user.10m(user1,user2,trainData10m,type))
  
  train.corr.10m <- rbind(train.corr.10m,user.pairs.corr)
  
  train.corr.10m
  
}

