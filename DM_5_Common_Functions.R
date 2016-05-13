#Author: Achal SHah
#Date: 02/04/2016
#Dataset: Movielens data set - 100k/10M data
#Project: Movie recommendation system
#Description: This is the file which provides functionalities to support main movie recommendation system.

#Utility function
print.time <- function(text)
{
        print(paste0(text," ",Sys.time()))
}

find.avg.rating.by.muv <- function(muv.id.1)
{
        mean(filter(reviews,item.id == muv.id.1)$rating,na.rm = T)
        
}

find.corr.user <- function(trainData,type,userCount = nrow(users),itemCount = nrow(items))
{
        print.time(paste(type,userCount,itemCount))
        
        trainData.matrix = matrix(NA,nrow = userCount,ncol = itemCount)
        trainData.matrix[cbind(trainData$user.id,trainData$item.id)] <- trainData$rating
        
        traindata.corr = as.matrix(dist(trainData.matrix, method = type))
        traindata.corr.w = traindata.corr / apply(traindata.corr, 1, max,na.rm=T)
        traindata.corr.w.matrix  = 1 - traindata.corr.w
        traindata.corr.df = melt(traindata.corr.w.matrix)
        colnames(traindata.corr.df) = c("user1","user2","corr.value")
        traindata.corr.df = traindata.corr.df %>% filter(user1 != user2,corr.value != 1) %>% arrange(user1,desc(corr.value)) #Remove outlines like remove if users are very much alike as it must have happened if common movies are very less
        traindata.corr.df
        
} 


predict.over.testdata <- function(testdata,traindata.euc.corr,traindata.man.corr,traindata.lmax.corr,traindata)
{
        testdata.pred <- testdata
        
        #Eucidean
        print.time("Euc 15")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(euc.pred.rating15 = predict.rating(user.id,item.id,15,traindata.euc.corr,traindata))
        
        print.time("Euc 25")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(euc.pred.rating25 = predict.rating(user.id,item.id,25,traindata.euc.corr,traindata))
        print.time("Euc 35")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(euc.pred.rating35 = predict.rating(user.id,item.id,35,traindata.euc.corr,traindata))
        
        #Manhattan
        print.time("Man 15")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(man.pred.rating15 = predict.rating(user.id,item.id,15,traindata.man.corr,traindata))
        print.time("Man 25")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(man.pred.rating25 = predict.rating(user.id,item.id,25,traindata.man.corr,traindata))
        print.time("Man 35")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(man.pred.rating35 = predict.rating(user.id,item.id,35,traindata.man.corr,traindata))
        
        #Lmax
        print.time("Lmax 15")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(lmax.pred.rating15 = predict.rating(user.id,item.id,15,traindata.lmax.corr,traindata))
        print.time("Lmax 25")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(lmax.pred.rating25 = predict.rating(user.id,item.id,25,traindata.lmax.corr,traindata))
        print.time("Lmax 35")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(lmax.pred.rating35 = predict.rating(user.id,item.id,35,traindata.lmax.corr,traindata))
        
        
        testdata.pred
}

predict.rating <- function(user.id.1,item.id.1,k,trainDataCorr,traindata){
        
        similar.users <- trainDataCorr %>% filter(user1 == user.id.1)
        movies.rated.by <- traindata %>% filter(item.id == item.id.1)
        common.users <- intersect(similar.users$user2,movies.rated.by$user.id)
        rating.by.corr <- sum((similar.users %>% filter(user2 %in% common.users) %>% select(corr.value))[1:k,] * (movies.rated.by %>% filter(user.id %in% common.users) %>% select(rating))[1:k,],na.rm = T)
        (rating.by.corr) / (sum((similar.users %>% filter(user2 %in% common.users) %>% select(corr.value))[1:k,],na.rm = T))
        
        #rating.by.corr <- sum((movies.rated.by %>% filter(user.id %in% common.users) %>% select(rating))[1:k,],na.rm = T)/k
}

#This function is used to predict testsets with corr. factor considering age
predict.over.testdata.with.age <- function(testdata,traindata.euc.corr,traindata.man.corr,traindata.lmax.corr,traindata)
{
        testdata.pred <- testdata
        
        #Eucidean
        print.time("Euc 35")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(euc.pred.age.rating35 = predict.rating(user.id,item.id,35,traindata.euc.corr,traindata))
        
        #Manhattan
        print.time("Man 35")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(man.pred.age.rating35 = predict.rating(user.id,item.id,35,traindata.man.corr,traindata))
        
        #Lmax
        print.time("Lmax 35")
        testdata.pred <- testdata.pred %>% group_by(user.id,item.id,rating) %>% mutate(lmax.pred.age.rating35 = predict.rating(user.id,item.id,35,traindata.lmax.corr,traindata))
        
        testdata.pred
        
}


#Functions to analyze 10m data
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

