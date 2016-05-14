#Author: Achal SHah
#Date: 02/04/2016
#Dataset: Movielens data set - 100k/10M data
#Project: Movie recommendation system
#Description: 

# Key features of this program
# Everytime correlation and prediction is found, I am writing in csv file for the reusablity
# You can find all the .csv file in the code/que5/RR/ folter

#To install package if not present
#install.packages("dplyr")
#install.packages("reshape2")

library(dplyr)
library(reshape2)

#Changing current directory to read data files
#setwd("C:\\Users\\Mr.Lazy\\Google Drive\\IUB Data Science\\Data Mining\\Homeworks\\Submission\\Code\\Que5")

#To support functionalities
source("CMN_Functions.R")

#If this variable is false then entire code will be run
#If debug mode is true then R program will use precalculated data
IsDebugMode <- F

#Pleases make it true if you are running on capable server
Is10MSupported <- F



reviews <- read.table('ml-100k/u.data', header=FALSE)
colnames(reviews)<-c('user.id','item.id','rating','timestamp')

items <- read.csv('ml-100k/u.item', header=FALSE, sep = "|")
colnames(items)<-c('muv.id', 'muv.title', 'release.date', 'video.release.date', 'IMDb.URL', 'Unknown',  'Action', 'Adventure', 'Animation', 'Childrens', 'Comedy', 'Crime', 'Documentary', 'Drama', 'Fantasy', 'Film-Noir', 'Horror', 'Musical', 'Mystery', 'Romance', 'Sci-Fi', 'Thriller', 'War', 'Western')

users <- read.csv('ml-100k/u.user', header=FALSE, sep='|')
colnames(users)<-c('user.id', 'age', 'gender', 'occupation', 'zip.code')

traindata1  <- read.table("ml-100k/u1.base",col.names = c('user.id','item.id','rating','timestamp'))
traindata2 <- read.table("ml-100k/u2.base",col.names =c('user.id','item.id','rating','timestamp'))
traindata3 <- read.table("ml-100k/u3.base",col.names = c('user.id','item.id','rating','timestamp'))
traindata4 <- read.table("ml-100k/u4.base",col.names =c('user.id','item.id','rating','timestamp'))
traindata5 <- read.table("ml-100k/u5.base",col.names = c('user.id','item.id','rating','timestamp'))

testdata1 <- read.table("ml-100k/u1.test",col.names = c('user.id','item.id','rating','timestamp'))
testdata2 <- read.table("ml-100k/u2.test",col.names = c('user.id','item.id','rating','timestamp'))
testdata3 <- read.table("ml-100k/u3.test",col.names = c('user.id','item.id','rating','timestamp'))
testdata4 <- read.table("ml-100k/u4.test",col.names =c('user.id','item.id','rating','timestamp'))
testdata5 <- read.table("ml-100k/u5.test",col.names =c('user.id','item.id','rating','timestamp'))

if(!IsDebugMode)
{
        traindata1.euc.corr <- find.corr.user(traindata1,"euclidean")
        traindata2.euc.corr <- find.corr.user(traindata2,"euclidean")
        traindata3.euc.corr <- find.corr.user(traindata3,"euclidean")
        traindata4.euc.corr <- find.corr.user(traindata4,"euclidean")
        traindata5.euc.corr <- find.corr.user(traindata5,"euclidean")
        
        write.csv(traindata1.euc.corr,"RR/traindata1.euc.corr.csv")
        write.csv(traindata2.euc.corr,"RR/traindata2.euc.corr.csv")
        write.csv(traindata3.euc.corr,"RR/traindata3.euc.corr.csv")
        write.csv(traindata4.euc.corr,"RR/traindata4.euc.corr.csv")
        write.csv(traindata5.euc.corr,"RR/traindata5.euc.corr.csv")
        
        traindata1.man.corr <- find.corr.user(traindata1,"manhattan")
        traindata2.man.corr <- find.corr.user(traindata2,"manhattan")
        traindata3.man.corr <- find.corr.user(traindata3,"manhattan")
        traindata4.man.corr <- find.corr.user(traindata4,"manhattan")
        traindata5.man.corr <- find.corr.user(traindata5,"manhattan")
        
        write.csv(traindata1.man.corr,"RR/traindata1.man.corr.csv")
        write.csv(traindata2.man.corr,"RR/traindata2.man.corr.csv")
        write.csv(traindata3.man.corr,"RR/traindata3.man.corr.csv")
        write.csv(traindata4.man.corr,"RR/traindata4.man.corr.csv")
        write.csv(traindata5.man.corr,"RR/traindata5.man.corr.csv")
        
        traindata1.lmax.corr <- find.corr.user(traindata1,"maximum")
        traindata2.lmax.corr <- find.corr.user(traindata2,"maximum")
        traindata3.lmax.corr <- find.corr.user(traindata3,"maximum")
        traindata4.lmax.corr <- find.corr.user(traindata4,"maximum")
        traindata5.lmax.corr <- find.corr.user(traindata5,"maximum")
        
        write.csv(traindata1.lmax.corr,"RR/traindata1.lmax.corr.csv")
        write.csv(traindata2.lmax.corr,"RR/traindata2.lmax.corr.csv")
        write.csv(traindata3.lmax.corr,"RR/traindata3.lmax.corr.csv")
        write.csv(traindata4.lmax.corr,"RR/traindata4.lmax.corr.csv")
        write.csv(traindata5.lmax.corr,"RR/traindata5.lmax.corr.csv")
} else {
        traindata1.euc.corr <- read.csv("RR/traindata1.euc.corr.csv")
        traindata2.euc.corr <- read.csv("RR/traindata2.euc.corr.csv")
        traindata3.euc.corr <- read.csv("RR/traindata3.euc.corr.csv")
        traindata4.euc.corr <- read.csv("RR/traindata4.euc.corr.csv")
        traindata5.euc.corr <- read.csv("RR/traindata5.euc.corr.csv")
        
        traindata1.man.corr <- read.csv("RR/traindata1.man.corr.csv")
        traindata2.man.corr <- read.csv("RR/traindata2.man.corr.csv")
        traindata3.man.corr <- read.csv("RR/traindata3.man.corr.csv")
        traindata4.man.corr <- read.csv("RR/traindata4.man.corr.csv")
        traindata5.man.corr <- read.csv("RR/traindata5.man.corr.csv")
        
        traindata1.lmax.corr <- read.csv("RR/traindata1.lmax.corr.csv")
        traindata2.lmax.corr <- read.csv("RR/traindata2.lmax.corr.csv")
        traindata3.lmax.corr <- read.csv("RR/traindata3.lmax.corr.csv")
        traindata4.lmax.corr <- read.csv("RR/traindata4.lmax.corr.csv")
        traindata5.lmax.corr <- read.csv("RR/traindata5.lmax.corr.csv")
}

if(!IsDebugMode)
{
        testdata1.pred <- predict.over.testdata(testdata1[,1:3],traindata1.euc.corr,traindata1.man.corr,traindata1.lmax.corr,traindata1)
        write.csv(testdata1.pred,"RR/testdata1.pred.csv")
        
        testdata2.pred <- predict.over.testdata(testdata2[,1:3],traindata2.euc.corr,traindata2.man.corr,traindata2.lmax.corr,traindata2)
        write.csv(testdata2.pred,"RR/testdata2.pred.csv")
        
        testdata3.pred <- predict.over.testdata(testdata3[,1:3],traindata3.euc.corr,traindata3.man.corr,traindata3.lmax.corr,traindata3)
        write.csv(testdata3.pred,"RR/testdata3.pred.csv")
        
        testdata4.pred <- predict.over.testdata(testdata4[,1:3],traindata4.euc.corr,traindata4.man.corr,traindata4.lmax.corr,traindata4)
        write.csv(testdata4.pred,"RR/testdata4.pred.csv")
        
        testdata5.pred <- predict.over.testdata(testdata5[,1:3],traindata5.euc.corr,traindata5.man.corr,traindata5.lmax.corr,traindata5)
        write.csv(testdata5.pred,"RR/testdata5.pred.csv")
        
        
        final.test.data.pred <- rbind(testdata1.pred,testdata2.pred,testdata3.pred,testdata4.pred,testdata5.pred)
        
        Sys.time()
        final.test.data.pred <- final.test.data.pred %>% group_by(user.id,item.id) %>% mutate(avg.rating = find.avg.rating.by.muv(item.id))
        
        write.csv(final.test.data.pred,"RR/part_A_final.test.data.pred.csv")
} else
{
        testdata1.pred <- read.csv("RR/testdata1.pred.csv")
        testdata2.pred <- read.csv("RR/testdata2.pred.csv")
        testdata3.pred <- read.csv("RR/testdata3.pred.csv")
        testdata4.pred <- read.csv("RR/testdata4.pred.csv")
        testdata5.pred <- read.csv("RR/testdata5.pred.csv")
        final.test.data.pred <- read.csv("RR/part_A_final.test.data.pred.csv")
}

#Performance calculation
MADs <- data.frame(avg.model = NA,euc15.model = NA,euc25.model = NA, euc35.model = NA, man15.model = NA, man25.model = NA, man35.model = NA, lmax15.model = NA, lmax25.model = NA, lmax35.model = NA)
MADs$avg.model <- sum(abs(final.test.data.pred$rating - final.test.data.pred$avg.rating),na.rm = T)/nrow(final.test.data.pred)

MADs$euc15.model <- sum(abs(final.test.data.pred$rating - round(final.test.data.pred$euc.pred.rating15)),na.rm = T)/nrow(final.test.data.pred)
MADs$euc25.model <- sum(abs(final.test.data.pred$rating - round(final.test.data.pred$euc.pred.rating25)),na.rm = T)/nrow(final.test.data.pred)
MADs$euc35.model <- sum(abs(final.test.data.pred$rating - round(final.test.data.pred$euc.pred.rating35)),na.rm = T)/nrow(final.test.data.pred)

MADs$man15.model <- sum(abs(final.test.data.pred$rating - round(final.test.data.pred$man.pred.rating15)), na.rm =T)/nrow(final.test.data.pred)
MADs$man25.model <- sum(abs(final.test.data.pred$rating - round(final.test.data.pred$man.pred.rating25)), na.rm = T)/nrow(final.test.data.pred)
MADs$man35.model <- sum(abs(final.test.data.pred$rating - round(final.test.data.pred$man.pred.rating35)), na.rm = T)/nrow(final.test.data.pred)

MADs$lmax15.model <- sum(abs(final.test.data.pred$rating - round(final.test.data.pred$lmax.pred.rating15)),na.rm = T)/nrow(final.test.data.pred)
MADs$lmax25.model <- sum(abs(final.test.data.pred$rating - round(final.test.data.pred$lmax.pred.rating25)),na.rm = T)/nrow(final.test.data.pred)
MADs$lmax35.model <- sum(abs(final.test.data.pred$rating - round(final.test.data.pred$lmax.pred.rating35)),na.rm = T)/nrow(final.test.data.pred)

MADs

#Part A conclusion:
#Best k value - 35
#SO from now on we will use k as 35 for all the algorithms


#Part b
#Take user's age into consideration along with movie ratings and do 5 fold validation on 100k data
#Consider user's age into corr. value
calc.user.age.corr <- function(trainData,old.corr.df,userData,itemData,type)
{
        print.time("calc.user.age.corr");
        age.matrix = matrix(NA,nrow = nrow(userData),ncol = 1)
        age.matrix[cbind(userData$user.id,1)] <- userData$age
        
        age.corr = as.matrix(dist(age.matrix, method = "euclidean"))
        age.corr.w = age.corr / max(age.corr,na.rm = T)
        age.corr.w.matrix  = 1 - age.corr.w
        print(dim(age.corr.w.matrix))
        
        trainData.corr.w.matrix = matrix(NA,nrow = nrow(userData),ncol = nrow(userData))
        trainData.corr.w.matrix[cbind(old.corr.df$user1,old.corr.df$user2)] <- old.corr.df$corr.value
        print(dim(trainData.corr.w.matrix))
        final.corr.matrix <- age.corr.w.matrix + trainData.corr.w.matrix
        
        final.corr.df = melt(final.corr.matrix)
        colnames(final.corr.df) = c("user1","user2","corr.value")
        final.corr.df = final.corr.df %>% filter(user1 != user2) %>% arrange(user1,desc(corr.value))
        final.corr.df
}

if(!IsDebugMode)
{
        traindata1.euc.corr.with.age  <- calc.user.age.corr(traindata1,traindata1.euc.corr,users,items,"euclidean")
        traindata2.euc.corr.with.age  <- calc.user.age.corr(traindata2,traindata2.euc.corr,users,items,"euclidean")
        traindata3.euc.corr.with.age  <- calc.user.age.corr(traindata3,traindata3.euc.corr,users,items,"euclidean")
        traindata4.euc.corr.with.age  <- calc.user.age.corr(traindata4,traindata4.euc.corr,users,items,"euclidean")
        traindata5.euc.corr.with.age  <- calc.user.age.corr(traindata5,traindata5.euc.corr,users,items,"euclidean")
        
        write.csv(traindata1.euc.corr.with.age,"RR/traindata1.euc.corr.with.age.csv")
        write.csv(traindata2.euc.corr.with.age,"RR/traindata2.euc.corr.with.age.csv")
        write.csv(traindata3.euc.corr.with.age,"RR/traindata3.euc.corr.with.age.csv")
        write.csv(traindata4.euc.corr.with.age,"RR/traindata4.euc.corr.with.age.csv")
        write.csv(traindata5.euc.corr.with.age,"RR/traindata5.euc.corr.with.age.csv")
        
        
        traindata1.man.corr.with.age  <- calc.user.age.corr(traindata1,traindata1.man.corr,users,items,"manhattan")
        traindata2.man.corr.with.age  <- calc.user.age.corr(traindata2,traindata2.man.corr,users,items,"manhattan")
        traindata3.man.corr.with.age  <- calc.user.age.corr(traindata3,traindata3.man.corr,users,items,"manhattan")
        traindata4.man.corr.with.age  <- calc.user.age.corr(traindata4,traindata4.man.corr,users,items,"manhattan")
        traindata5.man.corr.with.age  <- calc.user.age.corr(traindata5,traindata5.man.corr,users,items,"manhattan")
        
        write.csv(traindata1.man.corr.with.age,"RR/traindata1.man.corr.with.age.csv")
        write.csv(traindata2.man.corr.with.age,"RR/traindata2.man.corr.with.age.csv")
        write.csv(traindata3.man.corr.with.age,"RR/traindata3.man.corr.with.age.csv")
        write.csv(traindata4.man.corr.with.age,"RR/traindata4.man.corr.with.age.csv")
        write.csv(traindata5.man.corr.with.age,"RR/traindata5.man.corr.with.age.csv")
        
        traindata1.lmax.corr.with.age  <- calc.user.age.corr(traindata1,traindata1.lmax.corr,users,items,"maximum")
        traindata2.lmax.corr.with.age  <- calc.user.age.corr(traindata2,traindata2.lmax.corr,users,items,"maximum")
        traindata3.lmax.corr.with.age  <- calc.user.age.corr(traindata3,traindata3.lmax.corr,users,items,"maximum")
        traindata4.lmax.corr.with.age  <- calc.user.age.corr(traindata4,traindata4.lmax.corr,users,items,"maximum")
        traindata5.lmax.corr.with.age  <- calc.user.age.corr(traindata5,traindata5.lmax.corr,users,items,"maximum")
        
        write.csv(traindata1.lmax.corr.with.age,"RR/traindata1.lmax.corr.with.age.csv")
        write.csv(traindata2.lmax.corr.with.age,"RR/traindata2.lmax.corr.with.age.csv")
        write.csv(traindata3.lmax.corr.with.age,"RR/traindata3.lmax.corr.with.age.csv")
        write.csv(traindata4.lmax.corr.with.age,"RR/traindata4.lmax.corr.with.age.csv")
        write.csv(traindata5.lmax.corr.with.age,"RR/traindata5.lmax.corr.with.age.csv")
} else {
        traindata1.euc.corr.with.age <- read.csv("RR/traindata1.euc.corr.with.age.csv")
        traindata2.euc.corr.with.age <- read.csv("RR/traindata2.euc.corr.with.age.csv")
        traindata3.euc.corr.with.age <- read.csv("RR/traindata3.euc.corr.with.age.csv")
        traindata4.euc.corr.with.age <- read.csv("RR/traindata4.euc.corr.with.age.csv")
        traindata5.euc.corr.with.age <- read.csv("RR/traindata5.euc.corr.with.age.csv")
        
        traindata1.man.corr.with.age <- read.csv("RR/traindata1.man.corr.with.age.csv")
        traindata2.man.corr.with.age <- read.csv("RR/traindata2.man.corr.with.age.csv")
        traindata3.man.corr.with.age <- read.csv("RR/traindata3.man.corr.with.age.csv")
        traindata4.man.corr.with.age <- read.csv("RR/traindata4.man.corr.with.age.csv")
        traindata5.man.corr.with.age <- read.csv("RR/traindata5.man.corr.with.age.csv")
        
        traindata1.lmax.corr.with.age <- read.csv("RR/traindata1.lmax.corr.with.age.csv")
        traindata2.lmax.corr.with.age <- read.csv("RR/traindata2.lmax.corr.with.age.csv")
        traindata3.lmax.corr.with.age <- read.csv("RR/traindata3.lmax.corr.with.age.csv")
        traindata4.lmax.corr.with.age <- read.csv("RR/traindata4.lmax.corr.with.age.csv")
        traindata5.lmax.corr.with.age <- read.csv("RR/traindata5.lmax.corr.with.age.csv")
}

if(!IsDebugMode)
{
        testdata1.pred.with.age <- predict.over.testdata.with.age(testdata1[,1:3],traindata1.euc.corr.with.age,traindata1.man.corr.with.age,traindata1.lmax.corr.with.age,traindata1)
        write.csv(testdata1.pred.with.age,"RR/testdata1.pred.with.age.csv")
        
        testdata2.pred.with.age <- predict.over.testdata.with.age(testdata2[,1:3],traindata2.euc.corr.with.age,traindata2.man.corr.with.age,traindata2.lmax.corr.with.age,traindata2)
        write.csv(testdata2.pred.with.age,"RR/testdata2.pred.with.age.csv")
        
        testdata3.pred.with.age <- predict.over.testdata.with.age(testdata3[,1:3],traindata3.euc.corr.with.age,traindata3.man.corr.with.age,traindata3.lmax.corr.with.age,traindata3)
        write.csv(testdata3.pred.with.age,"RR/testdata3.pred.with.age.csv")
        
        testdata4.pred.with.age <- predict.over.testdata.with.age(testdata4[,1:3],traindata4.euc.corr.with.age,traindata4.man.corr.with.age,traindata4.lmax.corr.with.age,traindata4)
        write.csv(testdata4.pred.with.age,"RR/testdata4.pred.with.age.csv")
        
        testdata5.pred.with.age <- predict.over.testdata.with.age(testdata5[,1:3],traindata5.euc.corr.with.age,traindata5.man.corr.with.age,traindata5.lmax.corr.with.age,traindata5)
        write.csv(testdata5.pred.with.age,"RR/testdata5.pred.with.age.csv")
        
        final.test.data.pred.with.age <- rbind(testdata1.pred.with.age,testdata2.pred.with.age,testdata3.pred.with.age,testdata4.pred.with.age,testdata5.pred.with.age)
        
        final.test.data.pred.with.age <- final.test.data.pred.with.age %>% group_by(user.id,item.id) %>% mutate(avg.rating = find.avg.rating.by.muv(item.id))
        
        
        write.csv(final.test.data.pred.with.age,"RR/part_B_final.test.data.pred.with.age.csv")
}else{
        testdata1.pred.with.age <- read.csv("RR/testdata1.pred.with.age.csv")
        testdata2.pred.with.age <- read.csv("RR/testdata2.pred.with.age.csv")
        testdata3.pred.with.age <- read.csv("RR/testdata3.pred.with.age.csv")
        testdata4.pred.with.age <- read.csv("RR/testdata4.pred.with.age.csv")
        testdata5.pred.with.age <- read.csv("RR/testdata5.pred.with.age.csv")
        final.test.data.pred.with.age <- read.csv("RR/part_B_final.test.data.pred.with.age.csv")
        
        
}
#Performance calculation
MADs.with.age <- data.frame(avg.model = NA,euc35.model.with.age = NA, man35.model.with.age = NA, lmax35.model.with.age = NA)
MADs.with.age$avg.model <- sum(abs(final.test.data.pred.with.age$rating - final.test.data.pred.with.age$avg.rating),na.rm = T)/nrow(final.test.data.pred.with.age)

MADs.with.age$euc35.model.with.age <- sum(abs(final.test.data.pred.with.age$rating - round(final.test.data.pred.with.age$euc.pred.age.rating35)),na.rm = T)/nrow(final.test.data.pred.with.age)

MADs.with.age$man35.model.with.age <- sum(abs(final.test.data.pred.with.age$rating - round(final.test.data.pred.with.age$man.pred.age.rating35)), na.rm = T)/nrow(final.test.data.pred.with.age)

MADs.with.age$lmax35.model.with.age <- sum(abs(final.test.data.pred.with.age$rating - round(final.test.data.pred.with.age$lmax.pred.age.rating35)),na.rm = T)/nrow(final.test.data.pred.with.age)

MADs.with.age

#Part C
#Run 10M Data on top 3 algorithms (Euclidean,Manhatten and Lmax with k =35)
traindata1.10m <- read.csv("ml-10M100K/Clean/r1.train",col.names = c('user.id','item.id','rating','timestamp'))
traindata2.10m <- read.csv("ml-10M100K/Clean/r2.train",col.names = c('user.id','item.id','rating','timestamp'))
traindata3.10m <- read.csv("ml-10M100K/Clean/r3.train",col.names = c('user.id','item.id','rating','timestamp'))
traindata4.10m <- read.csv("ml-10M100K/Clean/r4.train",col.names = c('user.id','item.id','rating','timestamp'))
traindata5.10m <- read.csv("ml-10M100K/Clean/r5.train",col.names = c('user.id','item.id','rating','timestamp'))

testdata1.10m <- read.csv("ml-10M100K/Clean/r1.test",col.names = c('user.id','item.id','rating','timestamp'))
testdata2.10m <- read.csv("ml-10M100K/Clean/r2.test",col.names = c('user.id','item.id','rating','timestamp'))
testdata3.10m <- read.csv("ml-10M100K/Clean/r3.test",col.names = c('user.id','item.id','rating','timestamp'))
testdata4.10m <- read.csv("ml-10M100K/Clean/r4.test",col.names = c('user.id','item.id','rating','timestamp'))
testdata5.10m <- read.csv("ml-10M100K/Clean/r5.test",col.names = c('user.id','item.id','rating','timestamp'))

alldata.10m <- rbind(traindata2.10m,testdata2.10m)

#I was suspicious about the relativity of traindata and testdata and thats why checked that how many users are common in each train and test data sets.
# And yes, both are not correlated
# So doing analysis on the subset of data from now on. which is preset in both train data and test data.
#common.users.test.train <- intersect(traindata1.10m$user.id,testdata2.10m$user.id)
if(!Is10MSupported)
{
        traindata1.10m.euc.corr <- find.corr.over.train.data.10m(traindata1.10m[1:15000,],"euclidean")
        traindata1.10m.man.corr <- find.corr.over.train.data.10m(traindata1.10m[1:15000,],"manhattan")  
        traindata1.10m.lmax.corr <- find.corr.over.train.data.10m(traindata1.10m[1:15000,],"maximum")
        testdata1.10m.pred <- predict.over.testdata.10m(testdata2.10m[1:500,1:3],traindata1.10m.euc.corr,traindata1.10m.man.corr,traindata1.10m.lmax.corr,traindata1.10m[1:15000,])
        final.test.data.10.pred <- testdata1.10m.pred
        final.test.data.10.pred <- final.test.data.10.pred %>% group_by(user.id,item.id) %>% mutate(avg.rating = find.avg.rating.by.muv.10m(item.id))
        
}else{
        traindata1.10m.euc.corr <- find.corr.over.train.data.10m(traindata1.10m,"euclidean")
        traindata2.10m.euc.corr <- find.corr.over.train.data.10m(traindata2.10m,"euclidean")
        traindata3.10m.euc.corr <- find.corr.over.train.data.10m(traindata3.10m,"euclidean")
        traindata4.10m.euc.corr <- find.corr.over.train.data.10m(traindata4.10m,"euclidean")
        traindata5.10m.euc.corr <- find.corr.over.train.data.10m(traindata5.10m,"euclidean")
        
        write.csv(traindata1.10m.euc.corr,"RR/traindata1.10m.euc.corr.csv")
        write.csv(traindata2.10m.euc.corr,"RR/traindata2.10m.euc.corr.csv")
        write.csv(traindata3.10m.euc.corr,"RR/traindata3.10m.euc.corr.csv")
        write.csv(traindata4.10m.euc.corr,"RR/traindata4.10m.euc.corr.csv")
        write.csv(traindata5.10m.euc.corr,"RR/traindata5.10m.euc.corr.csv")
        
        traindata1.10m.man.corr <- find.corr.over.train.data.10m(traindata1.10m,"manhattan")
        traindata2.10m.man.corr <- find.corr.over.train.data.10m(traindata2.10m,"manhattan")
        traindata3.10m.man.corr <- find.corr.over.train.data.10m(traindata3.10m,"manhattan")
        traindata4.10m.man.corr <- find.corr.over.train.data.10m(traindata4.10m,"manhattan")
        traindata5.10m.man.corr <- find.corr.over.train.data.10m(traindata5.10m,"manhattan")
        
        write.csv(traindata1.10m.man.corr,"RR/traindata1.10m.man.corr.csv")
        write.csv(traindata2.10m.man.corr,"RR/traindata2.10m.man.corr.csv")
        write.csv(traindata3.10m.man.corr,"RR/traindata3.10m.man.corr.csv")
        write.csv(traindata4.10m.man.corr,"RR/traindata4.10m.man.corr.csv")
        write.csv(traindata5.10m.man.corr,"RR/traindata5.10m.man.corr.csv")
        
        traindata1.10m.lmax.corr <- find.corr.over.train.data.10m(traindata1.10m,"maximum")
        traindata2.10m.lmax.corr <- find.corr.over.train.data.10m(traindata2.10m,"maximum")
        traindata3.10m.lmax.corr <- find.corr.over.train.data.10m(traindata3.10m,"maximum")
        traindata4.10m.lmax.corr <- find.corr.over.train.data.10m(traindata4.10m,"maximum")
        traindata5.10m.lmax.corr <- find.corr.over.train.data.10m(traindata5.10m,"maximum")
        
        write.csv(traindata1.10m.lmax.corr,"RR/traindata1.10m.lmax.corr.csv")
        write.csv(traindata2.10m.lmax.corr,"RR/traindata2.10m.lmax.corr.csv")
        write.csv(traindata3.10m.lmax.corr,"RR/traindata3.10m.lmax.corr.csv")
        write.csv(traindata4.10m.lmax.corr,"RR/traindata4.10m.lmax.corr.csv")
        write.csv(traindata5.10m.lmax.corr,"RR/traindata5.10m.lmax.corr.csv")
        
        
        
        if(!IsDebugMode)
        {
                testdata1.10m.pred <- predict.over.testdata.10m(testdata2.10m[,1:3],traindata1.10m.euc.corr,traindata1.10m.man.corr,traindata1.10m.lmax.corr,traindata1.10m)
                
                testdata2.10m.pred <- predict.over.testdata.10m(testdata2.10m[,1:3],traindata2.10m.euc.corr,traindata2.10m.man.corr,traindata2.10m.lmax.corr,traindata2.10m)
                
                testdata3.10m.pred <- predict.over.testdata.10m(testdata3.10m[,1:3],traindata3.10m.euc.corr,traindata3.10m.man.corr,traindata3.10m.lmax.corr,traindata3.10m)
                
                testdata4.10m.pred <- predict.over.testdata.10m(testdata4.10m[,1:3],traindata4.10m.euc.corr,traindata4.10m.man.corr,traindata4.10m.lmax.corr,traindata4.10m)
                
                testdata5.10m.pred <- predict.over.testdata.10m(testdata5.10m[,1:3],traindata5.10m.euc.corr,traindata5.10m.man.corr,traindata5.10m.lmax.corr,traindata5.10m)
                
                final.test.data.10.pred <- rbind(testdata1.10m.pred,testdata2.10m.pred,testdata3.10m.pred,testdata4.10m.pred,testdata5.10m.pred)
                
                final.test.data.10.pred <- final.test.data.10.pred %>% group_by(user.id,item.id) %>% mutate(avg.rating = find.avg.rating.by.muv.10m(item.id))
                
                write.csv(final.test.data.10.pred,"Part_C_final.test.data.10.pred.csv")
        }else{
                
                final.test.data.10.pred <- read.csv("Part_C_final.test.data.10.pred.csv")
        }
}


#Performance calculation
MADs.for.10m <- data.frame(avg.model = NA,euc35.model.10m = NA, man35.model.10m = NA,lmax.model.10m = NA)
MADs.for.10m$avg.model <- sum(abs(final.test.data.10.pred$rating - final.test.data.10.pred$avg.rating),na.rm = T)/nrow(final.test.data.10.pred)

MADs.for.10m$euc35.model.10m <- sum(abs(final.test.data.10.pred$rating - round(final.test.data.10.pred$euc.pred.10m.rating35)),na.rm = T)/nrow(final.test.data.10.pred)

MADs.for.10m$man35.model.10m <- sum(abs(final.test.data.10.pred$rating - round(final.test.data.10.pred$man.pred.10m.rating35)), na.rm = T)/nrow(final.test.data.10.pred)

MADs.for.10m$lmax.model.10m <- sum(abs(final.test.data.10.pred$rating - round(final.test.data.10.pred$lmax.pred.10m.rating35)),na.rm = T)/nrow(final.test.data.10.pred)

MADs.for.10m
