#############################################
#                8. Model 2: KNN            #
#############################################
# Load data in either way
sales <- read.csv("sales.csv",header = TRUE,na.strings = "NA" )
test <- read.csv("sales_test.csv",header = TRUE, na.strings = "NA")
sales$date=as.Date(sales$date)
test$date=as.Date(test$date)
# sales <- load("sales.RData")
# test <- load("sales_test.RData")

test$units = 0
library(FNN)

#"dayOfweek","dayOfquarters","is_holiday","is_blackFriday"
#transform char features as number
#sales$week <- as.POSIXlt(sales$date)$wday
#test$week <- as.POSIXlt(test$date)$wday
#sales$quarters <- as.numeric(substr(sales$dayOfquarters,2,2))
#test$quarters <- as.numeric(substr(test$dayOfquarters,2,2))
#sales$blackFriday <- as.integer(ifelse(sales$is_blackFriday=="Yes",1,0))
#sales$holiday <- as.integer(ifelse(sales$is_holiday=="Yes",1,0))
#test$blackFriday <- as.integer(ifelse(test$is_blackFriday=="Yes",1,0))
#test$holiday <- as.integer(ifelse(test$is_holiday=="Yes",1,0))

##########################################################################################################
#(1) validation set to select K for log1p KNN: K=14
# train.sales <- subset(sales,date<"2013-04-01")
# vali.sales <- subset(sales,date>="2013-04-01")
# vali.pred <- vali.sales
# RMSLE <- rep(0,150)
# for (i in c(1:150)) {
#   for (id in levels(train.sales$id)) {
#    sales.train <- train.sales[which(train.sales$id==id),]
#    train.X=sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#    train.Y=log1p(sales.train[,"units"])
#    test.X=vali.sales[which(vali.sales$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
# 
#       if (dim(test.X)[1]!=0) {
#         knn.pred=knn.reg(train.X,test.X,train.Y,k=i)
#         pred <- as.list(knn.pred)$pred
#         vali.pred$units[which(vali.pred$id==id)] <- ceiling(expm1(pred))
#       }
#   }
# dif <- sum((log1p(vali.sales$units)-log1p(vali.pred$units))^2)
# RMSLE[i] <- sqrt(dif/(dim(vali.pred)[1]))
# print(i)
# }
# plot(RMSLE,type="l")
# which(RMSLE==min(RMSLE))
##################################################################################################
#(2) validation set for 5 times
# RMSLE <- matrix(0,nrow=5,ncol=50)
# #1st time
# sales1 <- subset(sales,date<"2012-05-01")
# train.sales <- subset(sales1,date<"2012-03-01")
# vali.sales <- subset(sales1,date>="2012-03-01")
# vali.pred <- vali.sales
# 
# for (i in c(1:50)) {
#   for (id in levels(train.sales$id)) {
#    sales.train <- train.sales[which(train.sales$id==id),]
#    train.X=sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#    train.Y=log1p(sales.train[,"units"])
#    test.X=vali.sales[which(vali.sales$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
# 
#       if (dim(test.X)[1]!=0) {
#         knn.pred=knn.reg(train.X,test.X,train.Y,k=i)
#         pred <- as.list(knn.pred)$pred
#         vali.pred$units[which(vali.pred$id==id)] <- ceiling(expm1(pred))
#       }
#   }
# dif <- sum((log1p(vali.sales$units)-log1p(vali.pred$units))^2)
# RMSLE[1,i] <- sqrt(dif/(dim(vali.pred)[1]))
# }
# print(1)
# 
# #2ed time
# sales2 <- subset(sales,date<"2012-09-01")
# train.sales <- subset(sales2,date<"2012-05-01")
# vali.sales <- subset(sales2,date>="2012-05-01")
# vali.pred <- vali.sales
# 
# for (i in c(1:50)) {
#   for (id in levels(train.sales$id)) {
#     sales.train <- train.sales[which(train.sales$id==id),]
#     train.X=sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#     train.Y=log1p(sales.train[,"units"])
#     test.X=vali.sales[which(vali.sales$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#     
#     if (dim(test.X)[1]!=0 & dim(train.X)[1]!=0) {
#       knn.pred=knn.reg(train.X,test.X,train.Y,k=i)
#       pred <- as.list(knn.pred)$pred
#       vali.pred$units[which(vali.pred$id==id)] <- ceiling(expm1(pred))
#     }
#   }
#   dif <- sum((log1p(vali.sales$units)-log1p(vali.pred$units))^2)
#   RMSLE[2,i] <- sqrt(dif/(dim(vali.pred)[1]))
# }
# print(2)
# 
# #3rd time
# sales3 <- subset(sales,date<"2013-01-01")
# train.sales <- subset(sales3,date<"2012-09-01")
# vali.sales <- subset(sales3,date>="2012-09-01")
# 
# vali.pred <- vali.sales
# 
# for (i in c(1:50)) {
#   for (id in levels(train.sales$id)) {
#     sales.train <- train.sales[which(train.sales$id==id),]
#     train.X=sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#     train.Y=log1p(sales.train[,"units"])
#     test.X=vali.sales[which(vali.sales$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#     
#     if (dim(test.X)[1]!=0) {
#       knn.pred=knn.reg(train.X,test.X,train.Y,k=i)
#       pred <- as.list(knn.pred)$pred
#       vali.pred$units[which(vali.pred$id==id)] <- ceiling(expm1(pred))
#     }
#   }
#   dif <- sum((log1p(vali.sales$units)-log1p(vali.pred$units))^2)
#   RMSLE[3,i] <- sqrt(dif/(dim(vali.pred)[1]))
# }
# print(3)
# 
# #4th time
# sales4 <- subset(sales,date<"2013-04-01")
# train.sales <- subset(sales4,date<"2013-01-01")
# vali.sales <- subset(sales4,date>="2013-01-01")
# 
# vali.pred <- vali.sales
# 
# for (i in c(1:50)) {
#   for (id in levels(train.sales$id)) {
#     sales.train <- train.sales[which(train.sales$id==id),]
#     train.X=sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#     train.Y=log1p(sales.train[,"units"])
#     test.X=vali.sales[which(vali.sales$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#     
#     if (dim(test.X)[1]!=0) {
#       knn.pred=knn.reg(train.X,test.X,train.Y,k=i)
#       pred <- as.list(knn.pred)$pred
#       vali.pred$units[which(vali.pred$id==id)] <- ceiling(expm1(pred))
#     }
#   }
#   dif <- sum((log1p(vali.sales$units)-log1p(vali.pred$units))^2)
#   RMSLE[4,i] <- sqrt(dif/(dim(vali.pred)[1]))
# }
# print(4)
# 
# #5th time
# train.sales <- subset(sales,date<"2013-04-01")
# vali.sales <- subset(sales,date>="2013-04-01")
# 
# vali.pred <- vali.sales
# 
# for (i in c(1:50)) {
#   for (id in levels(train.sales$id)) {
#     sales.train <- train.sales[which(train.sales$id==id),]
#     train.X=sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#     train.Y=log1p(sales.train[,"units"])
#     test.X=vali.sales[which(vali.sales$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#     
#     if (dim(test.X)[1]!=0) {
#       knn.pred=knn.reg(train.X,test.X,train.Y,k=i)
#       pred <- as.list(knn.pred)$pred
#       vali.pred$units[which(vali.pred$id==id)] <- ceiling(expm1(pred))
#     }
#   }
#   dif <- sum((log1p(vali.sales$units)-log1p(vali.pred$units))^2)
#   RMSLE[5,i] <- sqrt(dif/(dim(vali.pred)[1]))
# }
# print(5)
# 
# RMSLE2 <- colMeans(RMSLE)
# which(RMSLE2==min(RMSLE2)) #The best K.
# plot(RMSLE2,type="l")


#Cross-vallidation new to select K for log1p KNN: K=6
# RMSLE.cv <- matrix(0,nrow=5,ncol=150)
# set.seed(1)
# perm.index <- sample(1:length(sales$units),length(sales$units))
# sales.perm <- sales[perm.index,]
# 
# for (j in c(1:5)) {
#   start_time <- Sys.time()
# 
#   start <- round((j-1)*length(sales$units)*0.1)+1
#   end<- ifelse(j<5,round(j*length(sales$units)*0.1),length(sales$units))
#   vali<- start:end
#   train.sales <- sales.perm[-vali,]
#   vali.sales <- sales.perm[vali,]
#   vali.pred <- vali.sales
# 
#   for (i in c(1:150)) {
#     for (id in levels(train.sales$id)) {
#       sales.train <- train.sales[which(train.sales$id==id),]
#       train.X=sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters",
#                              "blackFriday","holiday")]
#       train.Y=log1p(sales.train[,"units"])
#       test.X=vali.sales[which(vali.sales$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters",
#                                                    "blackFriday","holiday")]
# 
#       if (dim(test.X)[1]!=0) {
#         knn.pred=knn.reg(train.X,test.X,train.Y,k=i)
#         pred <- as.list(knn.pred)$pred
#         vali.pred$units[which(vali.pred$id==id)] <- ceiling(expm1(pred))
#       }
#     }
# 
#     dif <- sum((log1p(vali.sales$units)-log1p(vali.pred$units))^2)
#     RMSLE.cv[j,i] <- sqrt(dif/(dim(vali.pred)[1])) #error
# 
#   }
#   print(j) #The jth set
#   print(Sys.time() - start_time) #Compute time
# }
# 
# RMSLE <- colMeans(RMSLE.cv)
# which(RMSLE==min(RMSLE)) #The best K.
# plot(RMSLE,type="l")

############################################################################################################
#KNN K=6 - RMSLE 0.11012 
# for (id in levels(sales$id)) {
#   sales.train <- sales[which(sales$id==id),]
#   train.X=sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#   train.Y=sales.train[,"units"]
#   test.X=test[which(test$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#   
#   if (dim(test.X)[1]!=0) {
#     knn.pred=knn.reg(train.X,test.X,train.Y,k=6)
#     pred <- as.list(knn.pred)$pred
#     test$units[which(test$id==id)] <- ceiling(pred)
#   }
#   print(id)
# }

#KNN log1p K=6 0.10905;K=14 0.10703;K=20  0.10850
for (id in levels(sales$id)) {
  sales.train <- sales[which(sales$id==id),]
  
  if (sum(sales.train[which(sales.train$date>"2013-04-01"),"units"])!=0) {
    
      train.X=sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
      train.Y=log1p(sales.train[,"units"])
      test.X=test[which(test$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
  
       if (dim(test.X)[1]!=0) {
             knn.pred=knn.reg(train.X,test.X,train.Y,k=14)
             pred <- as.list(knn.pred)$pred
             test$units[which(test$id==id)] <- ceiling(expm1(pred))
        }
  print(id)
  }
}

############################################################################################################
#Normalized the numeric variables

# for (id in levels(sales$id)) {
#   sales.train <- sales[which(sales$id==id),]
#   train.X= sales.train[,c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#   train.X[,c("days","dayOfmonth","week")]=scale(train.X[,c("days","dayOfmonth","week")])
#   train.Y=log1p(sales.train[,"units"])
#   test.X=test[which(test$id==id),c("days","dayOfmonth","cold","hot","storm","week","quarters","blackFriday","holiday")]
#   
#   if (dim(test.X)[1]!=0) {
#     test.X[,c("days","dayOfmonth","week")]=scale(test.X[,c("days","dayOfmonth","week")])
#     knn.pred=knn.reg(train.X,test.X,train.Y,k=14)
#     pred <- as.list(knn.pred)$pred
#     test$units[which(test$id==id)] <- ceiling(expm1(pred))
#   }
#   print(id)
# }
#submit data 
test$units[which(test$units<0)]=0

submit <- test[,c("date","id","units")]
submit$id <- paste(submit$id,submit$date,sep="_")
submit=submit[,2:3]

write.csv(submit,file="submit_KNN_K14_0309.csv",row.names = FALSE)