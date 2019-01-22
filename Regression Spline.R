#####################################################################################################
library(splines)
# Using the train set
sales <- read.csv("C:/Users/woshi/Documents/sales.csv",header = TRUE,na.strings = "NA" )
sales$date=as.Date(sales$date)

salesfull <- sales
test <- subset(salesfull,date>="2013-04-01")
sales <- subset(salesfull,date<"2013-04-01")
vali <- test


# 5-fold Cross-validation to decide degree of freedom
RMSLE.cv <- matrix(0,nrow=5,ncol=20)
set.seed(1)
perm.index <- sample(1:length(sales$units),length(sales$units))
sales.perm <- sales[perm.index,]

for (j in c(1:5)) {
  start_time <- Sys.time()
  
  start <- round((j-1)*length(sales$units)*0.1)+1
  end<- ifelse(j<5,round(j*length(sales$units)*0.1),length(sales$units))
  vali.index<- start:end
  train.sales <- sales.perm[-vali.index,]
  vali.sales <- sales.perm[vali.index,]
  vali.pred <- vali.sales
  
  #Degree from 1 to 20
  for (i in c(1:20)) {
    for (id in levels(train.sales$id)) {
      sales.train <- train.sales[which(train.sales$id==id),]
      if (sum(sales.train[,"units"])!=0) {
        #regression spline
        glm.fit = glm(log1p(units) ~ bs(days, df = i)+
                        dayOfweek+dayOfmonth+holiday+blackFriday
                      +hot+cold+storm, data=sales.train)
        vali.pred$units[which(vali.pred$id==id)] <- 
          ceiling(exp(predict(glm.fit,newdata=vali.sales[which(vali.sales$id==id),]))-1)
        print(id)
      }
    }
    vali.pred$units[which(vali.pred$units<0)]=0
    dif <- sum((log1p(vali.sales$units)-log1p(vali.pred$units))^2)
    RMSLE.cv[j,i] <- sqrt(dif/(dim(vali.pred)[1])) #error
    
  }
  
  print(j) #The jth set
  print(Sys.time() - start_time) #Compute time
}

RMSLE <- colMeans(RMSLE.cv)
i=which(RMSLE==min(RMSLE)) #The best i.
i

#Predict using the best cv degree

for (id in levels(sales$id)) {
  sales.train <- sales[which(sales$id==id),]
  
  if (sum(sales.train[,"units"])!=0) {
    glm.fit = glm(log1p(units) ~ bs(days, df = i)+
                    dayOfweek+dayOfmonth+holiday+blackFriday
                  +hot+cold+storm, data=sales.train)
    test$units[which(test$id==id)] <- 
      ceiling(exp(predict(glm.fit,newdata=test[which(test$id==id),]))-1)
    print(id)
  }
}


#summary of product 9_93
summary(glm.fit)
#The whole test error
dif <- sum((log1p(test$units)-log1p(vali$units))^2)
#Test error
RMSLE_Spline <- sqrt(dif/nrow(test))
RMSLE_Spline


##############################################################################################
# Using the test data set

sales <- read.csv("C:/Users/woshi/Documents/sales.csv",header = TRUE,na.strings = "NA" )
test <- read.csv("C:/Users/woshi/Documents/sales_test.csv",header = TRUE, na.strings = "NA")
sales$date=as.Date(sales$date)
test$date=as.Date(test$date)

test$units = 0

for (id in levels(sales$id)) {
  sales.train <- sales[which(sales$id==id),]
  
  # Try to find the best polynomial degree through cross-validation
  
  if (sum(sales.train[which(sales.train$date>"2013-04-01"),"units"])!=0) {
    glm.fit <- glm(log1p(units) ~ poly(days,7)  # The cv selected i
                   +month+dayOfweek+dayOfmonth+is_holiday+is_blackFriday+hot+cold+storm, 
                   data=sales.train)
    test$units[which(test$id==id)] <- ceiling(exp(predict(glm.fit,test[which(test$id==id),]))-1)
    #print(summary(lm.fit))
    #jpeg(sprintf("myplot_%s.jpg",id))
    #par(mfrow=c(2,2))
    #plot(lm.fit)
    #dev.off()
    print(id)
  }
}

#submit data 
test$units[which(test$units<0)]=0

submit <- test[,c("date","id","units")]
submit$id <- paste(submit$id,submit$date,sep="_")
submit=submit[,2:3]

write.csv(submit,file="submit_PolyR_0313.csv",row.names = FALSE)
