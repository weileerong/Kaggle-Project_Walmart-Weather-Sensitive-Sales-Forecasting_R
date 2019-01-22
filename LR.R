#############################################
#   7. Model 1: linear regression           #
#############################################
# Load data in either way
sales <- read.csv("sales.csv",header = TRUE,na.strings = "NA" )
test <- read.csv("sales_test.csv",header = TRUE, na.strings = "NA")
sales$date=as.Date(sales$date)
test$date=as.Date(test$date)
# sales <- load("sales.RData")
# test <- load("sales_test.RData")

test$units = 0
################without log transformation
# for (id in levels(sales$id)) {
#   sales.train <- sales[which(sales$id==id),]
#   lm.fit <- lm(units ~ days + dayOfweek+dayOfquarters+dayOfmonth+is_holiday+is_blackFriday+hot+cold+storm, data=sales.train)
#   test$units[which(test$id==id)] <- ceiling(predict(lm.fit,test[which(test$id==id),]))
#   #print(summary(lm.fit))
#   print(id)
# }

#log1p

for (id in levels(sales$id)) {
  sales.train <- sales[which(sales$id==id),]
  
  if (sum(sales.train[which(sales.train$date>"2013-04-01"),"units"])!=0) {
    lm.fit <- lm(log1p(units) ~ days+month+dayOfweek+dayOfmonth
                 +is_holiday+is_blackFriday+hot+cold+storm, data=sales.train)
    test$units[which(test$id==id)] <- ceiling(exp(predict(lm.fit,test[which(test$id==id),]))-1)
    #print(summary(lm.fit))
    #jpeg(sprintf("myplot_%s.jpg",id))
    #par(mfrow=c(2,2))
    #plot(lm.fit)
    #dev.off()
    print(id)
    
  }
  
  
}
#summary of product 9_93
summary(lm.fit)
#submit data 
test$units[which(test$units<0)]=0

submit <- test[,c("date","id","units")]
submit$id <- paste(submit$id,submit$date,sep="_")
submit=submit[,2:3]

write.csv(submit,file="submit_LR_0309.csv",row.names = FALSE)
