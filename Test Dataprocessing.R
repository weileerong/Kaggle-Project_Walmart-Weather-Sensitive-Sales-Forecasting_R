#############################################
#               Loding data                 #
#############################################
test <- read.csv("test.csv",header = TRUE)

#############################################
#             checking data                 #
#############################################
#there is no overlap between train and test data set
#prepost.match.data <- merge(total,test, by = c("date","store_nbr","item_nbr"))   

#############################################
#          adding feature                   #
#############################################
test$days <- as.numeric(as.Date(test[,1],"%Y-%m-%d")-as.Date("2012-1-1","%Y-%m-%d")+1)
test$id <- as.factor(paste(test$store_nbr,test$item_nbr,sep="_"))
# 
# test$date=as.Date(sales$date)
# test$day_week=weekdays(sales$date)
# #test$day_month=months(sales$date)
# test$day_quarters=quarters(sales$date)