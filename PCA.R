sales <- read.csv("sales.csv",header = TRUE,na.strings = "NA" )
test <- read.csv("sales_test.csv",header = TRUE, na.strings = "NA")
sales$date=as.Date(sales$date)
test$date=as.Date(test$date)
test$units = 0

library (pls)
#select best ncomp
for (id in levels(sales$id)) {
  sales.train <- sales[which(sales$id==id),]
  if (sum(sales.train[which(sales.train$date>"2013-04-01"),"units"])!=0) {
  sales.train = sales[which(sales$id==id),
                      c("units","days","month","dayOfweek","dayOfmonth",
                        "is_holiday","is_blackFriday","hot","cold","storm")]
  sales.test = test[which(test$id==id),
                    c("units","days","month","dayOfweek","dayOfmonth",
                      "is_holiday","is_blackFriday","hot","cold","storm")]
  set.seed (2)
  pcr.fit=pcr(log1p(units)~., data=sales.train, scale =FALSE,validation ="CV")
  set.seed (3)
  bestncomp = selectNcomp(pcr.fit, "onesigma", plot = FALSE) 
  if (bestncomp == 0 ) {bestncomp = 3}
  test$units[which(test$id==id)]=
    ceiling(expm1(predict(pcr.fit, sales.test, ncomp = bestncomp )))
  print(id)
  }
}
test$units[which(test$units<0)]=0

submit <- test[,c("date","id","units")]
submit$id <- paste(submit$id,submit$date,sep="_")
submit=submit[,2:3]

write.csv(submit,file="submit_PCA_0309.csv",row.names = FALSE)
