sales <- read.csv("sales.csv",header = TRUE,na.strings = "NA" )
test <- read.csv("sales_test.csv",header = TRUE, na.strings = "NA")
sales$date=as.Date(sales$date)
test$date=as.Date(test$date)
test$units = 0

library (glmnet)
grid =10^seq (10,-2,length =100)
for (id in levels(sales$id)) {
  sales.train <- sales[which(sales$id==id),]
  if (sum(sales.train[which(sales.train$date>"2013-04-01"),"units"])!=0) {
  sales.train = sales[which(sales$id==id),
                      c("units","days","month","dayOfweek","dayOfmonth",
                        "is_holiday","is_blackFriday","hot","cold","storm")]
  sales.test = test[which(test$id==id),
                    c("units","days","month","dayOfweek","dayOfmonth",
                      "is_holiday","is_blackFriday","hot","cold","storm")]
  train.mat = model.matrix(log1p(units)~., data=sales.train)[,-1]
  test.mat = model.matrix(log1p(units)~., data=sales.test)[,-1]
  
  ridge.mod = glmnet(train.mat, log1p(sales.train[, "units"]),alpha=0,lambda =grid)
  
  set.seed (1)
  cv.out = cv.glmnet(train.mat, log1p(sales.train[, "units"]), alpha=0)
  bestlam = cv.out$lambda.min
  
  #predict on test set
  test$units[which(test$id==id)] = ceiling(expm1(predict(ridge.mod, newx=test.mat, s=bestlam)))
  print(id)
  }
}
#coefficient of product 9_93
ridge.coef = predict(ridge.mod, s=bestlam, type="coefficients")

test$units[which(test$units<0)]=0

submit <- test[,c("date","id","units")]
submit$id <- paste(submit$id,submit$date,sep="_")
submit=submit[,2:3]

write.csv(submit,file="submit_Ridge_0309.csv",row.names = FALSE)