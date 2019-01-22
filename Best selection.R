library(leaps)

sales <- read.csv("sales.csv",header = TRUE,na.strings = "NA" )
test <- read.csv("sales_test.csv",header = TRUE, na.strings = "NA")
sales$date=as.Date(sales$date)
test$date=as.Date(test$date)
test$units = 0

#our own predict method
predict.regsubsets =function (object, newdata ,id ,...){
  form=as.formula (object$call[[2]])
  mat=model.matrix (form,newdata)
  coefi =coef(object,id=id)
  xvars =names(coefi)
  mat[,xvars ]%*% coefi
}

for (id in levels(sales$id)) {
  sales.train <- sales[which(sales$id==id),]
  if (sum(sales.train[which(sales.train$date>"2013-04-01"),"units"])!=0) {
        sales.train = sales[which(sales$id==id),
                      c("units","days","month","dayOfweek","dayOfmonth",
                        "is_holiday","is_blackFriday","hot","cold","storm")]
       sales.test = test[which(test$id==id),
                    c("units","days","month","dayOfweek","dayOfmonth",
                      "is_holiday","is_blackFriday","hot","cold","storm")]

  
          #ten-fold Cross-validation
           k=10
          set.seed (1)
          folds=sample (1:k,nrow(sales.full),replace =TRUE)
         cv.errors =matrix (NA ,k, 9, dimnames =list(NULL, paste(1:9) ))
          for(j in 1:k){ 
                regfit.full=regsubsets(log1p(units)~.,data=sales.train[folds!=j,],
                          nvmax =9)
    
                 for(i in 1:9) {
                       pred=predict(regfit.full,sales.train[folds ==j,], id=i)
                       cv.errors[j,i]=mean((log1p(sales.train$units[folds ==j])-pred)^2)
                 }
           }
          val.errors=colMeans(cv.errors)
          bestid = which.min(val.errors)
  
         test$units[which(test$id==id)] <-  ceiling(expm1(predict(regfit.full,sales.test, id=bestid)))
         print(id)
  }
}

#coefficient of one product 9_93
coef(regfit.full, id = bestid)

test$units[which(test$units<0)]=0

submit <- test[,c("date","id","units")]
submit$id <- paste(submit$id,submit$date,sep="_")
submit=submit[,2:3]

write.csv(submit,file="submit_BSS_0309.csv",row.names = FALSE)