#############################################
#             1. Emerging data              #
#############################################
#Load original data
train <- read.csv("train.csv",header = TRUE)
weather <- read.csv("weather.csv",header = TRUE,na.strings = c("-"," ","M"))
key <- read.csv("key.csv",header = TRUE)

#Merge three date sets
total1 <- merge(weather,key,by="station_nbr")
total <- merge(train,total1,by=c("store_nbr","date"),sort = FALSE)

#############################################
#       2. Add two main features            #
#############################################
#Add two variables in total data. 
#id represent unique number for each store/item. 
#days represent how many days from 2012-1-1 to the date of the observation
total$days <- as.numeric(as.Date(total[,2],"%Y-%m-%d")-as.Date("2012-1-1","%Y-%m-%d")+1)
total$id <- as.factor(paste(total$store_nbr,total$item_nbr,sep="_"))

#transform factor into numeric which seem to be numeric
total2 <- total #backup of total

#Save total table
#write.csv(total,file = "total.csv",row.names = FALSE)
#total <- read.csv("total.csv",header = TRUE,na.strings = "NA")



#############################################
#   3. Split zero and nonzero sales         #
#############################################

#There are many store/items have no sales
#split as nonsale and sale
agg <- aggregate(units~id,total,sum)
nonsale <- agg[agg$units==0,]  #subsetting of agg
sale <- agg[agg$units!=0,]     #subsetting of agg

sales <- total[(total$id %in% sale$id),]  #subsetting of total which has store/item has sales

#write.csv(sales,file = "sales.csv",row.names = FALSE)


#############################################
#             4.Feature enginering          #
###              (1) date                ####
#############################################
#(1)used in linear regression
#Create feature of dayOfQuaters, dayOfMonth, dayOfWeek
sales$date=as.Date(sales$date)
sales$dayOfquarters=quarters(sales$date) #factor:Q1,Q2,Q3,Q4
sales$dayOfmonth=as.POSIXlt(sales$date)$mday       #1:31
sales$dayOfweek=weekdays(sales$date)  #Monday,Tuesday,...

sales$dayOfquarters=as.factor(sales$dayOfquarters)
sales$dayOfweek=as.factor(sales$dayOfweek)

#Create is_holiday and is_blackFriday
sales$is_holiday="No"
sales[which(sales$date=="2012-01-01"),"is_holiday"] = "Yes"   #New Year's Day
sales[which(sales$date=="2012-01-02"),"is_holiday"] = "Yes"   
sales[which(sales$date=="2012-01-16"),"is_holiday"] = "Yes"   #Martin Luther King Day
sales[which(sales$date=="2012-02-14"),"is_holiday"] = "Yes"   #Valentine's Day
sales[which(sales$date=="2012-02-20"),"is_holiday"] = "Yes"   #Presidents' Day
sales[which(sales$date=="2012-04-08"),"is_holiday"] = "Yes"   #Easter Sunday
sales[which(sales$date=="2012-05-13"),"is_holiday"] = "Yes"   #Mothers' Day 
sales[which(sales$date=="2012-05-28"),"is_holiday"] = "Yes"   #Memorial Day
sales[which(sales$date=="2012-06-17"),"is_holiday"] = "Yes"   #Fathers' Day
sales[which(sales$date=="2012-07-04"),"is_holiday"] = "Yes"   #Independence Day
sales[which(sales$date=="2012-09-03"),"is_holiday"] = "Yes"   #Labor Day
sales[which(sales$date=="2012-10-08"),"is_holiday"] = "Yes"   #Columbus  Day
sales[which(sales$date=="2012-10-31"),"is_holiday"] = "Yes"   #Halloween   
sales[which(sales$date=="2012-11-06"),"is_holiday"] = "Yes"   #Election  Day
sales[which(sales$date=="2012-11-11"),"is_holiday"] = "Yes"   #Veterans  Day
sales[which(sales$date=="2012-11-12"),"is_holiday"] = "Yes"   
sales[which(sales$date=="2012-11-22"),"is_holiday"] = "Yes"   #Thanksgiving  Day
sales[which(sales$date=="2012-12-24"),"is_holiday"] = "Yes"   #Christmas Eve
sales[which(sales$date=="2012-12-25"),"is_holiday"] = "Yes"   #Christmas Day
sales[which(sales$date=="2012-12-31"),"is_holiday"] = "Yes"   #New Year's Eve

sales[which(sales$date=="2013-01-01"),"is_holiday"] = "Yes"   #New Year's Day
sales[which(sales$date=="2013-01-21"),"is_holiday"] = "Yes"   #Martin Luther King Day
sales[which(sales$date=="2013-02-14"),"is_holiday"] = "Yes"   #Valentine's Day
sales[which(sales$date=="2013-02-18"),"is_holiday"] = "Yes"   #Presidents'  Day
sales[which(sales$date=="2013-03-31"),"is_holiday"] = "Yes"   #Easter  Day
sales[which(sales$date=="2013-05-12"),"is_holiday"] = "Yes"   #Mothers' Day
sales[which(sales$date=="2013-05-27"),"is_holiday"] = "Yes"   #Memorial  Day
sales[which(sales$date=="2013-06-16"),"is_holiday"] = "Yes"   #Fathers' Day
sales[which(sales$date=="2013-07-04"),"is_holiday"] = "Yes"   #Independence Day
sales[which(sales$date=="2013-09-02"),"is_holiday"] = "Yes"   #Labor  Day
sales[which(sales$date=="2013-10-14"),"is_holiday"] = "Yes"   #Columbus  Day
sales[which(sales$date=="2013-10-31"),"is_holiday"] = "Yes"   #Halloween 
sales[which(sales$date=="2013-11-11"),"is_holiday"] = "Yes"   #Veterans  Day
sales[which(sales$date=="2013-11-28"),"is_holiday"] = "Yes"   #Thanksgiving 
sales[which(sales$date=="2013-12-24"),"is_holiday"] = "Yes"
sales[which(sales$date=="2013-12-25"),"is_holiday"] = "Yes"
sales[which(sales$date=="2013-12-31"),"is_holiday"] = "Yes"

sales[which(sales$date=="2014-01-01"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-01-20"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-02-14"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-02-17"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-04-13"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-04-20"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-05-11"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-05-26"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-06-15"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-07-04"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-09-01"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-10-13"),"is_holiday"] = "Yes"
sales[which(sales$date=="2014-10-31"),"is_holiday"] = "Yes"

sales$is_blackFriday="No"
sales[which(sales$date=="2012-11-23"),"is_blackFriday"] = "Yes"
sales[which(sales$date=="2013-11-29"),"is_blackFriday"] = "Yes"

sales$is_blackFriday=as.factor(sales$is_blackFriday)
sales$is_holiday=as.factor(sales$is_holiday)

sales$month = months(sales$date)
sales$month = as.factor(sales$month)


#############################################
#           4.Feature enginering            #
###              (2) weather             ####
#############################################


#transform snowfall and preciptotal as numeric and tranform "T" as 0.0 
sales$snowfall=as.character(sales$snowfall)
sales$preciptotal=as.character(sales$preciptotal)
sales[which(sales$snowfall=="  T"),17]="0.0"
sales[which(sales$preciptotal=="  T"),18]="0.0"
sales$snowfall <- as.numeric(sales$snowfall)
sales$preciptotal <- as.numeric(sales$preciptotal)

#create weather feature
sales$storm = FALSE
sales$storm[which(sales$preciptotal>0.1)] = TRUE
sales$hot = FALSE
sales$hot[which(sales$tmax>=90)] = TRUE
sales$cold = FALSE
sales$cold[which(sales$tmax<=45)] = TRUE
sales$id <- droplevels(sales$id)

#Correlation analysis
# subSales.weather=sales[,c("units","tmax","tmin","tavg","depart","dewpoint","wetbulb","heat","cool",
#                           "sunrise","sunset",
#                           "snowfall","preciptotal",
#                           "stnpressure","sealevel","resultspeed","resultdir","avgspeed")]
# 
# subSales.weather=na.omit(subSales.weather)
# 
# cor(subSales.weather)
# 
# pairs(sales[,c("units","tmax","tmin","tavg","depart","dewpoint","wetbulb","heat","cool")])
# pairs(sales[,c("tmax","sunrise","sunset","snowfall","preciptotal")])
# pairs(sales[,c("tmax","stnpressure","resultspeed","resultdir","avgspeed")])


#############################################
#            5.precess test set             #
#############################################

#load test data
test <- read.csv("test.csv",header = TRUE)
test1 <- merge(weather,key,by="station_nbr")
test <- merge(test,test1,by=c("date","store_nbr"),sort = FALSE)
#Add features
test$id <- as.factor(paste(test$store_nbr,test$item_nbr,sep="_"))
test$days <- as.numeric(as.Date(test[,"date"],"%Y-%m-%d")-as.Date("2012-1-1","%Y-%m-%d")+1)
test$date=as.Date(test$date)
test$dayOfquarters=quarters(test$date) #factor:Q1,Q2,Q3,Q4
test$dayOfmonth=as.POSIXlt(test$date)$mday       #1:31
test$dayOfweek=weekdays(test$date)  #Monday,Tuesday,...
test$dayOfquarters=as.factor(test$dayOfquarters)
test$dayOfweek=as.factor(test$dayOfweek)
test$is_holiday="No"
test[which(test$date=="2012-01-01"),"is_holiday"] = "Yes"   #New Year's Day
test[which(test$date=="2012-01-02"),"is_holiday"] = "Yes"   
test[which(test$date=="2012-01-16"),"is_holiday"] = "Yes"   #Martin Luther King Day
test[which(test$date=="2012-02-14"),"is_holiday"] = "Yes"   #Valentine's Day
test[which(test$date=="2012-02-20"),"is_holiday"] = "Yes"   #Presidents' Day
test[which(test$date=="2012-04-08"),"is_holiday"] = "Yes"   #Easter Sunday
test[which(test$date=="2012-05-13"),"is_holiday"] = "Yes"   #Mothers' Day 
test[which(test$date=="2012-05-28"),"is_holiday"] = "Yes"   #Memorial Day
test[which(test$data=="2012-06-17"),"is_holiday"] = "Yes"   #Fathers' Day
test[which(test$date=="2012-07-04"),"is_holiday"] = "Yes"   #Independence Day
test[which(test$date=="2012-09-03"),"is_holiday"] = "Yes"   #Labor Day
test[which(test$date=="2012-10-08"),"is_holiday"] = "Yes"   #Columbus  Day
test[which(test$date=="2012-10-31"),"is_holiday"] = "Yes"   #Halloween   
test[which(test$date=="2012-11-06"),"is_holiday"] = "Yes"   #Election  Day
test[which(test$date=="2012-11-11"),"is_holiday"] = "Yes"   #Veterans  Day
test[which(test$date=="2012-11-12"),"is_holiday"] = "Yes"   
test[which(test$date=="2012-11-22"),"is_holiday"] = "Yes"   #Thanksgiving  Day
test[which(test$date=="2012-12-24"),"is_holiday"] = "Yes"   #Christmas Eve
test[which(test$date=="2012-12-25"),"is_holiday"] = "Yes"   #Christmas Day
test[which(test$date=="2012-12-31"),"is_holiday"] = "Yes"   #New Year's Eve
test[which(test$date=="2013-01-01"),"is_holiday"] = "Yes"   #New Year's Day
test[which(test$date=="2013-01-21"),"is_holiday"] = "Yes"   #Martin Luther King Day
test[which(test$date=="2013-02-14"),"is_holiday"] = "Yes"   #Valentine's Day
test[which(test$date=="2013-02-18"),"is_holiday"] = "Yes"   #Presidents'  Day
test[which(test$date=="2013-03-31"),"is_holiday"] = "Yes"   #Easter  Day
test[which(test$date=="2013-05-12"),"is_holiday"] = "Yes"   #Mothers' Day
test[which(test$date=="2013-05-27"),"is_holiday"] = "Yes"   #Memorial  Day
test[which(test$date=="2013-06-16"),"is_holiday"] = "Yes"   #Fathers' Day
test[which(test$date=="2013-07-04"),"is_holiday"] = "Yes"   #Independence Day
test[which(test$date=="2013-09-02"),"is_holiday"] = "Yes"   #Labor  Day
test[which(test$date=="2013-10-14"),"is_holiday"] = "Yes"   #Columbus  Day
test[which(test$date=="2013-10-31"),"is_holiday"] = "Yes"   #Halloween 
test[which(test$date=="2013-11-11"),"is_holiday"] = "Yes"   #Veterans  Day
test[which(test$date=="2013-11-28"),"is_holiday"] = "Yes"   #Thanksgiving 
test[which(test$date=="2013-12-24"),"is_holiday"] = "Yes"
test[which(test$date=="2013-12-25"),"is_holiday"] = "Yes"
test[which(test$date=="2013-12-31"),"is_holiday"] = "Yes"
test[which(test$date=="2014-01-01"),"is_holiday"] = "Yes"
test[which(test$date=="2014-01-20"),"is_holiday"] = "Yes"
test[which(test$date=="2014-02-14"),"is_holiday"] = "Yes"
test[which(test$date=="2014-02-17"),"is_holiday"] = "Yes"
test[which(test$date=="2014-04-13"),"is_holiday"] = "Yes"
test[which(test$date=="2014-04-20"),"is_holiday"] = "Yes"
test[which(test$date=="2014-05-11"),"is_holiday"] = "Yes"
test[which(test$date=="2014-05-26"),"is_holiday"] = "Yes"
test[which(test$date=="2014-06-15"),"is_holiday"] = "Yes"
test[which(test$date=="2014-07-04"),"is_holiday"] = "Yes"
test[which(test$date=="2014-09-01"),"is_holiday"] = "Yes"
test[which(test$date=="2014-10-13"),"is_holiday"] = "Yes"
test[which(test$date=="2014-10-31"),"is_holiday"] = "Yes"
test$is_blackFriday="No"
test[which(test$date=="2012-11-23"),"is_blackFriday"] = "Yes"
test[which(test$date=="2013-11-29"),"is_blackFriday"] = "Yes"
test$is_blackFriday=as.factor(test$is_blackFriday)
test$is_holiday=as.factor(test$is_holiday)
test$month = months(test$date) #factor:Janu.
test$month = as.factor(test$month)
test$snowfall=as.character(test$snowfall)
test$preciptotal=as.character(test$preciptotal)
test[which(test$snowfall=="  T"),17]="0.0"
test[which(test$preciptotal=="  T"),18]="0.0"
test$snowfall <- as.numeric(test$snowfall)
test$preciptotal <- as.numeric(test$preciptotal)
test$storm = FALSE
test$storm[which(test$preciptotal>0.1)] = TRUE
test$hot = FALSE
test$hot[which(test$tmax>=90)] = TRUE
test$cold = FALSE
test$cold[which(test$tmax<=45)] = TRUE


#(2)used in KNN (transformation as numeric)
sales$week <- as.POSIXlt(sales$date)$wday
test$week <- as.POSIXlt(test$date)$wday
sales$quarters <- as.numeric(substr(sales$dayOfquarters,2,2))
test$quarters <- as.numeric(substr(test$dayOfquarters,2,2))
sales$blackFriday <- as.integer(ifelse(sales$is_blackFriday=="Yes",1,0))
sales$holiday <- as.integer(ifelse(sales$is_holiday=="Yes",1,0))
test$blackFriday <- as.integer(ifelse(test$is_blackFriday=="Yes",1,0))
test$holiday <- as.integer(ifelse(test$is_holiday=="Yes",1,0))


#############################################
#####        save new feature            ####
#############################################
write.csv(sales,file = "sales.csv",row.names = FALSE)
save(sales,file="sales.RData")
write.csv(test,file = "sales_test.csv",row.names = FALSE)
save(test,file="test.RData")


#############################################
#              6.Plot                       #
#############################################
#Sales vs. date plot for several products

#Sales for several products

#1_28
id="1_28"
test$units = "NA"
sales_i = sales[which(sales$id==id),
                c("units","date")]
sales_i.test = test[which(test$id==id),
                    c("units","date")]
sales_i.test$units = "NA"

timeseries_i <- rbind(sales_i,sales_i.test)

timeseries <- timeseries_i[order(timeseries_i$date),"units"]

ts_i=ts(timeseries,start = c(2012),frequency = 365.25)

plot.ts(ts_i,col="blue")

#2_93
id="2_93"

#3_45
id="3_45"

#5_93
id="5_93"

#9_93
id="9_93"




#############################################
#   7. Model 1: linear regression           #
#############################################

#source("LR.R")


#############################################
#    8. Model 2: KNN                        #
#############################################

#source("KNN.R")

#######################################################
#   9. Model 3: Ridge regression                      #
#######################################################

#source("Rid.R")


#############################################
#   10. Model 4: Lasso                      #
#############################################

#source("Lasso.R")



#############################################
#   11. Model 5: PCA                        #
#############################################
#source("PCA.R")


#############################################
#   12. Model 6: PLS                        #
#############################################
#source("PLS.R")


#############################################
#   13. Model 7: Best subset selection      #
#############################################
#source("Best selection.R")


#############################################
#   14. Model 8: Polynomial regression      #
#############################################
#source("PolyR.R")

#############################################
#   15. Model 9: Regression spline          #
#############################################
#source("PolyR.R")

