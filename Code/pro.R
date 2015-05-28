#setting working directory
path <- "/home/jerry/ali"
setwd(path)

#load the libs
library(plyr)
library(forecast)

## loading data (edit the paths)
users <- read.csv("./Raw/user_profile_table.csv", stringsAsFactors=F)
users_balance <- read.table("./Raw/user_balance_table.txt", stringsAsFactors=F,sep=",",head=T)
mfd_day_share_interest <- read.csv("./Raw//mfd_day_share_interest.csv",stringsAsFactors=F)
mfd_bank_shibor <-read.csv("./Raw//mfd_bank_shibor.csv",stringsAsFactors=F)

#Time-Series
users_balance$report_date <- as.factor(users_balance$report_date)
purchase <- ddply(users_balance,'report_date',summarise,purchases = sum(total_purchase_amt))
redeem <- ddply(users_balance,'report_date',summarise,redeems = sum(total_redeem_amt))
purchase_ts <- ts(purchase$purchases,frequency = 1,start = c(2013))
redeem_ts <- ts(redeem$redeems,frequency = 1,start = c(2013))
redeem_log_ts <- ts(log(redeem$redeems),frequency = 1,start = c(2013))

#purchasediff<-diff(purchase_ts,differences=1)

#redeemforecasts <- HoltWinters(redeem_ts, beta=FALSE, gamma=FALSE)
#redeemforecasts2 <- forecast.HoltWinters(redeemforecasts, h=30)

purchasearima <- auto.arima(purchase_ts)
autopurchasearimaforecast<-forecast.Arima(purchasearima,h=30,level=c(99.5))
plot.forecast(autopurchasearimaforecast)
autoforecastpurchase <- data.frame(autopurchasearimaforecast)
redeemarima <- auto.arima(redeem_ts)
autoredeemarimaforecast <- forecast.Arima(redeemarima,h=30,level=c(99.5))
autoforecastredeem <- data.frame(autoredeemarimaforecast)
fr2 <- data.frame(date=seq(from = 20140901,to=20140930),purchase=round(autoforecastpurchase$Point.Forecast,0),redeem=round(autoforecastredeem$Point.Forecast,0))
write.table(fr2,file = './Submission//tc_comp_predict_table2.csv',row.names = F,col.names = F,sep = ',')
#data-2013071
mfd_bank_shibor$days <- as.numeric(as.Date(as.character(mfd_bank_shibor$mfd_date),"%Y%m%d")-as.Date("20130701","%Y%m%d"))
mfd_bank_shibor$year <- substr(as.character(mfd_bank_shibor$mfd_date),1,4)
mfd_bank_shibor$month <- substr(as.character(mfd_bank_shibor$mfd_date),5,6)
mfd_bank_shibor$day <- substr(as.character(mfd_bank_shibor$mfd_date),7,8)
mfd_bank_shibor$weekday <- weekdays(as.Date(as.character(mfd_bank_shibor$mfd_date),"%Y%m%d"))

mfd_day_share_interest$days <- as.numeric(as.Date(as.character(mfd_day_share_interest$mfd_date),"%Y%m%d")-as.Date("20130701","%Y%m%d"))
mfd_day_share_interest$year <- substr(as.character(mfd_day_share_interest$mfd_date),1,4)
mfd_day_share_interest$month <- substr(as.character(mfd_day_share_interest$mfd_date),5,6)
mfd_day_share_interest$day <- substr(as.character(mfd_day_share_interest$mfd_date),7,8)
mfd_day_share_interest$weekday <- weekdays(as.Date(as.character(mfd_day_share_interest$mfd_date),"%Y%m%d"))

panel <- data.frame()
panel$days <- users_balance$days

users_balance$days <- as.numeric(as.Date(as.character(users_balance$report_date),"%Y%m%d")-as.Date("20130701","%Y%m%d"))
mfd_day_share_interest$days <- as.numeric(as.Date(as.character(mfd_day_share_interest$mfd_date),"%Y%m%d")-as.Date("20130701","%Y%m%d"))
samples <- read.csv("./Submission//（example）tc_comp_predict_table.csv",stringsAsFactors=F,head=F)
