getwd()
setwd("bdtc-2014/")
#
print("Clearing Workspace ...")
rm(list = ls(all = TRUE))
#
gc(TRUE)
gcinfo(FALSE)
#
library(dplyr)
train = read.csv('bike/train.csv')
test = read.csv('bike/test.csv')
#
head(train)
head(test)
#
str(train)
#
train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
#
# Process Time
#
# year,month,day,day of week
#
train$datefull <- as.POSIXct(train$datetime)
#
install.packages("lubridate")
require(lubridate)
#
train$hour <- as.factor(hour(ymd_hms(train$datefull)))
train$wday <- as.factor(wday(ymd_hms(train$datefull)))
#
# Ideas
#
# 1. Sunday ? Separae variable as a hint that on Sunday less bikes are rented (after a table)
train %>% group_by(wday) %>% summarise(counts=sum(count))
aggregate(count ~ wday,data=train,FUN=sum)
# Looks OK - not that different for any day
# 2. Daypart than individual hours ?
train %>% group_by(hour) %>% summarise(counts=sum(count))
# 
# party recursive partitioning (Thanks Brandon)
# http://cran.r-project.org/web/packages/party/vignettes/party.pdf
# http://www.r-bloggers.com/package-party-conditional-inference-trees/
#
install.packages('party')
library('party')
#
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + wday
fit.ctree <- ctree(formula, data=train)
#
fit.ctree
plot(fit.ctree)
#
# transform test data
#
test$season <- as.factor(test$season)
test$holiday <- as.factor(test$holiday)
test$workingday <- as.factor(test$workingday)
test$weather <- as.factor(test$weather)
#
test$datefull <- as.POSIXct(test$datetime)
test$hour <- as.factor(hour(ymd_hms(test$datefull)))
test$wday <- as.factor(wday(ymd_hms(test$datefull)))
#
pred <- predict(fit.ctree, test)
test$count <- pred
out <- test %>% select(datetime,count)
write.csv(out, file="bike/sub-01.csv",row.names = FALSE)
#
# Submit & it should give 0.51343, rank : 451
#
# Add Year & Month
train$year <- as.factor(year(ymd_hms(train$datefull)))
train$month <- as.factor(month(ymd_hms(train$datefull)))
#
test$year <- as.factor(year(ymd_hms(test$datefull)))
test$month <- as.factor(month(ymd_hms(test$datefull)))
#
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + wday + year + month
fit.ctree <- ctree(formula, data=train)
#
pred <- predict(fit.ctree, test)
test$count <- pred
out <- test %>% select(datetime,count)
write.csv(out, file="bike/sub-02.csv",row.names = FALSE)
#
# Submit & it should give 0.52667, didn't improve
#
library(randomForest)
mdl_rf <- randomForest(formula, data = train,importance = TRUE)
mdl_rf
importance(mdl_rf)
#
pred <- predict(mdl_rf,newdata = test)
test$count <- pred
out <- test %>% select(datetime,count)
write.csv(out, file="bike/sub-03.csv",row.names = FALSE)
#
# Submit & it should give 0.63424, didn't improve
#

