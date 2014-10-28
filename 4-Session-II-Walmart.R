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
train = read.csv('walmart/train.csv')
test = read.csv('walmart/test.csv')
#
str(train)
head(train)
#
head(test)
#
features = read.csv('walmart/features.csv')
str(features)
head(features)
#
# Don't think we will have time to explore this
# Hope you all will spend some time understanding the data, time series et al
#