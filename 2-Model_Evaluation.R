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
df_train = read.csv('titanic-r/train.csv')
df_test = read.csv('titanic-r/test.csv')
#
head(df_train)
df_train_all <- df_train %>% select(-Name,-Ticket,-Cabin,-Embarked,-Age)
df_train_all$Survived <- as.factor(df_train_all$Survived)
head(df_train_all)
nrow(df_train_all)
#
index_set <- 1:nrow(df_train_all)
split_set <- sample(index_set,0.80*length(index_set))
train_set <- df_train_all[split_set,]
train_set_y <- train_set %>% select(Survived)
test_set <- df_train_all[-split_set,]
test_set_y <- test_set %>% select(Survived)
891 * .8
#
spl = sample.split(df_train_all$Survived, 0.8)
train = subset(df_train_all, spl == TRUE)
test = subset(df_train_all, spl == FALSE)
#
library(randomForest)
mdl_rf <- randomForest(Survived ~ Pclass + Sex + SibSp+Parch+Fare, data = train_set,importance = TRUE)
mdl_rf
importance(mdl_rf)
#
mdl_rf$confusion
#
pred <- predict(mdl_rf,newdata = (test_set %>% select(-PassengerId)))
pred
#
accuracy = sum(pred==test_set_y$Survived)/length(pred)
print (sprintf("Accuracy = %3.2f %%",accuracy*100))
#
tab <- table(pred, true = test_set_y$Survived)
#
classAgreement(tab)
#
rf.cv <- rfcv(trainx = (df_train_all %>% select(-Survived,-PassengerId)), trainy = df_train_all$Survived, cv.fold=10)
with(rf.cv, plot(n.var, error.cv))
#
library("ROCR")
pred_01 <- prediction(pred,test_set_y)
rf_ROCR <- performance(pred_01, "tpr", "fpr")
plot(rf_ROCR, colorize=TRUE)
#
# Compute AUC
auc <- performance(pred_01, "auc")
print (auc)
#
# Home work : Compute AUC for svm with different kernels
#
