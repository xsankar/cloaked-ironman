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
sessionInfo()
df_train = read.csv('titanic-r/train.csv')
df_test = read.csv('titanic-r/test.csv')
#
head(df_train)
summary(df_train)
#
glimpse(df_train)
#
str(df_train)
#
female_count <- nrow(df_train %>% filter(Sex == 'female'))
female_survived <- nrow( df_train %>% filter(Sex == 'female',Survived == 1) )
print (sprintf("F-Survived   = %2.2f %%", 100 * (female_survived/female_count)))
#
male_count <- nrow(df_train %>% filter(Sex == 'male'))
male_survived <- nrow( df_train %>% filter(Sex == 'male',Survived == 1) )
print (sprintf("M-Survived   = %2.2f %%", 100 * (male_survived/male_count)))
#
print (sprintf("All-Survived   = %2.2f %%", 100 * (nrow( df_train %>% filter(Survived == 1) )/nrow(df_train))))
print (sprintf("All-Not Survived   = %2.2f %%", 100 * (nrow( df_train %>% filter(Survived == 0) )/nrow(df_train))))
#
head(df_test)
#
# 1 : Simple Model (M=Survived) 
#
df_test$Survived <- ifelse(df_test$Sex == "male",1,0)
head(df_test)
#
out <- df_test%>% select(PassengerId,Survived)
write.csv(out,"titanic-r/r-sub-01.csv",row.names = FALSE)
#
# Submit & it should give 0.23445, Rank 2681
#
#
# 2 : Simple Model (F=Survived) 
#
df_test$Survived <- ifelse(df_test$Sex == "female",1,0)
head(df_test)
#
out <- df_test %>% select(PassengerId,Survived)
write.csv(out,"titanic-r/r-sub-02.csv",row.names = FALSE)
#
# Submit & it should give 0.76555, Rank 2232
#
# Would age be a better predictor ?
#
table_1 <- table(df_train$Age,df_train$Survived)
table_1
View(table_1)
# Looks like No
#
# *** Home work : See if Pclass, SibSp or Parch is a better indication and change survival accordingly¶
#
#
# Now clean the data for applying tree algorithms
#
#
# 3 : Simple Tree Model (CART)
#
head(df_train)
df_train_1 <- df_train %>% select(-Name,-Ticket,-Cabin,-Embarked,-Age)
head(df_train_1)
#
library(rpart)
mdl_tree <- rpart(Survived ~ Pclass + Sex + SibSp+Parch+Fare, data = df_train_1, method = "class")
printcp(mdl_tree)
plotcp(mdl_tree)
print(mdl_tree)
plot(mdl_tree)
text(mdl_tree, use.n = TRUE,all = TRUE)
#
summary(mdl_tree)
#
# Let us see if we are better off
#
# Now, transform the test data¶
#
df_test_1 <- df_test %>% select(-Name,-Ticket,-Cabin,-Embarked,-Age)
df_test_1$Survived <- NULL # Delete the column we had created last time
str(df_test_1)
#
pred <- predict(mdl_tree,newdata = (df_test %>% select(-PassengerId)), type="class")
pred
df_test_1$Survived <- pred
head(df_test_1)
#
out <- df_test_1 %>% select(PassengerId,Survived)
write.csv(out,"titanic-r/r-sub-03.csv",row.names = FALSE)
#
# Submit & it should give 0.78947, Rank 809 !
#
# May be better because we have improved the survival rate of men !
#
#
# 4 : Ensamble/Bagging Tree Model (Random Forest)
#
library(randomForest)
?randomForest
mdl_rf <- randomForest(Survived ~ Pclass + Sex + SibSp+Parch+Fare, data = df_train_1,importance = TRUE)
mdl_rf
importance(mdl_rf)
#
summary(df_test_1)
df_test_1$Fare[is.na(df_test_1$Fare)] <- 35.0
#
pred <- predict(mdl_rf,newdata = (df_test_1 %>% select(-PassengerId)))
pred
df_test_1$Survived <- pred
head(df_test_1)
#
out <- df_test_1 %>% select(PassengerId,Survived)
write.csv(out,"titanic-r/r-sub-04.csv",row.names = FALSE)
#
# Submit & it should give 0.77990, not higher
#
# 5 : SVM : radial kernel
#
library(e1071)
mdl_svm <- svm(Survived ~ Pclass + Sex + SibSp+Parch+Fare, data = df_train_1)
mdl_svm
#
# prepare test data
#
str(df_test_1)
df_test_1$Survived <- NULL
#
pred <- predict(mdl_svm,newdata = (df_test_1 %>% select(-PassengerId)))
pred
df_test_1$Survived <- pred
head(df_test_1)
#
out <- df_test_1 %>% select(PassengerId,Survived)
write.csv(out,file="titanic-r/r-sub-05.csv",row.names = FALSE)
#
# Submit & it should give 0.77033, not higher
#
#
# 6 : SVM : sigmoid kernel
#
mdl_svm <- svm(Survived ~ Pclass + Sex + SibSp+Parch+Fare, data = df_train_1,kernel = "sigmoid")
mdl_svm
#
# prepare test data
#
str(df_test_1)
df_test_1$Survived <- NULL
#
pred <- predict(mdl_svm,newdata = (df_test_1 %>% select(-PassengerId)))
pred
df_test_1$Survived <- pred
head(df_test_1)
#
out <- df_test_1 %>% select(PassengerId,Survived)
write.csv(out,file="titanic-r/r-sub-06.csv",row.names = FALSE)
#
# Submit & it should give 0.63158, much lower
#



