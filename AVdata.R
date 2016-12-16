path <- c("/home/nlp/Documents/AVData/")

setwd(path)

train <-read.csv("train_gbW7HTd.csv")
test <-read.csv("test_2AFBew7.csv")

str(train)
str(test)

train_cont <- subset(train, select = c(ID,Age,Hours.Per.Week))
train_Cat <- subset(train,select = -c(ID,Age,Hours.Per.Week))

##########Univariate Analysis #############################
##########1. Continuous Variables:#########################
str(train_cont)
summary(train_cont)
library(pastecs)
options(scipen = 100)
options(digits = 2)
stat.desc(train_cont)



################Categorical Variables#######################
apply(train_Cat, 2, function(x){length(unique(x))})
 
unique(train_Cat$Relationship)

head(sort(table(train_Cat$Native.Country), decreasing = TRUE),10)
head(round(sort(prop.table(table(train_Cat$Native.Country)), decreasing = TRUE),6),10)


IQR(test$Age)


###############Multivariate Analysis####################

library(gmodels)  

CrossTable(train$Sex , train$Income.Group)
library(ggplot2)

ggplot(train,aes(Sex,fill =Income.Group)) + geom_bar() + theme_bw()


###########Missing Value Treatment#################

table(is.na(train))

colSums(is.na(train))

table(is.na(test))

colSums(is.na(test))

library("mlr")
?impute
impute_Data <- impute(train , classes = list(factor = imputeMode()))
impute_Data

##########Outlier################

library(ggplot2)

ggplot(train, aes(ID,Age)) + geom_jitter()


##Vaiable Trans########
sapply(train, class)
table(train$Workclass)
library(car)
train$Workclass <- recode(train$Workclass, "c('State-gov','Federal-gov',' Never-worked ','Self-emp-inc','Without-pay')= 'others'")
table(train$Workclass)
table(is.na(train$Workclass))
test$Workclass <- recode(test$Workclass, "c('State-gov','Federal-gov',' Never-worked ','Self-emp-inc','Without-pay')= 'others'")
table(test$Workclass)
table(is.na(test$Workclass))

as.matrix(prop.table(table(train$Workclass)))
as.matrix(prop.table(table(test$Workclass)))


###########Modeling####################

table(train$Income.Group)

train$Income.Group <- ifelse(train$Income.Group == "<=50K",1,0)
head(train$Income.Group)
table(train$Income.Group)


train <- subset(train, select = -c(ID))

library(rpart)
set.seed(500)
train.tree <- rpart(Income.Group ~ ., data = train , method = "class" , control = rpart.control(minsplit = 20, minbucket = 100, maxdepth = 10, xval = 5) )

summary(train.tree)

library(rpart.plot)

prediction_train <- predict(train.tree , data = train , type = "class")
prediction_test <- predict(train.tree, newdata = test , type = "class")

table(prediction_test)

library(caret)

confusionMatrix(prediction_train , train$Income.Group)

solution <- data.frame(ID = test$ID , Income.Group = prediction_test)
write.csv(solution , file = "sample_submission.csv")


















