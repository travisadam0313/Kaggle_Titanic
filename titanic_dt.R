library(rpart)
library(rpart.plot)
library(rattle)
library(readxl)
library(randomForest)

#Set seed and working directory
set.seed(313)
setwd("~/Desktop/Programming/R/Titanic")

#Import Data
  #NA's in the age column have been manually populated prior to import
  #with averages based off Sex and Pclass
test_n_df<-read.csv("test_n.csv")
train_n_df<-read.csv("train_n.csv")

#Explore Data
pairs(train_df[,c("Survived","Pclass","Age","Fare")],na.action = na.omit)
train_df$Sex<-as.factor(train_df$Sex)
plot(train_df$Age,train_df$Fare)

#Format the Data
train_n_df$Pclass<-as.factor(train_n_df$Pclass)
train_n_df$Sex<-as.factor(train_n_df$Sex)
test_n_df$Pclass<-as.factor(test_n_df$Pclass)
test_n_df$Sex<-as.factor(test_n_df$Sex)

#Identify optimal complexity parameter
control <- rpart.control(cp=0,minbucket = 10)
model <- rpart(Survived ~
              Sex +
              Pclass +
              Fare +
              SibSp,
              data= train_n_df,
              method = "class")
              #,control = control)


print(model$cptable)
#plotcp(model)
#grid()
cp_val<-model$cptable[which.min(model$cptable[,4]),1]
min_split<-model$cptable[which.min(model$cptable[,4]),2]

#Assemble decision tree formula from rpart library
fit <- rpart(Survived ~
              Age +
              Sex +
              Pclass +
              Fare +
              SibSp,
             data= train_n_df,
             method = "class")
#,control=rpart.control(minsplit=min_split, cp=cp_val))

fancyRpartPlot(fit,cex=.7, main = "Survivors")

#Prepare decision tree submission
Prediction <- predict(fit, test_n_df, type = "class")
submit <- data.frame(PassengerId = test_n_df$PassengerId, Survived = Prediction)
write.csv(submit, file = "DT2.csv", row.names = FALSE)

#Create fit model
rf_fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + SibSp,
                    data=train_n_df, 
                    importance=TRUE, 
                    ntree=2000)
#Assess Variables for Importance
plot(rf_fit)
varImpPlot(rf_fit)

#Prepare submission
test_n_df$Survived<-0
Prediction <- predict(rf_fit, test_n_df)
submit <- data.frame(PassengerId = test_n_df$PassengerId, Survived = Prediction)
write.csv(submit, file = "RF1.csv", row.names = FALSE)
#1,439th Place on the Leader board at 0.78947 accuracy.
