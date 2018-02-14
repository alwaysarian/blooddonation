library(doParallel)
registerDoParallel(cores=2)
foreach(i=1:3) %dopar% sqrt(i)

train <- read.csv('train.csv', as.is = T);
test <- read.csv('test.csv', as.is = T);

train <- train[,c(1,2,3,5,6)]
test <- test[,c(1,2,3,5)]

#Scaling value fro train manually

train$Months.since.Last.Donation = (train$Months.since.Last.Donation - mean(train$Months.since.Last.Donation)) / sd(train$Months.since.Last.Donation)
train$Number.of.Donations = (train$Number.of.Donations - mean(train$Number.of.Donations)) / sd(train$Number.of.Donations)
train$Months.since.First.Donation = (train$Months.since.First.Donation - mean(train$Months.since.First.Donation)) / sd(train$Months.since.First.Donation)

#For attempt 2


test$Months.since.Last.Donation = (test$Months.since.Last.Donation - mean(train$Months.since.Last.Donation)) / sd(train$Months.since.Last.Donation)
test$Number.of.Donations = (test$train$Number.of.Donations - mean(train$train$Number.of.Donations)) / sd(train$train$Number.of.Donations)
test$Months.since.First.Donation = (test$Months.since.First.Donation - mean(train$Months.since.First.Donation)) / sd(train$Months.since.First.Donation)

#Scaling Value for Test


#test$Months.since.Last.Donation = (test$Months.since.Last.Donation - mean(test$Months.since.Last.Donation)) / sd(test$Months.since.Last.Donation)
#test$Number.of.Donations = (test$Number.of.Donations - mean(test$Number.of.Donations)) / sd(test$Number.of.Donations)
#test$Months.since.First.Donation = (test$Months.since.First.Donation - mean(test$Months.since.First.Donation)) / sd(test$Months.since.First.Donation)

dm.navg <- avNNet(train[c(2:4)], train$Made.Donation.in.March.2007, size = 2, rang
                  = 0.5, decay = 5e-6, maxit = 200, repeats = 50)

nn_pred<-predict(dm.navg, test, type = "class")


FINALSUB_nn <-data.frame(test$X, nn_pred)

FINALSUB_nn




+ Accuracies[i] <- confusionMatrix(dm$group[-inTrain], predict(dm.navg, dm[-
                                                                             inTrain,], type = "class"))$overall["Accuracy"]






train <- train[,c(1,scale(2,3,5))]
sca


for (i in 3)
{
  x <- train[,i]
  y <- colnames(train[i])
  OUT <- Boxplot(x)
  list1 <- cbind(train[OUT,c("X",y)])
  a <- print(list1)
  a
  train[Boxplot(x),y] <- NA
  train <- na.omit(train)
}

knn4 <- train(Made.Donation.in.March.2007 ~ ., data = train, method = "knn",
              
              preProcess = c("center", "scale"), tuneLength = 10,
              
              trControl = trainControl(method = "cv"))

update(knn4, list(.k = 13))
knn4_pred <- predict(knn4,newdata = test)
FINALSUB <-data.frame(test$X, knn4_pred)

FINALSUB


Boxplot(train$Number.of.Donations)



Boxplot(train$Number.of.Donations)