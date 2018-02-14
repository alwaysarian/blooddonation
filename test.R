library(caret)
require(rpart)
library(nnet)
require(e1071)

df <- read.csv("bloodgroup.csv", header=T)
test<- read.csv("test.csv", header=T)
test<-test[,c(2:5)]

#scale(test)

#colSums(is.na(df))
#colSums(is.na(test))


workdata1 <- na.omit(df[,c(2,3,4,5,6)])
#test <- cbind(test["Made.Donation.in.March.2007"],scale(test[,c(2,3,4,5)]) )

#workdata1 <- cbind(workdata1["Made.Donation.in.March.2007"], scale(workdata1[,c(1,2,3,4)]) )

dm.navg <- avNNet(workdata1[c(2:5)], workdata1$Made.Donation.in.March.2007, size = 2, rang
                          = 0.5, decay = 5e-6, maxit = 200, repeats = 50)
predict(dm.navg, newdata = test, type = "class")
