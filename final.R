library(caret)
require(rpart)
library(nnet)
require(e1071)

df <- read.csv("bloodgroup.csv", header=T)

colSums(is.na(df))

test<- read.csv("test.csv", header=T)
test <- na.omit(test[,c(2,3,5)])

#Made 
workdata1 <- na.omit(df[,c(2,3,5,6)])


edata1<-rpart(Made.Donation.in.March.2007~ ., data=workdata1,method = "class",
                 parms= list(split = "gini",prior = c(1/2,1/2)),
                 control = rpart.control(usesurrogate= 0, maxsurrogate= 0))
                 
predict(edata1,newdata= test,type = "class")


confusionMatrix(workdata1[inTest,"Made.Donation.in.March.2007"],predict(edata1,newdata= test,type = "class"))

#LDA for Made.Donation.in.March.2007

Accuracies <- c(0.00)
for (i in seq(100))
 {
   inTrain <- createDataPartition(y = workdata1$Made.Donation.in.March.2007, p = .70, list = FALSE)
   training <- workdata1[inTrain,]
   edata2 <- lda(as.matrix(workdata1[,c(1:4)]), workdata1[,"Made.Donation.in.March.2007"], data = workdata1, prior = c(1/2,1/2),
                                      subset = -inTrain, CV = T)
   Accuracies[i] <- confusionMatrix(workdata1[-inTrain,"Made.Donation.in.March.2007"], edata2$class)$overall["Accuracy"]
 }
summary(Accuracies)

confusionMatrix(workdata1[-inTrain,"Made.Donation.in.March.2007"], edata2$class)

#logisitic
test<- read.csv("test.csv", header=T)

glmfit <- glm(Made.Donation.in.March.2007 ~ ., data=workdata1, family=binomial())

GFac <- fitted(glmfit)
thresh <- 0.5
GClass <- cut(GFac, breaks=c(-Inf, thresh, Inf), labels=c('0','1'))

predict(GClass, test) 

confusionMatrix(GClass, test) 


