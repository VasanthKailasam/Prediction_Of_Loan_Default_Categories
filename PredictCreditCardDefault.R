library(caret)
library(rpart)
library(klaR)
library(e1071)
library(caTools)
library(gbm)
library(randomForest)

LoanData <- Loan_payments_data
LoanData <- LoanData[,c(3:6, 9:11, 2)]
LoanData$effective_date <- as.Date(LoanData$effective_date, "%m/%d/%Y")
LoanData$due_date <- as.Date(LoanData$due_date,"%m/%d/%Y")
LoanData$education[LoanData$education=="Bechalor"] <- "Bachelor"
LoanData$education[LoanData$education=="college"] <- "CommunityCollege"
LoanData$terms <- as.factor(LoanData$terms)
LoanData$Gender <- as.factor(LoanData$Gender)
LoanData$loan_status <- as.factor(LoanData$loan_status)
LoanData$education <- as.factor(LoanData$education)
LoanData$window <- LoanData$due_date - LoanData$effective_date
LoanData$window <- as.numeric(LoanData$window)
final <- LoanData[,-c(3,4)]

set.seed(5678)
final <- final[sample(nrow(final)),]
folds <- cut(seq(1,nrow(final)),breaks=10,labels=FALSE)
accuracylist_nb <- rep(0,10)
accuracylist_rp <- rep(0,10)
accuracylist_gb <- rep(0,10)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final[testIndexes, ]
  trainData <- final[-testIndexes, ]
  nbmodel <- naiveBayes(loan_status~., data = trainData)
  rpmodel <- rpart(loan_status~., data =trainData, cp=0)
  randomforest_model <-  randomForest(loan_status~., data=trainData)
  prediction <- predict(nbmodel, testData)
  prediction1 <- predict(rpmodel, testData, type = 'class')
  prediction3 <- predict(randomforest_model, newdata=testData)
  # pred_class <- apply(prediction3,1,which.max)
  # pred_class[pred_class == 1] <- "COLLECTION"
  # pred_class[pred_class == 2] <- "COLLECTION_PAIDOFF"
  # pred_class[pred_class == 3] <- "PAIDOFF"
  nboutput <- confusionMatrix(testData$loan_status, prediction)
  rpoutput <- confusionMatrix(testData$loan_status, prediction1)
  rfoutput <- confusionMatrix(testData$loan_status, prediction3)
  accuracylist_nb[i] <- paste0(nboutput$overall[1])
  accuracylist_rp[i] <- paste0(rpoutput$overall[1])
  accuracylist_gb[i] <- paste0(gboutput$overall[1])
}






