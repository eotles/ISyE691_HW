#Load Libraries
library(MASS)
library(e1071)
library(nnet)
library(ElemStatLearn)
library(rpart)

#Useful Functions
error <- function(predictions, actuals){
  e = sum(predictions != actuals)/length(actuals)
  return(e)
}

printErrors <- function(trainAct, testAct, trainPred, testPred){
  cat("   Trai Err: ",error(trainPred, trainAct),"\n   Test Err: ",error(testPred, testAct)) 
}

getLDA <- function(trainData, testData, ldaCols){
  cat(" LDA\n")
  ldaMod = lda(trainData[,ldaCols], trainData[,1])
  ldaTrainPred = predict(ldaMod,trainData[,ldaCols])$class
  #print(ldaTrainPred)
  ldaTestPred = predict(ldaMod,testData[,ldaCols])$class
  printErrors(trainData[,1], testData[,1], ldaTrainPred, ldaTestPred) 
  return(ldaMod)
}

getQDA <- function(train, test, qdaCols){
  cat(" QDA\n")
  #note need to apply jitter to all of the columns to avoid rank deficiency
  qdaMod = qda(apply(train[,qdaCols],2,jitter),train[,1])
  qdaTrainPred = predict(qdaMod, train[qdaCols])$class
  #print(qdaTrainPred)
  qdaTestPred = predict(qdaMod, test[qdaCols])$class
  printErrors(train[,1], test[,1], qdaTrainPred, qdaTestPred)
  return(qdaMod)
}

getNB <- function(train, test, nbCols){
  cat(" Naive Bayes\n")
  #nbMod = naiveBayes(as.factor(V1) ~., train)
  nbMod = naiveBayes(train[,nbCols],as.factor(train[,1]))
  nbTrainPred = predict(nbMod, train[,nbCols])
  #print(nbTrainPred)
  nbTestPred = predict(nbMod, test[,nbCols])
  printErrors(train[,1], test[,1], nbTrainPred, nbTestPred)
  return(nbMod)
}

getLR <- function(train, test){
  cat(" Logistic Regression\n")
  lrMod = multinom(as.factor(V1) ~., data=train)
  lrTrainPred = predict(lrMod, train)
  #print(lrTrainPred)
  lrTestPred = predict(lrMod, test)
  printErrors(train[,1], test[,1], lrTrainPred, lrTestPred)
  return(lrMod)
}

getSVM <- function(train, test){
  cat(" SVM\n")
  svm = svm(as.factor(V1) ~., data=train)
  svmTrainPred = predict(svm, train, type="class")
  #print(svmTrainPred)
  svmTestPred = predict(svm, test, type="class")
  printErrors(train[,1], test[,1], svmTrainPred, svmTestPred)
  return(svm)
}

getCART <- function(train, test){
  cat(" CART\n")
  cart = rpart(as.factor(V1)~., data=train, method="class")
  cartTrainPred = predict(cart, train, type="class")
  #print(cartTrainPred)
  cartTestPred = predict(cart, test, type="class")
  printErrors(train[,1], test[,1], cartTrainPred, cartTestPred)
}

#Load & Prepare Data
zip.train = read.table(file="/Users/eotles/Downloads/zip.train.csv",sep=",");
ziptrain23 = subset(zip.train,zip.train[,1]==2|zip.train[,1]==3)
ziptrain235 = subset(zip.train,zip.train[,1]==2|zip.train[,1]==3|zip.train[,1]==5)
zip.test = read.table(file="/Users/eotles/Downloads/zip.test.csv",sep=",");
ziptest23 = subset(zip.test,zip.test[,1]==2|zip.test[,1]==3)
ziptest235 = subset(zip.test,zip.test[,1]==2|zip.test[,1]==3|zip.test[,1]==5)
ytrain23 = ziptrain23[,1]
ytrain235 = ziptrain235[,1]
ytest23 = ziptest23[,1]
ytest235 = ziptest235[,1]

#Problem 1
cat("Problem 1")
#1.1 - Linear Regression
cat(" Linear Regression")
mod1 = lm(V1 ~., data=ziptrain23)
pred = predict.lm(mod1,ziptest23)
lmTrainPred = ifelse(predict.lm(mod1,ziptrain23)<2.5,2,3)
lmTestPred = ifelse(predict.lm(mod1,ziptest23)<2.5,2,3)
printErrors(ziptrain23[,1], ziptest23[,1], lmTrainPred, lmTestPred)
#1.2 - kNN
cat(" kNN")
kSet = c(1,3,5,7,15)
library(class)
for(k in kSet){
  knnTrainPred = knn(ziptrain23, ziptrain23, ziptrain23[,1],k)
  knnTestPred = knn(ziptrain23,ziptest23,ziptrain23[,1],k)
  cat(paste("\n k =",k,"\n"))
  printErrors(ziptrain23[,1], ziptest23[,1], knnTrainPred, knnTestPred)
}

#Problem 2
cat("Problem 2")
#2.1 - LDA
ldaCols = c(2:16,18:257)
twoLDA = getLDA(ziptrain23,ziptest23, ldaCols)
#2.2 - QDA
qdaCols = c(2:257)
twoQDA = getQDA(ziptrain23, ziptest23, qdaCols)
#2.3 - Naive Bayes
nbCols = c(1:257)
twoNB = getNB(ziptrain23, ziptest23, nbCols)
#2.4 - Logistic Regression
twoLR = getLR(ziptrain23, ziptest23)
#2.5 - SVM
twoSVM = getSVM(ziptrain23, ziptest23)
#2.6 - CART
twoCART = getCART(ziptrain23, ziptest23)


#Problem 3
cat("Problem 3")
#3.1 - LDA
thrLDA = getLDA(ziptrain235,ziptest235, ldaCols)
#3.2 - QDA
thrQDA = getQDA(ziptrain235,ziptest235, qdaCols)
#3.3 - Naive Bayes
thrNB = getNB(ziptrain235, ziptest235, nbCols)
#3.4 - Logistic Regression
thrLR = getLR(ziptrain235, ziptest235)
#3.5 - SVM
thrSVM = getSVM(ziptrain235, ziptest235)
#3.6 - CART
thrCART = getCART(ziptrain235, ziptest235)


