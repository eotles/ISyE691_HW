Sys.setenv(LANG = "en")

##Data 1: Fisher's Iris Dat, made up of 150 obs. 
#        belonging to 3 different classes

### Support Vector Machine
library(e1071)
#randomly split data into a training set with 75 obs and a test set with 75 obs
set.seed(123)
sub <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25));
fit3 <- svm(Species ~ ., data = iris,subset = sub);
table(predict(fit3, iris[-sub,], type="class"), iris[-sub, "Species"])

#        setosa versicolor virginica
#  setosa         25          0        0
#  versicolor      0         24        3
#  virginica       0          1        22

### Tree
library(rpart)
fit1 <- rpart(Species ~ ., data= iris, subset = sub);
table(predict(fit1, iris[-sub,], type="class"), iris[-sub, "Species"])

#        setosa versicolor virginica
#  setosa         25          0         0
#  versicolor      0         24         3
#  virginica       0          1        22


### Neural Network
library(nnet)
fit2 <- nnet(Species ~ ., data= iris, subset = sub,  size =2)
table(predict(fit2, iris[-sub,], type="class"), iris[-sub, "Species"])

#        setosa versicolor virginica
#  setosa         25          0        0
#  versicolor      0         23        3
#  virginica       0          2        22


#Data 2: Ripley's Data, two classes and each class is from a mixture of two normal dist.
#  training set (125 obs. from each class); test set (500 from each class)

library(MASS)
data(synth.tr) #training data
data(synth.te) # test data

synth.tr$yc <- as.factor(synth.tr$yc);
synth.te$yc <- as.factor(synth.te$yc);

#nnet 
# one default is the factor (decay) or lambda on a squared error penalty term, 
#               default is 0
# In general we ran the size (# d nodes in the hidden layer) from 10 to 20
#     and then CV to find the best size
#
library(nnet)
rip.fit1 <- nnet( yc ~ xs+ys, data = synth.tr, size =16, decay = 5e-4)
table(predict(rip.fit1, synth.tr[,-3], type="class"), synth.tr[,3])
table(predict(rip.fit1, synth.te[,-3], type="class"), synth.te[,3])

#      0   1
#  0 117  13
#  1   8 112
#
#      0   1
#  0 463  71
#  1  37 429
#
## Running again!!! It will give different answers, since neural nets chooses
##  initialization at random and there are multiple local optima. 
## We can pool the predictions from multiple run (say by majority vote)
## to get improved prediction

# Compared to the tree

library(rpart)
rip.fit2 <- rpart( yc ~ xs+ys, data = synth.tr)
table(predict(rip.fit2, synth.tr[,-3], type="class"), synth.tr[,3])
table(predict(rip.fit2, synth.te[,-3], type="class"), synth.te[,3])

#      0   1
#  0 113  20
#  1  12 105
#
#      0   1
#  0 469  72
#  1  31 428

### Compared to the Support Vector Machine
library(e1071)
#randomly split data into a training set with 75 obs and a test set with 75 obs
rip.fit3 <- svm(yc ~ xs+ys, data = synth.tr);
table(predict(rip.fit3, synth.tr[,-3], type="class"), synth.tr[,3])
table(predict(rip.fit3, synth.te[,-3], type="class"), synth.te[,3])

#   0   1
#0 106  13
#1  19 112

#    0   1
# 0 458  53
# 1  42 447

#CART
spam <- read.table(file= "http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data",
                   sep = ",")
dim(spam)
set.seed(123)
flag <- sort(sample(4601,1536, replace = FALSE))
spamtrain <- spam[!is.na(flag),]
spamtest <- spam[flag,]
dim(spamtest[spamtest$V58 ==1,])

library(rpart)

rpart.spam1 <- rpart(V58 ~ .,data=spamtrain, method="class", parms=list(split="gini"))

rpart.spam2 <- rpart(V58 ~ .,data=spamtrain, method="class", parms=list(split="information")) #use cross entropy

par(mfrow=c(1,1))

post(rpart.spam1,filename="")

post(rpart.spam2,filename="")

## Training & Test Errors for Tree
y1    <- spamtrain$V58;
y2    <- spamtest$V58;
y1hatc <- ifelse(predict(rpart.spam1,spamtrain)[,2] < 0.5, 0, 1)
sum(y1hatc != y1)/length(y1)
# 0.09737014
y2hatc <-  predict(rpart.spam1, spamtest[,-58],type="class")
sum(y2hatc != y2)/length(y2)
# 0.09114583 (test error)

# We can use a cp plot to prune back the tree a bit with 
# little changes in classification error.
#par(mfrow=c(2,1))

plotcp(rpart.spam1) #show cost-complexity
rpart.pruned1 <- prune(rpart.spam1,cp=0.05)
y2hatc1 <-  predict(rpart.pruned1, spamtest[,-58],type="class")
sum(y2hatc1 != y2)/length(y2)
## 0.1510417

par(mfrow=c(1,2))

post(rpart.spam1,filename="")
post(rpart.pruned1,filename="")
