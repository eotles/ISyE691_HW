crimes <- read.table(file = "/Users/eotles/Downloads/crimes.csv", header= TRUE, sep = ",")
mod3a = lm(crimes[,1] ~ .,data=crimes[,2:14])
summary(mod3a)

for(i in 2:6){
    print("NEW ITERATION")
    print(i)
    op = which(crimes.models.size == i)
    crimes.models.rss[op]
    flag = op[which.min(crimes.models.rss[op])]
    print(crimes.models[flag,])
    crimes.models.rss[flag]
}

mod3b2 = lm(crimes[,1] ~ Ex0 + X, data=crimes[,2:14])
summary(mod3b2)
sum(resid(mod3b2)^2)

mod3b3 = lm(crimes[,1] ~ Ed + Ex0 + X, data=crimes[,2:14])
summary(mod3b3)
sum(resid(mod3b3)^2)

mod3b4 = lm(crimes[,1] ~ Age + Ed + Ex0 + X, data=crimes[,2:14])
summary(mod3b4)
sum(resid(mod3b4)^2)

mod3b5 = lm(crimes[,1] ~ Age + Ed + Ex0 + U2 + X, data=crimes[,2:14])
summary(mod3b5)
sum(resid(mod3b5)^2)

mod3b6 = lm(crimes[,1] ~ Age + Ed + Ex0 + Ex1 + U2 + X, data=crimes[,2:14])
summary(mod3b6)
sum(resid(mod3b6)^2)


####Ridge regression (MASS: lm.ridge, mda: gen.ridge)
### close to fig 3.7 (difference on x-axis)
library(MASS);
crimes.ridge <- lm.ridge( crimes[,1] ~ ., data = crimes[,2:14], lambda= seq(0,100,0.01));
plot(crimes.ridge) 
select(crimes.ridge)
# Using lambda = 2.19, we can get the best model:
crimes.ridge$coef[, which(crimes.ridge$lambda == 2.19)]
### compared it with OLS
crimes.ridge$coef[, which(crimes.ridge$lambda == 0)]
### Fitted value with ridge \lambda = 2.19 
lambda1 <- 2.19;
coef1 <- coef(crimes.ridge)[which(crimes.ridge$lambda == lambda1),]
#data.matrix(crimes[,2:14]) %*% coef1
#pred1 <- as.vector(crimes[,2:12] %*% coef1)
pred1 = vector()
for(i in 1:47){
  pred1[i] = (sum(crimes[i,2:14]*coef1[2:14])+coef1[1])
}
ytrue <- crimes[,1]; 
MSE1 <-  mean((pred1 - ytrue)^2); 
print(MSE1)
#366.8 

coef0 <- coef(crimes.ridge)[which(crimes.ridge$lambda == 0),]
#pred0 <- as.vector(as.matrix(cbind(1, training[,1:8])) %*% coef0)
pred0 = vector()
for(i in 1:47){
  pred0[i] = (sum(crimes[i,2:14]*coef0[2:14])+coef0[1])
}
ytrue <- crimes[,1]; 
MSE0 <-  mean((pred0 - ytrue)^2); 
print(MSE0)
(#337.8 for OLS)

  ## LASSO (Fig 3.9)
  
  #scale
  crimes[,2:14] = scale(crimes[,2:14],TRUE,TRUE)
  
  library(lars)
  crimes.lars <- lars( as.matrix(crimes[,2:14]), crimes[,1], type= "lasso", trace= TRUE); 
  plot(crimes.lars)
  coef.lars(crimes.lars, s=0.5, mode="lambda")
  
  ## select the path with the smallest Cp 
  Cp1  <- summary(crimes.lars)$Cp;
  index1 <- which.min(Cp1);
  
  ## Two ways to get beta values
  coef(crimes.lars)[index1,]
  crimes.lars$beta[index1,]
  lasso.lambda <- crimes.lars$lambda[index1]
  
  ## Fitted value
  ## training error for lasso
  fit1 <- predict(crimes.lars, as.matrix(crimes[,2:14]), s=lasso.lambda, type="fit", mode="lambda");
  yhat <- fit1$fit; 
  mean( (yhat - crimes[,1])^2);  
