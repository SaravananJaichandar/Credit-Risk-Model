
dat=read.csv("file:///E:/chrome downloads/german_credit_data (1).csv")
head(dat)            #watch first 6 row of the data
dat = dat[,-1] 
#know more about the data
#summary of the data
summary(dat)

#check the rate of missing values
miss = function(x){sum(is.na(x))/length(x)*100}
apply(dat,2,miss)

#pay attention to the unbalanced y-value
plot(dat[,10])

#devide them by y-value
good = dat[which(dat[,10]=="good"),]
bad = dat[which(dat[,10]=="bad"),]

####data exploring####
# 2.1 age & housing
#age
#win.graph(width = 8,height = 2.5)
par(mfrow=c(1,3))
hist(good[,1],xlab="age",ylab="",main="",ylim=c(0,160))
hist(bad[,1],xlab="age",ylab="",main="",ylim=c(0,160))
hist(dat[,1],xlab="age",ylab="",main="")

#housing
plot(good[,4],xlab="housing",ylab="",main="",ylim=c(0,500))
plot(bad[,4],xlab="housing",ylab="",main="",ylim=c(0,500))
plot(dat[,4],xlab="housing",ylab="",main="")

#2.2 credit amount&duration
plot(y=good[,7],x=good[,8],pch=1,
     xlab="duration",ylab="credit amount",main="")
points(y=bad[,7],x=bad[,8],pch=3,
       xlab="duration",ylab="credit amount",main="")

####data processing####
#handle categorical variables
library(nnet)
sex = class.ind(dat[,2]) 
housing = class.ind(dat[,4]) 
saving = class.ind(dat[,5])
colnames(saving) = c("s_little","s_moderate","s_quite rich","s_rich")
checking = class.ind(dat[,6]) 
colnames(checking) = c("c_little","c_moderate","c_rich")
purpose = class.ind(dat[,9]) 
risk = class.ind(dat[,10])

#correlation plot
datn = dat[,c(-2,-4,-5,-6,-9,-10)]
newdat = cbind(datn,sex,housing,saving,checking,purpose,risk)
mycor = cor(newdat)

library(corrplot)
par(mfrow = c(1,1))
corrplot(mycor,tl.col="black",method="color",shade.lwd = 0.1,tl.cex=0.8,order="hclust")


#4.1 split data by group sampling
newdat = newdat[,-25]
newdat = as.matrix(newdat[order(newdat$good), ])
siz = c(300,700)
n = round(0.7*siz)

library(sampling)
tr = strata(newdat, stratanames='good', size=n, method="srswor")
train = newdat[tr$ID_unit,] 
test = newdat[-tr$ID_unit,] 
xtr = train[,1:24]
ytr = train[,25] 
xte = test[,1:24]
yte = test[,25]

#write.table(train, file ="train data.csv", sep =",")
#write.table(test, file ="test data.csv", sep =",")


#4.2 logistic regression
glm = glm(good~.,data=as.data.frame(train),family=binomial("logit"))

logit.step = step(glm, direction = c("both"))

glm1 = glm(good~s_little+c_little+c_moderate+Duration+female+own,
           data=as.data.frame(train),family=binomial("logit"))

#computing classification rate in training set
pyltr = glm1$fitted.values
yltr = rep(0,length(pyltr))
for(i in 1:length(pyltr)){
  if(pyltr[i]<=0.5){yltr[i]=0}
  else{yltr[i]=1}
}
train_result1 = table(yltr,ytr)
rate1 = (train_result1[1,1]+train_result1[2,2])/sum(train_result1)

#computing classification rate in testing set
y_pred = predict(glm1, as.data.frame(xte))
y_tran = rep(0,length(y_pred))
for(i in 1:length(y_pred)){
  if(y_pred[i]<=0){y_tran[i]=0}
  else{y_tran[i]=1}
}
test_result1 = table(y_tran,yte)
rate2 = (test_result1[1,1]+test_result1[2,2])/sum(test_result1)

library(xgboost)
xgb = xgboost(data = xtr, 
              label = ytr, 
              eta = 0.1,
              max_depth = 5, 
              gamma = 0,
              objective = "binary:logistic",
              nrounds = 15, 
              subsample = 0.8,
              colsample_bytree = 0.8,
              seed = 1,
              eval_metric = "error"
)

#computing classification rate in training set
y_pred = predict(xgb, xtr)
y_train = rep(0,length(y_pred))
for(i in 1:length(y_pred)){
  if(y_pred[i]<=0.5){y_train[i]=0}
  else{y_train[i]=1}
}
test_result3 = table(y_train,ytr)
rate3 = (test_result3[1,1]+test_result3[2,2])/sum(test_result3)

#computing classification rate in testing set
y_pred = predict(xgb, xte)
y_test = rep(0,length(y_pred))
for(i in 1:length(y_pred)){
  if(y_pred[i]<=0.5){y_test[i]=0}
  else{y_test[i]=1}
}
test_result4 = table(y_test,yte)
rate4 = (test_result4[1,1]+test_result4[2,2])/sum(test_result4)

paste("classification rate in training set via logit regression is", round(rate1,2))
paste("classification rate in testing set via logit regression is", round(rate2,2))
paste("classification rate in training set via xgboost is", round(rate3,2))
paste("classification rate in testing set via xgboost is", round(rate4,2))
