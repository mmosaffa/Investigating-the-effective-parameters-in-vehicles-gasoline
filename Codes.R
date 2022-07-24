set.seed(1)
x = rnorm(100)
eps = rnorm(100,0,0.25)
y = -1 + 0.5*x + eps
lmmodel = lm(y~x)
summary(lmmodel)
plot(x, y, pch=4, col="red", xlab="x", ylab="y")
abline(lmmodel,lwd=3,col="3")
legend(-2, 0, legend=c("Fitted Values"),col=c("green"), lty=1:2, cex=0.8)
t= x^2
lmmodel2 = lm(y~x+t)
summary(lmmodel2)

set.seed(1)
x = rnorm(100)
eps = rnorm(100,0,0.05)
y = -1 + 0.5*x + eps
lmmodel = lm(y~x)
summary(lmmodel)
plot(x, y, pch=4, col="red", xlab="x", ylab="y")
abline(lmmodel,lwd=3,col="3")
legend(-2, 0, legend=c("Fitted Values"),col=c("green"), lty=1:2, cex=0.8)
t= x^2
lmmodel2 = lm(y~x+t)
summary(lmmodel2)


dim(Auto)
d = Auto 
med = median(d$mpg)
d$mpg[d$mpg<=med] = 0
d$mpg[d$mpg!=0] = 1
df = Auto
df = data.frame(Auto,d$mpg)
names(df)[10]<-paste("mpg01")

par(mfrow=c(2,4)) 
plot(df$mpg,df$cylinders,xlab = "mpg",ylab = "cylinders")
plot(df$mpg,df$displacement,xlab = "mpg",ylab = "displacement")
plot(df$mpg,df$horsepower,xlab = "mpg",ylab = "horsepower")
plot(df$mpg,df$weight,xlab = "mpg",ylab = "weight")
plot(df$mpg,df$acceleration,xlab = "mpg",ylab = "acceleration")
plot(df$mpg,df$year,xlab = "mpg",ylab = "year")
plot(df$mpg,df$origin,xlab = "mpg",ylab = "origin")

par(mfrow=c(2,4)) 
boxplot(df$mpg,df$cylinders,xlab = "mpg",ylab = "cylinders")
boxplot(df$mpg,df$displacement,xlab = "mpg",ylab = "displacement")
boxplot(df$mpg,df$horsepower,xlab = "mpg",ylab = "horsepower")
boxplot(df$mpg,df$weight,xlab = "mpg",ylab = "weight")
boxplot(df$mpg,df$acceleration,xlab = "mpg",ylab = "acceleration")
boxplot(df$mpg,df$year,xlab = "mpg",ylab = "year")
boxplot(df$mpg,df$origin,xlab = "mpg",ylab = "origin")

mglm = lm(mpg01~cylinders+displacement+horsepower+weight+acceleration+year+origin,data = df)
mstep=step(mglm,direction = "backward")
set.seed(1) 
sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]
dim(train)
dim(test)

accuracy.test = data.frame()
library(MASS)
ldamodel =lda(mpg01~cylinders+horsepower+weight+origin+year,data=train)
predmodel.train.lda = predict(ldamodel, data=train)
predmodel.test.lda = predict(ldamodel, newdata= test, type="response")
table(Predicted.train=predmodel.train.lda$class, Survived=train$mpg01)
test.accuracy.LDA = table(Predicted.test=predmodel.test.lda$class, Survived=test$mpg01)
test.accuracy.LDA
((test.accuracy.LDA[1,1]+test.accuracy.LDA[2,2])/dim(test)[1])*100
accuracy.test[1,1] = "LDA Test Accuracy"
accuracy.test[1,2] = ((test.accuracy.LDA[1,1]+test.accuracy.LDA[2,2])/dim(test)[1])*100
accuracy.test[1,3] = ((test.accuracy.LDA[1,1])/(test.accuracy.LDA[1,1]+test.accuracy.LDA[1,2]))*100
accuracy.test[1,4] = ((test.accuracy.LDA[2,2])/(test.accuracy.LDA[2,1]+test.accuracy.LDA[2,2]))*100

library(MASS)
qdamodel =qda(mpg01~cylinders+horsepower+weight+origin+year,data=train)
predmodel.train.qda = predict(qdamodel, data=train)
predmodel.test.qda = predict(qdamodel, newdata= test, type="response")
table(Predicted=predmodel.train.qda$class, Survived=train$mpg01)
test.accuracy.QDA = table(Predicted.test=predmodel.test.qda$class, Survived=test$mpg01)
test.accuracy.QDA
((test.accuracy.QDA[1,1]+test.accuracy.QDA[2,2])/dim(test)[1])*100
accuracy.test[2,1] = "QDA Test Accuracy"
accuracy.test[2,2] = ((test.accuracy.QDA[1,1]+test.accuracy.QDA[2,2])/dim(test)[1])*100
accuracy.test[2,3] = ((test.accuracy.QDA[1,1])/(test.accuracy.QDA[1,1]+test.accuracy.QDA[1,2]))*100
accuracy.test[2,4] = ((test.accuracy.QDA[2,2])/(test.accuracy.QDA[2,1]+test.accuracy.QDA[2,2]))*100

library(MASS)
logmodel =glm(mpg01~cylinders+horsepower+weight+origin+year,family = binomial,data=train)
predmodel.train.log = predict(logmodel, data=train)
predmodel.test.log = predict(logmodel, newdata= test, type="response")
predmodel.train.log = predict(logmodel, type="response")
predmodel.train.log = ifelse(predmodel.train.log > 0.5, 1, 0)
table(Predicted=predmodel.train.log, Survived=train$mpg01)
predmodel.test.log = predict(logmodel, newdata= test, type="response")
predmodel.test.log = ifelse(predmodel.test.log > 0.5, 1, 0)
test.accuracy.LOG = table(Predicted.test=predmodel.test.log, Survived=test$mpg01)
test.accuracy.LOG
((test.accuracy.LOG[1,1]+test.accuracy.LOG[2,2])/dim(test)[1])*100
accuracy.test[3,1] = "LOG Test Accuracy"
accuracy.test[3,2] = ((test.accuracy.LOG[1,1]+test.accuracy.LOG[2,2])/dim(test)[1])*100
accuracy.test[3,3] = ((test.accuracy.LOG[1,1])/(test.accuracy.LOG[1,1]+test.accuracy.LOG[1,2]))*100
accuracy.test[3,4] = ((test.accuracy.LOG[2,2])/(test.accuracy.LOG[2,1]+test.accuracy.LOG[2,2]))*100


library(class)

trainknn = train[,c(2,4,5,7,8)]
testknn = test[,c(2,4,5,7,8)]
nor = function(x) { (x -min(x))/(max(x)-min(x))   }
trainknn = as.data.frame(lapply(trainknn, nor))
testknn = as.data.frame(lapply(testknn, nor))
iris_target_category = train$mpg01
iris_test_category = test$mpg01
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracymatrix = data.frame()
for (j in 1:100){
  for (i in 1:50) {
    knnmodel = knn(trainknn,testknn,cl=iris_target_category,k=i)
    tab = table(knnmodel,iris_test_category)
    accuracymatrix[i,1] = i
    accuracymatrix[i,j+1] = accuracy(tab)
  }
}
dim(accuracymatrix)
accuracymatrixknn = data.frame()
for (i in 1:100){
  accuracymatrixknn[i,1]=which.max(accuracymatrix[,i+1])
}
hist(accuracymatrixknn[,1])
plot(accuracymatrix[,1],accuracymatrix[,2])
knnmodel = knn(trainknn,testknn,cl=iris_target_category,k=1)
test.accuracy.KNN = table(knnmodel,iris_test_category)
accuracy(test.accuracy.KNN)
table(Predicted.test=knnmodel.test.qda$class, Survived=test$mpg01)
accuracy.test[4,1] = "KNN Test Accuracy"
accuracy.test[4,2] = accuracy(test.accuracy.KNN)
accuracy.test[4,3] = ((test.accuracy.KNN[1,1])/(test.accuracy.KNN[1,1]+test.accuracy.KNN[1,2]))*100
accuracy.test[4,4] = ((test.accuracy.KNN[2,2])/(test.accuracy.KNN[2,1]+test.accuracy.KNN[2,2]))*100

