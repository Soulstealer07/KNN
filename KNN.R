library(ISLR)
library(class)
data()
Caravan<-as.data.frame(Caravan)
attach(Caravan)
summary(Purchase)
#Percentage Purchased
p<-348/(5474+348)
p
#Standardize the data
standardized.x<-scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.x[,1])
var(standardized.x[,2])
#Split the observations into a test set containing the first 1000 observations and a training set

test <- 1:1000
train.X <- standardized.x[-test,]
test.X<-standardized.x[test,]
train.Y <- Purchase[-test]
test.Y <-Purchase[test]

set.seed(1)
knn.pred<-knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
successrate <- 9/(68+9)
successrate

knn.pred<-knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
successrate<- 5/(21+5)
successrate

knn.pred<-knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
successrate <- 4/(11+4)
successrate


#Logistic Regression

glm.fits <- glm(Purchase ~., data = Caravan, family=binomial, subset = -test)
glm.probs <- predict(glm.fits,Caravan[test,],type="response")
glm.pred<-rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
successrate <- 0/(7+0)
successrate
glm.pred<-rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
successrate <- 11/(22+11)
successrate
