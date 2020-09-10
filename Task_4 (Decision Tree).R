pacman::p_load(dplyr, rpart, rpart.plot, caret) #loading required libraries in R

iris=read.csv('Iris.csv', header=T) #reading data from csv file into R
head(iris)  #viewing first few data entries
str(iris) #internal structure of data set
dim(iris)  #no. of rows and columns
iris=select(iris,-Id)  #eliminating the Id column
iris$Species=factor(iris$Species)  #converting the datatype to factor for Classification Tree
dim(iris)
str(iris)


#dividing the dataset into training and test dataset
index=createDataPartition(iris$Species, p=0.8, list=F, times = 1)

Trgset=iris[index,]
testset=iris[-index,]
dim(testset)

#creating and training the decision tree model based on training dataset
dt2<-rpart(Species ~ . , data=Trgset, method='class')
dt2
rpart.plot(dt2, type=2, extra=104, nn=T)


#pruning the tree
#on the basis of the complexity factor that gives the least error
printcp(dt2)  
#least error 0.05 at CP of 0.01
dt2<-prune(dt2, cp=0.01)
dt2
rpart.plot(dt2, type=2, nn=T)

#predicting the testdata based on the trained model
predictdata=predict(dt2, testset, type='class') 
predictdata


#checking accuracy by making confusion matrix
caret::confusionMatrix(testset$Species, predictdata)
# 93% accuracy

#creating new dataset to feed into the classifier so that it can predict the right class accordingly

newdata=data.frame(SepalLengthCm=c(5.6,4,6.1,3), SepalWidthCm=c(2.8,3.6,4,2.1), PetalLengthCm=c(5.6,6,2.7,1.2), PetalWidthCm=c(1.2, 2, 2.4, 1.9))

newdata

#predicting the Species class for newdata based on the model
PredictClass=predict(dt2, newdata, type='class')
PredictClass





