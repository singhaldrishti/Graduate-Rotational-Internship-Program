#TASK 2: To explore Supervised Machine Learning
#predicting the percentage of marks that a student is expected to score 
#based upon the number of hours they studied.
#This is a simple linear regression task as it involves just two variables.



StudentStudy <- read.csv("http://bit.ly/w-data") #loading data into R
head(StudentStudy) #viewing first few data entries of the data set
  
dim(StudentStudy)  # displaying the no. of rows and columns
str(StudentStudy) #Displaying the internal structure of the data set
StudentStudy$Scores <- as.numeric(StudentStudy$Scores) #converting scores into a numeric datatype

 
#check linearity of the data set first before plotting it
plot(StudentStudy$Hours, StudentStudy$Scores)

#creating training and test data
trainingRowIndex<-sample(1:nrow(StudentStudy),  0.8*nrow(StudentStudy)) #80% data in training set
trainingData <- StudentStudy[trainingRowIndex, ]
trainingData
testData<-StudentStudy[-trainingRowIndex, ]
testData

#Building a model for StudentStudy data set
#Linear model therefore lm() function used

lmodel<- lm(Scores~Hours, data=trainingData)
lmodel

#Checking summary of the model
summary(lmodel)

#Plotting regression line on graph to show the relationship
abline(lm(StudentStudy$Scores ~ StudentStudy$Hours), col="red")

#predicting the value of Scores based on training dataset
Predicted_trainingScore = predict (lmodel, trainingData )
Predicted_trainingScore
rmse(trainingData$Scores, Predicted_trainingScore) #calculating root mean square error

#predicting the value of Scores based on test dataset
Predicted_testScore = predict (lmodel, testData )
Predicted_testScore
rmse(testData$Scores, Predicted_testScore)

#predicting the value of Scores based on a new data of hours
NewData = data.frame(Hours=c(10, 9.25, 6, 7.15))
Predicted_score = predict (lmodel, NewData)
NewData
Predicted_score



