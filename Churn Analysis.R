library(tidyverse)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

#Reading the dataset
data<-read.csv("Telecom.csv")
str(data)
summary(data)

#Checking the number of missing values in dataset
sapply(data, function(x) sum(is.na(x)))

#Imputing the missing values using mean & categorical data using mode using a function
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

for (var in 1:ncol(data)) {
  if (class(data[,var])=="numeric") {
    data[is.na(data[,var]),var] <- mean(data[,var], na.rm = TRUE)
  } else if (class(data[,var]) %in% c("character", "factor")) {
    data[is.na(data[,var]),var] <- Mode(data[,var], na.rm = TRUE)
  }
}

#Data Wrangling
#Changing No Internet Service to No 
cols_recode1 <- c(10:15)
for(i in 1:ncol(data[,cols_recode1])) {
  data[,cols_recode1][,i] <- as.factor(mapvalues
                                        (data[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#Changing No Phone service to No
data$MultipleLines <- as.factor(mapvalues(data$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))
#Creating range for tenure
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

data$tenure_group <- sapply(data$tenure,group_tenure)
data$tenure_group <- as.factor(data$tenure_group)


#Exploratory Analysis
boxplot(data$tenure~data$Churn, main="Boxplot of Tenure by Churn",col=(c("red","blue")),
        xlab="Churn",ylab="Tenure")
boxplot(data$MonthlyCharges~data$Churn, main="Boxplot of Monthly Charges by Churn",col=(c("red","blue")),
        xlab="Churn",ylab="Monthly Charges")
boxplot(data$TotalCharges~data$Churn, main="Boxplot of Total Charges by Churn",col=(c("red","blue")),
        xlab="Churn",ylab="Total Charges")

#Removal of unrequired colums
data$customerID <- NULL
data$tenure <- NULL
data$TotalCharges<-NULL
#Logistic Regression

#Splitting of dataset into test and train
train<- createDataPartition(data$Churn,p=0.8,list=FALSE)
set.seed(2017)
training<- data[train,]
testing<- data[-train,]

dim(training); dim(testing)

#Logistic Model
model<-glm(Churn~SeniorCitizen+InternetService+OnlineSecurity+
             TechSupport+Contract+PaperlessBilling+tenure_group,
           family=binomial(link="logit"),data=training)
print(summary(model))

anova(model, test="Chisq")

fitted.results<-predict(model,newdata = testing,type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError<-mean(fitted.results!=testing$Churn)
print(paste('Logistic Regression Acurracy',1-misClasificError))

#Odds Ratio
exp(cbind(OR=coef(model), confint(model)))


#Decision Tree
tree <- ctree(Churn~SeniorCitizen+Contract+tenure_group+PaperlessBilling+
                InternetService+OnlineSecurity+TechSupport, training)
plot(tree, type='simple')

p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = predict(tree,testing), Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))


#RandomForest
rfModel <- randomForest(Churn ~SeniorCitizen+Contract+tenure_group+PaperlessBilling+
                          InternetService+OnlineSecurity+TechSupport, data = training)
print(rfModel)

pred_rf <- predict(rfModel, testing)
caret::confusionMatrix(pred_rf, testing$Churn)
