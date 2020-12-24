getwd()
setwd(choose.dir())

df <- read.csv("bank-loan.csv",header = TRUE)
head(df)
dim(df)
summary(df)

#Checking Missing Values
apply(is.na(df),2,sum)

#Dropping missing value rows
df = na.omit(df)
dim(df)

#outlier analysis
summary(df)

boxplot(df$age, col="blue", main="Age")

boxplot(df$employ, col="blue", main="Employ")

boxplot(df$address, col="blue", main="Address")

boxplot(df$income, col="blue", main="Income")

boxplot(df$debtinc, col="blue", main="Debt to Income Ratio")

boxplot(df$creddebt, col="blue", main="Cred to Debt Ratio")

boxplot(df$othdebt, col="blue", main="Other Debt")

#Dealing with outlier using winsorization method

bench1=quantile(df$age, 0.95)
df$age[df$age>bench1] = bench1

bench2=quantile(df$ed, 0.95)
df$ed[df$ed>bench2] = bench2

bench3=quantile(df$employ, 0.95)
df$employ[df$employ>bench3] = bench3


bench4=quantile(df$address, 0.95)
df$address[df$address>bench4] = bench4

bench5=quantile(df$income, 0.95)
df$income[df$income>bench5] = bench5

bench6=quantile(df$debtinc, 0.95)
df$debtinc[df$debtinc>bench6] = bench6

bench7=quantile(df$creddebt, 0.95)
df$creddebt[df$creddebt>bench7] = bench7

bench8=quantile(df$othdebt, 0.95)
df$othdebt[df$othdebt>bench8] = bench8

bench9=quantile(df$default, 0.95)
df$default[df$default>bench9] = bench9


summary(df)
dim(df)

# checking correlation
library(corrplot)
cor=cor(df)
corrplot(cor,method = "square")
cor

# T test
ttest=data.frame()

ttest=lapply(1:8,function(i){
  t.test(df[df$default == 1,i],df[df$default == 0,i], var.equal = FALSE)
})
ttest

#Visualization

#Default
hist(df$default)
table(df$default)
sort(table(df$default), decreasing = TRUE)


# Load ggplot2
library(ggplot2)

#Age
ggplot(df, aes(x=as.factor(default), y=age)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("default")

#Ed
ggplot(df, aes(x=as.factor(default), y=ed)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("default")

#Employ
ggplot(df, aes(x=as.factor(default), y=employ)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("default")

#Address
ggplot(df, aes(x=as.factor(default), y=address)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("default")

#income
ggplot(df, aes(x=as.factor(default), y=income)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("default")

#debtinc
ggplot(df, aes(x=as.factor(default), y=debtinc)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("default")

#Creddebt
ggplot(df, aes(x=as.factor(default), y=creddebt)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("default")

#Othdebt
ggplot(df, aes(x=as.factor(default), y=othdebt)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("default")

#There are 850 observations and 9 features in the data set
#Removed missing values
#Now data has 700 observations and 9 features
#Out of 850 customers data, 700 are existing customers and 150 are new customers
#The data is highly imbalanced
#From  T test, found out that the correlation between the variables is within the acceptable limits



#splitting data to train and test
df$default <- factor(df$default)
set.seed(1029)
ind= sample(2,nrow(df),replace=TRUE,prob=c(0.7,0.3))
train= df[ind==1,]
test= df[ind==2,]

# Data for Developing Predictive Model
table(train$default)
prop.table(table(train$default))
summary(train)
dim(train)


#SMOTE
#install.packages('smotefamily')
library(smotefamily)
set.seed(1029)
balanced_data=SMOTE(train[,-9], train[,9], K = 5,dup_size = 2)

balanced_train= as.data.frame(balanced_data$data)
names(balanced_train)[9]="default"
balanced_train$default=as.factor(balanced_train$default)
#View(balanced_train)
table(balanced_train$default)
table(train$default)



#Logistic Regression

#install.packages('caret')  
library(caret)
lg <- glm (default ~., data=balanced_train, family = binomial)
summary(lg)

#VIF
car::vif(lg)

# Predict the Values
predict <- predict(lg, test,type = "response")


# Create Confusion Matrix
predict <- predict(lg, test,type = "response")
prdBln <- ifelse(predict > 0.5, 1, 0)
cnfmtrx <- table(prd=prdBln, act=test$default)
confusionMatrix(cnfmtrx,positive ="1")


#Accuracy
acc_lg <- mean(test$default == prdBln)
precision_lg= posPredValue(as.factor(prdBln), test$default, positive ="1")
recall_lg= sensitivity(as.factor(prdBln), test$default, positive ="1")
F1_lg= (2*precision_lg * recall_lg) / (precision_lg + recall_lg)

print(c(Accuracy=acc_lg,Precision= precision_lg, Recall =recall_lg, F1_score=F1_lg ))



#LG Model Performance

#PR Curve - with cut off 0.5
pred <- prediction(predict, test$default)
pr <- performance(pred,measure="prec", x.measure="rec")
plot(pr, colorize = TRUE, main= "PR Curve")



#Roc Curve - with cut off 0.5
pred <- prediction(predict, test$default)
roc <- performance(pred,'tpr','fpr')
plot(roc, colorize = TRUE, main= "ROC Curve")
abline(a=0,b=1)

#Auc - with cut off 0.5 
auc= performance(pred,"auc")
auc= unlist(slot(auc, "y.values"))
auc= round(auc,4)
legend(.6,0.4,auc,title="AUC")


#Find the optimum cutoff value
library(ROCR)
pred <- prediction(predict, test$default)
eval <- performance(pred,"acc")
plot(eval)
abline(h=0.82, v=0.48)

#identify best values
max = which.max(slot(eval,"y.values")[[1]])
acc = slot(eval, "y.values")[[1]][max]
cut = slot(eval, "x.values")[[1]][max]
print(c(Accuracy=acc,Cutoff= cut))





# So let's create model with cutoff 0.4795718

lg <- glm (default ~., data=balanced_train, family = binomial)


# Create Confusion Matrix with cut off 0.4795718
predict <- predict(lg, test,type = "response")
prdBln <- ifelse(predict > 0.4795718, 1, 0)
cnfmtrx <- table(prd=prdBln, act=test$default)
confusionMatrix(cnfmtrx,positive ="1")

#Accuracy with cut off 0.4795718
acc_lg <- mean(test$default == prdBln)
precision_lg= posPredValue(as.factor(prdBln), test$default, positive ="1")
recall_lg= sensitivity(as.factor(prdBln), test$default, positive ="1")
F1_lg= (2*precision_lg * recall_lg) / (precision_lg + recall_lg)

print(c(Accuracy=acc_lg,Precision= precision_lg, Recall =recall_lg, F1_score=F1_lg ))






#random forest
library(randomForest)
set.seed(1029)
rf = randomForest(default ~., data=balanced_train)
print(rf)

predict_rf <- predict(rf, test)
confusionMatrix(data=predict_rf,  
                reference=test$default, positive ="1" )


acc_rf <- mean(test$default == predict_rf)
precision_rf= posPredValue(predict_rf, test$default, positive ="1")
recall_rf= sensitivity(predict_rf, test$default, positive ="1")
F1_rf= (2*precision_rf * recall_rf) / (precision_rf + recall_rf)

print(c(Accuracy=acc_rf,Precision= precision_rf, Recall =recall_rf, F1_score=F1_rf ))


# Error rate of Random Forest
plot(rf)


# Tune mtry
set.seed(1029)
t <- tuneRF(balanced_train[,-9], balanced_train[,9],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)


rf = randomForest(default ~., data=balanced_train,mtry=4)
print(rf)

predict_rf <- predict(rf, test)
confusionMatrix(data=predict_rf,  
                reference=test$default, positive ="1" )

acc_rf <- mean(test$default == predict_rf)
precision_rf= posPredValue(predict_rf, test$default, positive ="1")
recall_rf= sensitivity(predict_rf, test$default, positive ="1")
F1_rf= (2*precision_rf * recall_rf) / (precision_rf + recall_rf)

print(c(Accuracy=acc_rf,Precision= precision_rf, Recall =recall_rf, F1_score=F1_rf ))


# Even after tuning we didn't get the maximum accuracy.
# Checking the models using train function.

#Train function
library(caret)

# define training control
train_control <- trainControl(method = "cv", number = 5)

#Random Forest
library(randomForest)
rf1 <- train(default ~ .,
            data = balanced_train,
            trControl = train_control,
            method = "rf",
            importance=TRUE)
rf1

#Decision Tree
balanced_train.cols = balanced_train[,2:ncol(balanced_train)]
tree <- train(default ~ .,
              data = balanced_train,
              trControl = train_control,
              method = "rpart",
              tuneLength=30)

tree

#SVM
svm <- train(default ~ .,
             data = balanced_train,
             trControl = train_control,
             method = "svmLinear")
svm

#Logistic Regression
library(caret)
lg1 <- train(default ~ .,
            data = balanced_train,
            trControl = train_control,
            method = "glm",
            family=binomial())
lg1

#We have higher accuracy for Random forest, so let's build the model using it.


plot(rf1)
varImp(rf)

# Predict and Create Confusion Matrix

rf1_predict <- predict(rf1,test)
confusionMatrix(data=rf1_predict,  
                reference=test$default, positive ="1" )

acc_rf1 <- mean(test$default == rf1_predict)
precision_rf1= posPredValue(rf1_predict, test$default, positive ="1")
recall_rf1= sensitivity(rf1_predict, test$default, positive ="1")
F1_rf1= (2*precision_rf1 * recall_rf1) / (precision_rf1 + recall_rf1)


print(c(Accuracy=acc_rf1,Precision= precision_rf1, Recall =recall_rf1, F1_score=F1_rf1 ))


#PR Curve - RF
rf1_predict_pb <- predict(rf1,test,type="prob")[-1]
pred <- prediction(rf1_predict_pb, test$default)
pr <- performance(pred,measure="prec", x.measure="rec")
plot(pr, colorize = TRUE, main= "PR Curve")


#Roc Curve - RF
rf1_predict_pb <- predict(rf1,test,type="prob")[-1]
pred_rf1 <- prediction(rf1_predict_pb, test$default)
roc <- performance(pred_rf1,'tpr','fpr')
plot(roc, colorize = TRUE, main= "ROC Curve")
abline(a=0,b=1)

#Auc - RF
auc= performance(pred_rf1,"auc")@y.values[[1]]
auc= round(auc,4)
legend(.6,0.4,auc,title="AUC")

#checking the precision, recall and F1 score, it's safe to build model with the Logistic Regression (lg)


# We have high accuracy with LG cutoff 0.4795718.


#create function
default_pred <- function(age,ed,employ,address,income,debtinc,creddebt,othdebt) {
  newdata <- data.frame(age = age,
                        ed = ed,
                        employ = employ,
                        address = address,
                        income = income,
                        debtinc = debtinc,
                        creddebt = creddebt,
                        othdebt = othdebt
  )
  
  predict1 = predict(lg, newdata,type = "response")
  predict1 
  prd1 =ifelse(predict1 > 0.4795718, print("Default"),print("No Default"))
}

default_pred(24,1,3,4,19,23.8,1.3,3.2)


lg <- glm (default ~., data=balanced_train, family = binomial)

saveRDS(lg,"model.rds")




