data <- read.csv(file.choose(),header = T)
head(data)
data$Gender=ifelse(data$Gender=="M",1,0)
data$result=ifelse(data$result=="Y",1,0)
head(data)

final_data <- data[-c(1,2)]
head(final_data)
train_data <- createDataPartition(final_data$result,p=0.7,list=F)
train <- data[train_data,]
test <- data[-train_data,]
dim(train)
dim(test)
train2=train
train2$result<-as.factor(train2$result)
train2$Gender<-as.factor(train2$Gender)
str(train2)

#---------------------------rf-Model---------------------------------
set.seed(10000000)

library(randomForest)
rfm<-randomForest(train2$result~.,data=train2[-c(1,2)] ,mtry=6,ntree=500)
rfm$importance
importance(rfm)
varImpPlot(rfm)


#------------------------ Prediction -----------------------------
set.seed(10000000)
pred<- predict(rfm,newdata = test ,type = "prob",predict.all = F)
pred <- data.frame(pred)
dim(test)
View(pred)
probability <- pred[,2]
train_pred <- data.frame(test,pred)
head(train_pred)

data_score <- ifelse(pred[,2] < 0.5,"Not_affected","Affected")
View(data_score)
final_table<- data.frame(data_score,probability)
View(final_table)
