library(ggplot2)
library(rpart)
library(rpart.plot)
library(gmodels)
library(e1071)
library(gridExtra)
library(randomForest)


veri<-read.csv("almanya analiz.csv",header=TRUE,sep=",",stringsAsFactors = TRUE)

#summary(veri)

temp = as.data.frame(scale(veri[,1:20]))
temp$Sonuc = veri$Sonuc
set.seed(114)

#summary(temp)
smp_size =  100
train_ind = sample(seq_len(nrow(temp)), size = smp_size,replace = TRUE)
train = temp[train_ind, ]
test = temp[-train_ind, ]


#Decision tree
model.rpart = rpart(Sonuc ~ . ,data =train)
preds.rpart = predict(model.rpart,newdata = test,type = "class")
#CrossTable(test$Sonuc,preds.rpart,chisq = F,prop.r = F,prop.c = F,prop.t = F,prop.chisq = F)
dtSonuc = ((7+9+11)/nrow(test))*100

#k-nearest neighbours
library(class)
cl = train$Sonuc
preds.knn = knn(train[,1:20],test[,1:20],cl,k=3)
#CrossTable(preds.knn,test$Sonuc,chisq = F,prop.r = F,prop.c = F,prop.t = F,prop.chisq = F)
knnSonuc = ((2+9+11)/nrow(test))*100


#Support Vector Machine(SVM)
model.svm = svm(Sonuc ~ . ,data =train)
preds.svm = predict(model.svm,newdata = test)
#CrossTable(preds.svm,test$Sonuc,chisq = F,prop.r = F,prop.c = F,prop.t = F,prop.chisq = F)
svmSonuc = ((3+13+12)/nrow(test))*100

#Random Forest
model.rf = randomForest(Sonuc ~ .,data = train)
preds.rf = predict(model.rf,newdata = test)
#CrossTable(preds.rf,test$Sonuc,chisq = F,prop.r = F,prop.c = F,prop.t = F,prop.chisq = F)
rfSonuc = ((3+10+13)/nrow(test))*100

#naiveBayes
model.nb = naiveBayes(Sonuc ~ .,data = train)
preds.nb = predict(model.nb,newdata = test)
CrossTable(test$Sonuc,preds.nb,chisq = F,prop.r = F,prop.c = F,prop.t = F,prop.chisq = F)
nbSonuc = ((4+4+11)/nrow(test))*100

#Accuracy comparison
models = data.frame(Technique = c("Decision Tree","kNN","SVM","Random Forest","naiveBayes"), Accuracy_Percentage = c(dtSonuc,knnSonuc,svmSonuc,rfSonuc,nbSonuc))
models



