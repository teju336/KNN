Cancerdataset <- read.csv("~/Downloads/data.csv", stringsAsFactors = FALSE)
str(Cancerdataset)
Cancerdataset <- Cancerdataset[-1]
table(Cancerdataset$diagnosis)
Cancerdataset$diagnosis <- factor(Cancerdataset$diagnosis, levels = c("B", "M"),
                                  labels = c("Benign", "Malignant"))
round(prop.table(table(Cancerdataset$diagnosis)) * 100, digits = 1)
summary(Cancerdataset[c("radius_mean", "area_mean", "smoothness_mean")])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) -min(x)))
}
wbcd_n <- as.data.frame(lapply(Cancerdataset[2:31], normalize))
summary(wbcd_n$area_mean)
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- Cancerdataset[1:469, 1]
wbcd_test_labels <- Cancerdataset[470:569, 1]
library("class")
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
install.packages("gmodels")
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
wbcdz <- as.data.frame(scale(Cancerdataset[-1]))
summary(wbcdz$area_mean)
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# Then classify the test instances using the knn() function. 
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# We'll then compare the predicted labels to the actual labels using CrossTable()
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
library(corrplot)

library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
glass <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data", 
                  col.names=c("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type"))
glass$Type<-factor(glass$Type)
glass1<-glass[,-10]
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
glass2<-normalize(glass1)
head(glass2)
index1<-sample(nrow(glass1),0.75*nrow(glass2))


trainglass<-glass2[index1,]
testglass<-glass2[-index1,]
ytrain=glass$Type[index1]
ytest=glass$Type[-index1]
library(class)
knnmodelglass=knn(trainglass,testglass,k=sqrt(nrow(trainglass)),cl=ytrain)
knnmodelglass
library(caret)
library(lattice)
library(ggplot2)
confusionMatrix(ytest,knnmodelglass)
