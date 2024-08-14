install.packages("rpart")

sd = read.csv('/Users/rayan786/R_W2/seeds_dataset_class.csv', sep=",")
sd_rand = sd[sample(150,150),]

sdclass = sd[,1]
sdvalue = sd[,-1]

sdclassTrain = sd_rand[,1]
sdvalueTrain = sd_rand[,-1]

sdclassTest = sdclass[150:209]
sdvalueTest = sdvalue[150:209,]

library(rpart)
fit <- rpart(sdclassTrain~., method="class", data=sdvalueTrain)
plot(fit, uniform=TRUE, main="Decision Tree for seeds_dataset_class")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

treepred <-predict(fit, newdata= sdvalueTest, type = 'class')
n = length(sdclassTest) #the number of test cases
ncorrect = sum(treepred==sdclassTest) #the number of correctly predicted
accuracy=ncorrect/n

table_mat = table(sdclassTest, treepred)
print(table_mat)

library(rpart)
pfit1 = prune(fit, cp=0.1)
plot(pfit1, uniform=TRUE, main="Pruned Decision Tree at cp=0.1")
text(pfit1, use.n=TRUE, all=TRUE, cex=.8)
pruned1 <-predict(pfit1, sdvalueTest, type = 'class')
n1 = length(sdclassTest) #the number of test cases
ncorrect1 = sum(pruned1==sdclassTest) #the number of correctly predicted
accuracy1=ncorrect1/n1

library(rpart)
pfit2<- prune(fit, cp=0.2)
plot(pfit2, uniform=TRUE, main="Pruned Decision Tree at cp=0.2")
text(pfit2, use.n=TRUE, all=TRUE, cex=.8)
pruned2 <-predict(pfit2, sdvalueTest, type = 'class')
n2 = length(sdclassTest) #the number of test cases
ncorrect2 = sum(pruned2==sdclassTest) #the number of correctly predicted
accuracy2=ncorrect2/n2

library(class)
knn3pred = knn(sdvalueTrain, sdvalueTest, sdclassTrain, k=3)
nk = length(sdclassTest) #the number of test cases
ncorrectk = sum(knn3pred==sdclassTest) #the number of correctly predicted
accuracyk=ncorrectk/nk

#Scatter plot 2 selected variables of the data
plot(x=sd$Length, y=sd$Width, xlab="Length", ylab = "Width", main = "Length vs Width", col= treepred)
#plot(x=sdvalueTest$Length, y=sdvalueTest$Width, xlab="Length", ylab = "Width", main = "Length vs Width",col=sdvalueTest)

