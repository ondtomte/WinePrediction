wine <- read.csv(file="winequality-white.csv",sep=";")
wine <- cbind(scale(wine[,-12]),quality=wine[,12])
wine$good <- wine$quality>5
wine$quality<-  NULL
set.seed(23)
library(caTools)
s<- sample.split(wine$good,SplitRatio = 0.7)
train<- wine[s==TRUE,]
test<- wine[s==FALSE,]

glmMod<-glm(good~.,data=train,family = binomial)
pred<-predict(glmMod,newdata = test,type = "response")
table(test$good,pred>0.5)

#Tree
library(rpart)
library(rpart.plot)
tree = rpart(good ~ ., data = train, method="class", minbucket=25)

prp(tree)

# Make predictions
PredictCART = predict(tree, newdata = test, type = "class")
table(test$good, PredictCART)

library(ROCR)

PredictROC = predict(tree, newdata = test)
pred = prediction(PredictROC[,2], test$good)
perf = performance(pred, "tpr", "fpr")
plot(perf)

train2<- train
train2$good<- as.numeric(train2$good)
wineSvm <- svm(good~.,data=train2)
predSvm <- predict(wineSvm,newdata=test)
table(test$good,predSvm>0.5)

library(randomForest)
train3<- train
train3$good <- as.factor(train3$good)
forest <- randomForest(good~.,data=train3,ntree=139, nodesize=25)
predForest<- predict(forest,newdata=test)
table(test$good,predForest)


#cor
cor_wine<-cor(wine)
cor_wine[lower.tri(cor_wine,diag=TRUE)] <-NA
df_wine <- as.data.frame(as.table(cor_wine))
df_wine <- df_wine[order(-abs(df_wine$Freq)),]
#Top 10 highest correlated attributes
View(df_wine[1:10,])
#Top 5 attributes with lowest correlation against target
df_wine <- df_wine[df_wine$Var2=="quality",]
df_wine <- df_wine[order(abs(df_wine$Freq)),]
View(df_wine[1:5,])

#Shared information
require(plyr)
require(reshape)
my_fun<-function(x){freq <- count(wine,c("quality",x))
colnames(freq) <- c("Var1","Var2","Freq")
freq2d<-cast(freq, Var1 ~ Var2,fill=0,value ="Freq")
mi.plugin(freqs2d = freq2d)}

cn<- colnames(wine[,-12])
mi <- sapply(cn,my_fun)
sort(mi,decreasing = TRUE)
