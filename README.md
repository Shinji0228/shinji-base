# shinji-base
---
---
title: "Untitled"
author: "Shinji Yamamoto"
date: "2022-11-04"
output: word_document
---

データの読み込み   
```{r}
RPj <- read.csv("R_Pj.csv",row.names = 1)
RP <- read.csv("R_P.csv",row.names = 1)
PPj <- read.csv("Pj_P.csv",row.names = 1)
library(glmnet)
library(caret)
```
RPJ 20品詞のelastic model
```{r}
m <- round(nrow(RPj)/3)
set.seed(1)
sp <- sample(1:nrow(RPj),m)
train_data <- RPj[-sp,]
test_data <- RPj[sp,]
train_data$type <- as.factor(train_data$type)
class(train_data$type)
set.seed(1)

cv10 <- trainControl(method="cv",number=10)
tuneGrid <- expand.grid(alpha=seq(0),lambda=seq(0,1,0.1))
elnet <- train(type~.,data=train_data,method="glmnet",trControl=cv10,tuneGrid=tuneGrid)
pre.test <- predict(elnet,test_data)
conMe1 <- table(pre.test,test_data[,1])
confusionMatrix(conMe1,mode = "prec_recall")
plot(varImp(elnet,scale = FALSE),20,cex=1.5)

elnet_coef <- predict(elnet$finalModel, type = "coefficients", s = elnet$bestTune$lambda)
elnet_coef
rownames(elnet_coef)
elnet_coef.1 <- elnet_coef[sort.list(elnet_coef[,1]),]
elnet_coef.1
barplot(elnet_coef.1,horiz=TRUE,las=2,col = c(rep("green4",8),rep("turquoise",12)),cex.axis = 0.7,cex.names = 0.35,xlab = "緑＝その他　青＝プロレタリア児童文学")
```

RP 20品詞 elastic model
```{r}
m1 <- round(nrow(RP)/3)
set.seed(0)
sp1 <- sample(1:nrow(RP),m1)
train_data1 <- RP[-sp1,]
test_data1 <- RP[sp1,]
train_data1$type <- as.factor(train_data1$type)
class(train_data1$type)
set.seed(1)
cv10 <- trainControl(method="cv",number=10)
tuneGrid <- expand.grid(alpha=seq(0),lambda=seq(0,1,0.1))
elnet1 <- train(type~.,data=train_data1,method="glmnet",trControl=cv10,tuneGrid=tuneGrid)
pre.train1 <- predict(elnet1,test_data1)
conMel1 <- table(pre.train1,test_data1[,1])
confusionMatrix(conMel1,mode="prec_recall")
plot(varImp(elnet1),10,cex=1.5)

elnet_coef1 <- predict(elnet1$finalModel, type = "coefficients", s = elnet1$bestTune$lambda)
elnet_coef1
rownames(elnet_coef1)
elnet_coef2.3 <- elnet_coef1[sort.list(elnet_coef1[,1]),]
elnet_coef2.3
barplot(elnet_coef2.3,horiz=TRUE,las=2,col = c(rep("green4",9),rep("pink",11)),cex.axis = 0.7,cex.names = 0.35,xlab = "緑＝その他　桃＝プロレタリア文学")
```
PPJ 20品詞 elastic model
```{r}

m2 <- round(nrow(PPj)/3)
set.seed(3)
sp2 <- sample(1:nrow(PPj),m2)
train_data2 <- PPj[-sp2,]
test_data2 <- PPj[sp2,]
train_data2$type <- as.factor(train_data2$type)
class(train_data2$type)

set.seed(1)
cv10 <- trainControl(method="cv",number=10)
tuneGrid <- expand.grid(alpha=seq(0),lambda=seq(0,1,0.1))
elnet2 <- train(type~.,data=train_data2,method="glmnet",trControl=cv10,tuneGrid=tuneGrid)
pre.train2 <- predict(elnet2,test_data2)
conMel2 <- table(pre.train2,test_data2[,1])
confusionMatrix(conMel2,mode="prec_recall")
plot(varImp(elnet2),10,cex=1.5)

elnet_coef2 <- predict(elnet2$finalModel, type = "coefficients", s = elnet2$bestTune$lambda)
rownames(elnet_coef2)
elnet_coef2.2 <- elnet_coef2[sort.list(elnet_coef2[,1]),]
elnet_coef2.2

barplot(elnet_coef2.2,horiz=TRUE,las=2,col = c(rep("turquoise",8),rep("pink",12)),cex.axis = 0.7,cex.names = 0.35,xlab = "桃＝プロレタリア文学　青＝プロレタリア児童文学")

```
PPJ 感動詞　elastic model 
```{r}

ppj <- read.csv("P_Pjkando.csv",row.names = 1)
m4 <- round(nrow(ppj)/3)
set.seed(1)
sp4 <- sample(1:nrow(PPj),m4)
train_data4 <- ppj[-sp4,]
test_data4 <- ppj[sp4,]
train_data4$type <- as.factor(train_data4$type)
class(train_data4$type)
set.seed(3)
cv10 <- trainControl(method="cv",number=10)
tuneGrid <- expand.grid(alpha=seq(0,1,0.1),lambda=seq(0,1,0.1))
elnet4 <- train(type~.,data=train_data4,method="glmnet",trControl=cv10,tuneGrid=tuneGrid)
pre.elnet4 <- predict(elnet4,test_data4)
confusionMatrix(table(pre.elnet4,test_data4[,1]),mode="prec_recall")
plot(varImp(elnet4),10,cex=1.5)
pre.elnet4.coef <- predict(elnet4$finalModel, type = "coefficients", s = elnet4$bestTune$lambda)
elnet_coef2.2 <- elnet_coef2[sort.list(elnet_coef2[,1]),]

elnet4.coef <- pre.elnet4.coef[sort.list(pre.elnet4.coef[,1]),]
elnet4.coef
dim(ppj)
el4c <- elnet4.coef[c(1:5,73:82)]
el4c1 <- elnet4.coef[1:26]
el4c1

el4c2 <- elnet4.coef[55:81]
el4c2
barplot(el4c,horiz = TRUE,las=2,col=c(rep("turquoise",5),rep("pink",10)),cex.axis = 0.7,cex.names = 0.35,xlab = "桃＝プロレタリア文学　青＝プロレタリア児童文学")
```
PPJ 感嘆符のelasticelastic  model

```{r}
P_PJ_kan1 <- read.csv("P_PJ.kantan_cutoff0.csv",row.names = 1)
mk1 <- round(nrow(P_PJ_kan1)/3)
set.seed(3)
spk2 <- sample(1:nrow(P_PJ_kan1),mk1)
ktrain_data2 <- P_PJ_kan1[-spk2,]
ktest_data2 <- P_PJ_kan1[spk2,]
ktrain_data2$type <- as.factor(ktrain_data2$type)
class(ktrain_data2$type)

cv10 <- trainControl(method="cv",number=10)
tuneGrid <- expand.grid(alpha=seq(0,1,0.1),lambda=seq(0,1,0.1))
elnet <- train(type~.,data=ktrain_data2,method="glmnet",trControl=cv10,tuneGrid=tuneGrid)
pre.test <- predict(elnet,ktest_data2)
conMe1 <- table(pre.test,ktest_data2[,1])
confusionMatrix(conMe1,mode = "prec_recall")
plot(varImp(elnet,scale = FALSE),20,cex=1.5)

elnet_coef <- predict(elnet$finalModel, type = "coefficients", s = elnet$bestTune$lambda)
elnet_coef
rownames(elnet_coef)
elnet_coef.1 <- elnet_coef[sort.list(elnet_coef[,1]),]
barplot(elnet_coef.1)
```
Word cloud
```{r}
P_wordcloud <- read.csv("P_wordcloud.csv",row.names = 1)
PJ_wordcloud <- read.csv("PJ_wordcloud.csv",row.names = 1)
PM_wordcloud <- read.csv("P_wordcloud_meishi.csv",row.names = 1)
PJM_wordcloud <- read.csv("PJ_wordcloud_meisi.csv",row.names = 1)
R_wordcloud <- read.csv("R_wordcloud.csv",row.names = 1)


library(wordcloud)
commonality.cloud(P_wordcloud, max.words=500, colors=brewer.pal(8, "Dark2"),main="プロレタリア")
commonality.cloud(PM_wordcloud, max.words=500, colors=brewer.pal(8, "Dark2"),main="プロレタリア")　#名詞

commonality.cloud(PJ_wordcloud, max.words=500, colors=brewer.pal(8, "Dark2"),main="プロレタリア児童文学")
commonality.cloud(PJM_wordcloud, max.words=500, colors=brewer.pal(8, "Dark2"),main="プロレタリア児童文学")　#名詞

commonality.cloud(R_wordcloud, max.words=500, colors=brewer.pal(8, "Dark2"),main="その他作品")



```

```
