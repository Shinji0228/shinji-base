# shinji-base
---
---
title: "Untitled"
author: "Shinji Yamamoto"
date: "2022-11-04"
output: word_document
---
説明
このコードを一言で表すと、3ジャンルの文学作品をテキストマイニングし、文学ジャンルごとの特徴品詞を抽出し、
それらの特徴品詞から、特徴語を抽出しています。
３ジャンルはプロレタリア文学、プロレタリア児童文学、その他文学です。
抽出方法は特徴品詞、特徴語、共にelastic net回帰分析です。

elastic net回帰分析はデータから特徴的な単語や品詞を抽出する方法です。

注意点
コードが長いため、一部省いています。

特徴品詞を出すためのデータの読み込み   
```{r}
RPj <- read.csv("R_Pj.csv",row.names = 1)　R＝その他作品、Pj＝プロレタリア児童文学作品　RPj＝その他作品とプロレタリア児童文学作品を組み合わせたデータ
RP <- read.csv("R_P.csv",row.names = 1)　　RP＝その他作品とプロレタリア文学作品を組み合わせたデータ
PPj <- read.csv("Pj_P.csv",row.names = 1)
library(glmnet)
library(caret)
```
RPJ 20品詞のelastic net回帰分析　 その他作品とプロレタリア児童文学作品を組み合わせたデータから特徴的な品詞を抽出している。
```{r}
m <- round(nrow(RPj)/3)　　　　　データを3分割するための行数を得る　　150÷3=50なら50行ずつ取得するということ
set.seed(1)　　　　　　　　　　　乱数値を固定　→　実行する度に結果が変わることを予防するため
sp <- sample(1:nrow(RPj),m)　　データの3分の1を取得　　　　　　　　　　　学習データとテストデータを2対1に分けるため
train_data <- RPj[-sp,]　　　　学習データとして、データの3分の2を取得　　
test_data <- RPj[sp,]　　　　　テストデータとして、学習データ以外のデータを取得
train_data$type <- as.factor(train_data$type)　データの目的変数を因子型に変換（基準となるデータを別の型に変え他の型と違うよとPCに識別させるため）　
class(train_data$type)　　　　　　　　　　　　　　因子型にできたかの確認


cv10 <- trainControl(method="cv",number=10)　　　　　　　　　elastic net回帰分析で使う方法を指定
tuneGrid <- expand.grid(alpha=seq(0),lambda=seq(0,1,0.1))　elastic net回帰分析で使うパラメータを指定
elnet <- train(type~.,data=train_data,method="glmnet",trControl=cv10,tuneGrid=tuneGrid)　elasticnet回帰分析を行っている　
pre.test <- predict(elnet,test_data)　　　　　　　　　　　　　　　　　　　　　　　　　　　　　テストデータを使い回帰分析が正しいか検証
conMe1 <- table(pre.test,test_data[,1])　　　　　　　　　　　　　　　　　　　　　　　　　　　テストデータを使い回帰分析が正しいか検証
confusionMatrix(conMe1,mode = "prec_recall")　　　　　　　　　　　　　　　　　　　　　　　　分析結果の正解率などを出力　　　　　　　　　　　　　　　　　　　　　　

elnet_coef <- predict(elnet$finalModel, type = "coefficients", s = elnet$bestTune$lambda)　　最適なパラメータの値を出力し特徴品詞を抽出
elnet_coef.1 <- elnet_coef[sort.list(elnet_coef[,1]),]　　　　　　　　　　　　　　　　　　　　　特徴品詞を重要な順に並び替え　　　　　　　　　　　　　　　　　　　
elnet_coef.1　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　結果の確認
barplot(elnet_coef.1,horiz=TRUE,las=2,col = c(rep("green4",8),rep("turquoise",12)),cex.axis = 0.7,cex.names = 0.35,xlab = "緑＝その他　青＝プロレタリア児童文学")　　　　　　　　　　　　　　　　　　　　　　　　　
　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　特徴品詞の重要度を棒グラフとし図示
```

RP 20品詞 elastic net回帰分析　　データが変わっただけです。
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
PPJ 20品詞 elastic net回帰分析　　　　データが変わっただけです。
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
以上の分析で3ジャンルの特徴品詞が抽出できました。
次は特徴語を抽出します。
得られた特徴品詞を使い、別のソフトでデータを加工し読み込みます。

特徴語を抽出するためのデータの読み込み
```{r}
P <- read.csv("P_Pjrandom_hirakann.csv",row.names = 1) プロレタリアとプロレタリア児童文学作品を組み合わせたデータです。

R <- read.csv("P_Rrandom.csv",row.names = 1)

PJ <- read.csv("R_PJ_elastic.csv",row.names = 1)
```
```
PPJ 特徴品詞5の単語 elastic net回帰分析　　　　　　プロレタリア文学とプロレタリア児童文学作品の特徴語を抽出しています。
　　　　　　　　　　　　　　　　　　　　　　　　　　特徴品詞の抽出とほとんど同じです。
　　　　　　　　　　　　　　　　　　　　　　　　　　
```{r}
dim(P)
nrow(P)
m <- round(nrow(P)/3)　　　　　　　　　　　　　
set.seed(1)
sp <- sample(1:nrow(P),m)
sp
train_data <- P[-sp,]
train_data
test_data <- P[sp,]
train_data$type <- as.factor(train_data$type) 
class(train_data$type)

cv10 <- trainControl(method="cv",number=10)
tuneGrid <- expand.grid(alpha=seq(0,1,0.1),lambda=seq(0,1,0.1))
elnet <- train(type~.,data=train_data,method="glmnet",trControl=cv10,tuneGrid=tuneGrid)
pre.test <- predict(elnet,test_data)
conMe1 <- table(pre.test,test_data[,1])
confusionMatrix(conMe1,mode = "prec_recall")

elnet_coef <- predict(elnet$finalModel, type = "coefficients", s = elnet$bestTune$lambda)
elnet_coef
rownames(elnet_coef)
elnet_coef.1 <- elnet_coef[sort.list(elnet_coef[,1]),]
elnet_coef.1
dim(P)
selnet_coef.1 <- elnet_coef.1[138:157]　　　　　　　　　　　　　　　　　　　図示できないほどデータ数が多いため、特に重要となる語を指定
selnet_coef.1　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　指定した語を確認

barplot(selnet_coef.1,las=2,horiz = T,col = c(rep("pink",14),rep("turquoise",15)),cex.axis = 0.7,cex.names = 0.2,xlab = "青＝プロレタリア文学　桃＝プロレタリア児童文学")　　　　　　　　　　　　　　　　　　　　　　　　　　　　　指定した語の重要度を棒グラフにし図示

```
PR 5品詞　elastic net回帰分析　　　　データが変わっただけです。

```{r}
m2 <- round(nrow(R)/3)
set.seed(1)
sp2 <- sample(1:nrow(R),m2)
#sp2
train_data2 <- R[-sp2,]
#train_data2
test_data2 <- R[sp2,]
train_data2$type <- as.factor(train_data2$type) # 因子に変換
class(train_data2$type)

cv10 <- trainControl(method="cv",number=10)
tuneGrid <- expand.grid(alpha=seq(0,1,0.1),lambda=seq(0,1,0.1))
elnet <- train(type~.,data=train_data2,method="glmnet",trControl=cv10,tuneGrid=tuneGrid)
pre.test <- predict(elnet,test_data2)
conMe1 <- table(pre.test,test_data2[,1])
confusionMatrix(conMe1,mode = "prec_recall")

elnet_coef <- predict(elnet$finalModel, type = "coefficients", s = elnet$bestTune$lambda)
elnet_coef
rownames(elnet_coef)
elnet_coef.1 <- elnet_coef[sort.list(elnet_coef[,1]),]
elnet_coef.1
dim(P_Rrandom)
selnet_coef.1 <- elnet_coef.1[110:139]
selnet_coef.1
selnet_coef.2 <- elnet_coef.1[1:40]
selnet_coef.2


selnet_coef.1
barplot(selnet_coef.1,las=2,col = rep("pink",20),cex.axis = 0.7,cex.names = 0.25,horiz = T,main = "プロレタリア文学の特徴語")

barplot(selnet_coef.2,las=2,col = rep("green4",20),cex.axis = 0.7,cex.names = 0.25,horiz = T,main = "その他作品の特徴語")

```
RPJ 5品詞　elastic net回帰分析　　　　　　　　　　　データが変わっただけです。
```{r}

m3 <- round(nrow(Pj)/2)
set.seed(1)
sp3 <- sample(1:nrow(Pj),m3)
#sp2
train_data3 <- Pj[-sp3,]
#train_data2
test_data3 <- Pj[sp3,]
train_data3$type <- as.factor(train_data3$type) # 因子に変換
class(train_data3$type)

cv10 <- trainControl(method="cv",number=10)
tuneGrid <- expand.grid(alpha=seq(0,1,0.1),lambda=seq(0,1,0.1))
elnet <- train(type~.,data=train_data3,method="glmnet",trControl=cv10,tuneGrid=tuneGrid)
pre.test <- predict(elnet,test_data3)
conMe1 <- table(pre.test,test_data3[,1])
confusionMatrix(conMe1,mode = "prec_recall")

elnet_coef <- predict(elnet$finalModel, type = "coefficients", s = elnet$bestTune$lambda)
elnet_coef
rownames(elnet_coef)
elnet_coef.1 <- elnet_coef[sort.list(elnet_coef[,1]),]
elnet_coef.1
selnet_coef.1 <- elnet_coef.1[2700:2765]
selnet_coef.1
selnet_coef.2 <- elnet_coef.1[1:100]
selnet_coef.2
barplot(selnet_coef.1,las=2,col = rep("turquoise",20),cex.axis = 0.7,cex.names = 0.25,horiz = T,main = "プロレタリア児童文学作品の特徴")

barplot(selnet_coef.2,las=2,col = rep("green4",20),cex.axis = 0.7,cex.names = 0.25,horiz = T,main = "その他作品の特徴")

```
