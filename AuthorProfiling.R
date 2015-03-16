# load dataset
dataset <- read.csv("TCSS555- Author Profiling.csv", header=TRUE)

# view dataset
data(dataset)
summary(dataset)
sapply(dataset, class)

# install packages
install.packages("nnet")
install.packages("Metrics")
install.packages("neuralnet")
install.packages("devtools")

# load libraries
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(Metrics)
library(nnet)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

# Build individual datasets for each Big Five Personality Trait with 21 selected input values
dat1 <- dataset[,c("OpenS","Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
dat1
summary(dat1)

dat2 <- dataset[,c("ConsS","Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
dat2
summary(dat2)

dat3 <- dataset[,c("ExtS","Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
dat3
summary(dat3)

dat4 <- dataset[,c("AgrS","Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
dat4
summary(dat4)

dat5 <- dataset[,c("NeuS","Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
dat5
summary(dat5)

# Perform 10-Fold Cross Validation and build training and testing data for each individual dataset
tc1 <- trainControl("cv",10,savePred=T)
(fit1 <- train(OpenS~.,data=dat1,method="glm",trControl=tc1,family=poisson(link = "log")))
head(fit1$pred)
inTraining1 <- createDataPartition(dat1$OpenS, p = .9, list = FALSE)
training1 <- dat1[inTraining1,]
testing1 <- dat1[-inTraining1,]
summary(training1)
summary(testing1)
x1 <- training1[,c("Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
summary(x1)
y1 <-training1[,c("OpenS")]
summary(y1)
x1<-data.frame(x1)
y1<-data.frame(y1)

tc2 <- trainControl("cv",10,savePred=T)
(fit2 <- train(ConsS~.,data=dat2,method="glm",trControl=tc2,family=poisson(link = "log")))
head(fit2$pred)
inTraining2 <- createDataPartition(dat2$ConsS, p = .9, list = FALSE)
training2 <- dat2[inTraining2,]
testing2 <- dat2[-inTraining2,]
summary(training2)
summary(testing2)
x2 <- training2[,c("Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
summary(x2)
y2 <-training2[,c("ConsS")]
summary(y2)
x2<-data.frame(x2)
y2<-data.frame(y2)

tc3 <- trainControl("cv",10,savePred=T)
(fit3 <- train(ExtS~.,data=dat3,method="glm",trControl=tc3,family=poisson(link = "log")))
head(fit3$pred)
inTraining3 <- createDataPartition(dat3$ExtS, p = .9, list = FALSE)
training3 <- dat3[inTraining3,]
testing3 <- dat3[-inTraining3,]
summary(training3)
summary(testing3)
x3 <- training3[,c("Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
summary(x3)
y3 <-training3[,c("ExtS")]
summary(y3)
x3<-data.frame(x3)
y3<-data.frame(y3)

tc4 <- trainControl("cv",10,savePred=T)
(fit4 <- train(AgrS~.,data=dat4,method="glm",trControl=tc4,family=poisson(link = "log")))
head(fit4$pred)
inTraining4 <- createDataPartition(dat4$AgrS, p = .9, list = FALSE)
training4 <- dat4[inTraining4,]
testing4 <- dat4[-inTraining4,]
summary(training4)
summary(testing4)
x4 <- training4[,c("Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
summary(x4)
y4 <-training4[,c("AgrS")]
summary(y4)
x4<-data.frame(x4)
y4<-data.frame(y4)

tc5 <- trainControl("cv",10,savePred=T)
(fit5 <- train(NeuS~.,data=dat5,method="glm",trControl=tc5,family=poisson(link = "log")))
head(fit5$pred)
inTraining5 <- createDataPartition(dat5$NeuS, p = .9, list = FALSE)
training5 <- dat5[inTraining5,]
testing5 <- dat5[-inTraining5,]
summary(training5)
summary(testing5)
x5 <- training5[,c("Positive","Negative","firstPersonSingular","firstPersonPlural","secondPerson","thirdPersonSingular","thirdPersonPlural","posSelfImage","negSelfImage","Pleasantness","avgWordLength","avgSentenceLength","numSyllables","avgSyllablesPerWord","LexicalDiversity","numPassiveVerbs","SWNpositivity","SWNnegativity","SWNobjectivity")]
summary(x5)
y5 <-training5[,c("NeuS")]
summary(y5)
x5<-data.frame(x5)
y5<-data.frame(y5)


# Train neural network with varied number of hidden nodes(indicated by size), track training runtime of each
set.seed(2)

system.time (dat1mod1<-nnet(x1,y1,size=2,linout=T))
system.time (dat1mod2<-nnet(x1,y1,size=5,linout=T))
system.time (dat1mod3<-nnet(x1,y1,size=10,linout=T))
system.time (dat1mod4<-nnet(x1,y1,size=25,linout=T))

system.time (dat2mod1<-nnet(x2,y2,size=2,linout=T))
system.time (dat2mod2<-nnet(x2,y2,size=5,linout=T))
system.time (dat2mod3<-nnet(x2,y2,size=10,linout=T))
system.time (dat2mod4<-nnet(x2,y2,size=25,linout=T))

system.time (dat3mod1<-nnet(x3,y3,size=2,linout=T))
system.time (dat3mod2<-nnet(x3,y3,size=5,linout=T))
system.time (dat3mod3<-nnet(x3,y3,size=10,linout=T))
system.time (dat3mod4<-nnet(x3,y3,size=25,linout=T))

system.time (dat4mod1<-nnet(x4,y4,size=2,linout=T))
system.time (dat4mod2<-nnet(x4,y4,size=5,linout=T))
system.time (dat4mod3<-nnet(x4,y4,size=10,linout=T))
system.time (dat4mod4<-nnet(x4,y4,size=25,linout=T))

system.time (dat5mod1<-nnet(x5,y5,size=2,linout=T))
system.time (dat5mod2<-nnet(x5,y5,size=5,linout=T))
system.time (dat5mod3<-nnet(x5,y5,size=10,linout=T))
system.time (dat5mod4<-nnet(x5,y5,size=25,linout=T))

# Plot each trained neural network
plot.nnet(dat1mod1)
plot.nnet(dat1mod2)
plot.nnet(dat1mod3)
plot.nnet(dat1mod4)

plot.nnet(dat2mod1)
plot.nnet(dat2mod2)
plot.nnet(dat2mod3)
plot.nnet(dat2mod4)

plot.nnet(dat3mod1)
plot.nnet(dat3mod2)
plot.nnet(dat3mod3)
plot.nnet(dat3mod4)

plot.nnet(dat4mod1)
plot.nnet(dat4mod2)
plot.nnet(dat4mod3)
plot.nnet(dat4mod4)

plot.nnet(dat5mod1)
plot.nnet(dat5mod2)
plot.nnet(dat5mod3)
plot.nnet(dat5mod4)

# Show accuracy of prediction and variance with number of hidden nodes
pred<-predict(dat1mod1, testing1)
plot(pred)
pred<-predict(dat1mod2, testing1)
plot(pred)
pred<-predict(dat1mod3, testing1)
plot(pred)
pred<-predict(dat1mod4, testing1)
plot(pred)

pred<-predict(dat2mod1, testing1)
plot(pred)
pred<-predict(dat2mod2, testing1)
plot(pred)
pred<-predict(dat2mod3, testing1)
plot(pred)
pred<-predict(dat2mod4, testing1)
plot(pred)

pred<-predict(dat3mod1, testing1)
plot(pred)
pred<-predict(dat3mod2, testing1)
plot(pred)
pred<-predict(dat3mod3, testing1)
plot(pred)
pred<-predict(dat3mod4, testing1)
plot(pred)

pred<-predict(dat4mod1, testing1)
plot(pred)
pred<-predict(dat4mod2, testing1)
plot(pred)
pred<-predict(dat4mod3, testing1)
plot(pred)
pred<-predict(dat4mod4, testing1)
plot(pred)

pred<-predict(dat5mod1, testing1)
plot(pred)
pred<-predict(dat5mod2, testing1)
plot(pred)
pred<-predict(dat5mod3, testing1)
plot(pred)
pred<-predict(dat5mod4, testing1)
plot(pred)