setwd("~/git/kaggle-pneumovax")

#load libraries
library(lambda.tools)
library(futile.logger)
library(randomForest)

#read in data
pneumo.train <- read.csv('Data/training.csv', stringsAsFactors=FALSE)
pneumo.train$grouping <- as.factor(pneumo.train$grouping)
pneumo.train$in.epi <- as.factor(pneumo.train$in.epi)
vars <- colnames(pneumo.train)[! colnames(pneumo.train) %in% c('code','country','year')]

#build linear model
model <- glm(deaths.per.1k ~ total.usd.2013 + PCV10.Price + PCV13.Price + 
               gni.2013 + sanitation.2013 + birth.rate.2013, data = train)


#read in test dta
test <- read.csv('Data/testing.csv', stringsAsFactors=FALSE)
test$grouping <- factor(test$grouping, levels=levels(df$grouping))
test$in.epi <- factor(test$in.epi, levels=levels(df$in.epi))
vars <- colnames(test)[! colnames(test) %in% c('code','country','year')]


#predict deaths
pred <- predict(model, newdata=test[,vars])
out <- data.frame(Id=test$code, Prediction=pred)
out$Prediction[is.na(out$Prediction)] <- 0

write.csv(out, 'submission.csv', row.names=FALSE, quote=FALSE)

