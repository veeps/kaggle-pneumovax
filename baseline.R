library(lambda.tools)
library(futile.logger)
setwd("~/Downloads/CUNY")
library(randomForest)

df <- read.csv('training.csv', stringsAsFactors=FALSE)
head(df)
df$grouping <- as.factor(df$grouping)
df$in.epi <- as.factor(df$in.epi)
vars <- colnames(df)[! colnames(df) %in% c('code','country','year')]
model <- randomForest(deaths.per.1k ~ ., df[,vars], na.action=na.omit)

test <- read.csv('testing.csv', stringsAsFactors=FALSE)
test$grouping <- factor(test$grouping, levels=levels(df$grouping))
test$in.epi <- factor(test$in.epi, levels=levels(df$in.epi))
vars <- colnames(test)[! colnames(test) %in% c('code','country','year')]
test <- fold(vars[!vars %in% c('grouping','in.epi','pct.pop.vaccinated')], 
  function(v,acc) { acc[,v] <- as.numeric(acc[,v]); acc }, test)
pred <- predict(model, newdata=test[,vars])

?randomForest

out <- data.frame(Id=test$code, Prediction=pred)
out$Prediction[is.na(out$Prediction)] <- 0
# write.csv(out, 'submission.csv', row.names=FALSE, quote=FALSE)

