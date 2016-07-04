setwd("~/git/kaggle-pneumovax")

# load libraries
library(lambda.tools)
library(futile.logger)
library(randomForest)
library(ggplot2)
library(caret)

# read in data
pneumo.train <- read.csv('Data/training.csv', stringsAsFactors=FALSE)
pneumo.train$grouping <- as.factor(pneumo.train$grouping)
pneumo.train$in.epi <- as.factor(pneumo.train$in.epi)
vars <- colnames(pneumo.train)[! colnames(pneumo.train) %in% c('code','country','year', 'in.epi', 'grouping')]

# read in test dta
test <- read.csv('Data/testing.csv', stringsAsFactors=FALSE)
test$grouping <- factor(test$grouping, levels=levels(pneumo.train$grouping))
test$in.epi <- factor(test$in.epi, levels=levels(pneumo.train$in.epi))
vars <- colnames(test)[! colnames(test) %in% c('code','country','year')]

############ explorations ############ 
# let's do some base understanding of relationships with graphs
income <- ggplot(pneumo.train, aes(x = gni.2013, y = deaths.per.1k)) 
income +geom_point(aes(color=deaths.per.1k))
# this shows me that several low income countries have high pneumonia deaths

# relationship of PCV coverage
pop.vaccinated <- ggplot(pneumo.train, aes(x = pct.pop.vaccinated, y = deaths.per.1k)) 
pop.vaccinated +geom_point() + geom_smooth(method = 'lm')
# too many missing values, not easy to make assumptions and can't tell if it's a linear relationship

# relationship of introduction of PCV into EPI schedule
ggplot(pneumo.train, aes(x = deaths.per.1k, fill = factor(in.epi))) + 
geom_density() + facet_grid(in.epi ~ .)
# this shows me more countries DON'T use PCV, and deaths are higher when PCV not introduced (duh)


############ now let's build models ############ 
# build basic linear model
model1 <- glm(deaths.per.1k ~ total.usd.2013 + total.ppp.2013 + govt.usd.2013 +
               govt.ppp.2013 + pct.deaths.5y + pct.pop.vaccinated +
               PCV10.Price + PCV13.Price + gni.2013 + sanitation.2013 + 
               birth.rate.2013, data = pneumo.train)

model2 <- glm(deaths.per.1k ~  pct.pop.vaccinated , data = pneumo.train)

# evaluate models 1 and 2
plot(model1, which = 1)
plot(model2, which = 1)
# this shows me that model 1 has too few predictions, model 2 has more predictions, but not great
errors <- residuals(model2)
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
rmse <- sqrt(mse)
rmse

############ first submission ############ 
pred <- predict(model2, newdata=test[,vars])
out2 <- data.frame(Id=test$code, Prediction=pred)
out2$Prediction[is.na(out2$Prediction)] <- 0
#too many NA's in output
#write.csv(out2, 'submission.csv', row.names=FALSE, quote=FALSE)

# use PCA to determine principal components
cor.matrix <- cor(pneumo.train[, vars], y= NULL,  use = "na.or.complete")
correlations <- as.numeric(cor.matrix)

# density plot of correlation to see relationship
ggplot(data.frame(Correlation = correlations), aes(x = Correlation, fill = 1)) +
  geom_density() 

# combing the two price columns because there are few intersects
# will make a rough estimate and use the mean of prices for countries with both PCV prices
pneumo.train$PCV.Price <- rowMeans(pneumo.train[,c("PCV10.Price", "PCV13.Price")], na.rm = TRUE)
# multiply price by 3 because there are 3 doses per child
pneumo.train$PCV.Price <- pneumo.train$PCV.Price * 3

?rowSums
# see what the correlation is between GNI and price
ggplot(pneumo.train, aes(x = gni.2013, y = PCV.Price)) + geom_point()
# this shows me some countries in the same income level pay different prices
# impute prices for missing data
PCV.Price.Impute<- preProcess(pneumo.train[,29], method="knnImpute", na.remove = FALSE)
?preProcess

# predict basic linear model
pred <- predict(model2, newdata=test[,vars])
out2 <- data.frame(Id=test$code, Prediction=pred)
out2$Prediction[is.na(out2$Prediction)] <- 0
#too many NA's in output
write.csv(out2, 'submission.csv', row.names=FALSE, quote=FALSE)



write.csv(out, 'submission.csv', row.names=FALSE, quote=FALSE)

