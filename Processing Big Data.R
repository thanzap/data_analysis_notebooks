library(dplyr)
library(tidyverse)
data <- read.csv("processing_big_data.csv", header=TRUE, sep=";")
str(data)

# Removing columns
data <- subset( data, select = -c( number_of_reviews,
                                          name,host_name,last_review,host_id,neighbourhood))
# Checking NA's data
colSums(is.na(data))
row.has.na <- apply(data, 1, function(x){any(is.na(x))})
data <- data[!row.has.na,]
# Removing rows with price=0
data <- subset(data, price>0)
# Transforiming char variables as factors
str(data)
data$room_type <-as.factor(data$room_type)
data$neighbourhood_group<- as.factor(data$neighbourhood_group)
levels(data$room_type)
str(data)
# Histogram of price
hist(data$price)
hist(data$reviews_per_month)
# Removing outliers of price and number of reviews
data <- subset(data, reviews_per_month < 194)
data <- subset( data, price <400)
par(mfrow=c(1,2))
boxplot(data$price, main="Price boxplot")
boxplot(data$reviews_per_month, main="Reviews per month boxplot")
# Removing others type of rooms
summary(data)
# Expensive or not
mean(data$price)
data$expensive <- factor(ifelse(data$price <= 122.63,"Cheap", "Expensive" ))
str(data)
# Scatterplot matrix
data <- subset(data, reviews_per_month < 1500)
pairs(~price+latitude+longitude+minimum_nights+availability_365+reviews_per_month, data=data,
      main="Simple Scatterplot Matrix")
# Linear model into polynomial model
lm_fit <- lm(price~reviews_per_month, data=data)
summary(lm_fit)
par(mfrow=c(2,2))
plot(lm_fit)
lm_fit2<- lm(price~poly(reviews_per_month,2), data=data)
summary(lm_fit2)
plot(lm_fit2)
reviews.limits <- range(data$reviews_per_month)# Get min and max of reviews
reviews.grid <- seq(from=reviews.limits[1], to=reviews.limits[2])# Create a respective sequence
preds <- predict(lm_fit, newdata=list(reviews_per_month=reviews.grid), se=TRUE)
se.bands <- cbind(preds$fit-2*preds$se.fit, preds$fit+2*preds$se.fit)# Create +- 2*SE region
# Plotting the data 
plot(data$reviews_per_month, data$price, xlim=reviews.limits, xlab="Reviews", ylab="Price", cex=0.5, col="darkgrey")
title("Linear regression model")
lines(reviews.grid,preds$fit,lwd=2,col="blue")
matlines(reviews.grid,se.bands,lwd=1,col="blue",lty=3)
lm_fit3 <- lm(price~poly(reviews_per_month,3),data=data)
lm_fit4 <- lm(price~poly(reviews_per_month,4),data=data)
lm_fit5 <- lm(price~poly(reviews_per_month,5),data=data)
anova(lm_fit,lm_fit2,lm_fit3,lm_fit4,lm_fit5)
# As we can see the simple linear model is the most statistically significant
#Predict an airbnb appartment of 122.63$
lm_fit6 <- glm(I(price>122.63)~ reviews_per_month,data=data,family=binomial)
preds2 <- predict(lm_fit6,newdata=list(reviews_per_month=reviews.grid),se=T)
pfit <- exp(preds2$fit)/(1+exp(preds2$fit)) 
se.bands2.logit<-cbind(preds2$fit-2*preds2$fit, preds2$fit+preds2$se.fit)
se.bands2 <- exp(se.bands2.logit)/(1+exp(se.bands2.logit))
plot(data$reviews_per_month, I(data$price), xlim=reviews.limits, xlab="Reviews", ylab="Pr(Price>122.63$|Reviews per month",type="n",ylim=c(0,2))
title("Prediction on linear regression model")
lines(reviews.grid,pfit,lwd=2,col="blue")
matlines(reviews.grid,se.bands2,lwd=1,col="blue",lty=3)
# Logistic Regression
glm_fit <- glm(expensive~latitude+longitude+minimum_nights+availability_365+reviews_per_month,data=data,family=binomial)
summary(glm_fit)
summary(glm_fit)$coef
glm_fit2 <- glm(expensive~minimum_nights+availability_365+reviews_per_month,data=data,family=binomial)
summary(glm_fit2)
coef(glm_fit2)
anova(glm_fit,glm_fit2)
#predict
glm_probs <- predict(glm_fit2, type="response")
glm_probs[1:10]
contrasts(data$expensive)
glm_pred <- rep('Cheap', 28061)
table(glm_pred, data$expensive)
glm_pred[glm_probs > 0.5] <- "Expensive"
table(glm_pred, data$expensive)
#Accuracy
mean(glm_pred==data$expensive)
#Error rate
print(1 - accuracy)
#split dataset on ratio of 70/30% based on id= 3rd Quartile 
summary(data)
train <- data$id < 25572892
data_train <- data[train,]
data_test <-data[!train,]
glm_fit2 <- glm(expensive~minimum_nights+availability_365,data=data_train,family=binomial)
glm_probs <- predict(glm_fit2,data_test,type="response")
glm_pred <- rep("Cheap",7016)
glm_pred[glm_probs>0.5] <-  "Expensive"
table(glm_pred, data_test$expensive)
mean(glm_pred==data_test$expensive)
#QDA
library(MASS)
qda_fit <- qda(expensive ~ minimum_nights+availability_365+reviews_per_month, data_train)
qda_fit
qda_pred <- predict(qda_fit, data_test)
qda_class <- qda_pred$class
table(qda_class, data_test$expensive)
mean(qda_class == data_test$expensive)
#Unsupervised Learning
unique(data$room_type)
data$room_type <- as.numeric(as.factor(data$room_type))
data$neighbourhood_group <- as.numeric(as.factor(data$neighbourhood_group))
data$expensive <- as.numeric(as.factor(data$expensive))
str(data)
#PCA analysis
pca<- prcomp(data, scale=TRUE)
summary(pca)
plot(pca)
plot(pca$x[,1],pca$x[,2])
pca_var <- pca$sdev^2
pca_var_per <- round(pca_var/sum(pca_var)*100, 1)
barplot(pca_var_per,Main="Scree Plot",xlab="Principal Component",ylab="Percent Variation")
pve <- 100*pca$sdev^2/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve,type="o",ylab="Cumulative PVE",xlab="Principal Component",col="red")
plot(cumsum(pve),type="o",ylab="Cumulative PVE", xlab="Principal Component", col="red")
# K-means clustering
set.seed(123)
data <-scale(data)
#k=2
km_out2<- kmeans (data,2, nstart=25)
km_out2$cluster
plot(pca$x[,1],pca$x[,2],col=(km_out2$cluster+1),main="K=2",xlab="",ylab="",pch=20, cex=2)
km_out2
km_out2$tot.withinss
#k=3
km_out3<- kmeans (data,3, nstart=25)
km_out$cluster
plot(pca$x[,1],pca$x[,2],col=(km_out3$cluster+1),main="K=3",xlab="",ylab="",pch=20, cex=2)
km_out3
km_out3$tot.withinss
