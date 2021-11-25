library(tidyverse)
library(ggplot2)
library(dplyr)
library(RcmdrMisc)
data <- read.csv("Statistical Techniques.csv", header=TRUE, sep=";")
data <- subset( data, select = -c(arrival_date_week_number, arrival_date_day_of_month,
                                  market_segment,distribution_channel,reservation_status_date,
                                  total_of_special_requests,days_in_waiting_list,reservation_status,
                                  company,agent, children))
# Data type of each variable with sample values
#Check unique values of columns 
unique(data[("is_canceled")])
unique(data[("is_repeated_guest")])
# 2 columns to calculate total number of days stayed and total cost
data <- data %>%
  mutate(total_nights= stays_in_weekend_nights + stays_in_week_nights,
         total_cost= adr*total_nights)
# Convert columns as factors  
data$arrival_date_month <- as.factor(data$arrival_date_month)
data$is_canceled <- as.factor(data$is_canceled)
data$is_repeated_guest <- as.factor(data$is_repeated_guest)
data$hotel <- as.factor(data$hotel)
data$meal <- as.factor(data$meal)
data$country <- as.factor(data$country)
data$assigned_room_type <- as.factor(data$assigned_room_type)
data$reserved_room_type <- as.factor(data$reserved_room_type)
data$deposit_type <- as.factor(data$deposit_type)
data$customer_type <- as.factor(data$customer_type)
data$reservation_status_date <- as.factor(data$reservation_status_date)
data$arrival_date_year <- as.factor(data$arrival_date_year)
str(data)
# Checking missing values of the dataset
colSums(is.na(data))
#Exclude total nights=0
data<-subset(data, total_nights> "0")
#2016 year selection
data <- subset(data,arrival_date_year=="2016")
# Summary of data
summary(data)
#Descriptive statistics
data %>% select(total_nights,total_cost) %>% numSummary
data %>% select(adr,lead_time) %>% numSummary
xtab1 <- xtabs(~ hotel +is_canceled, data)
xtab1
#Histogram matrix
hist(data$total_nights,breaks="FD")
hist(data$lead_time, breaks="FD")
#Scatterplot matrix
pairs(~adr+total_nights+total_cost+lead_time, data=data,
      main="Simple Scatterplot Matrix")

#NUmber of city hotel and Resort Hotel cancelled or not cancelled 
ggplot(data = data,aes(factor(is_canceled)))+
  geom_bar( col='black', fill="#993333", alpha = 0.5) +
  facet_wrap(~hotel) +
  scale_x_discrete("Canceled",labels = c("No","Yes")) +
  scale_y_continuous("Count",limits = c(0,50000),breaks=seq(0,47222,by=5000))  +
  theme(axis.text.x = element_text(face="bold", size=10))
# Graph of Number of arrival Date by Month
ggplot(data,aes(factor(arrival_date_month,levels=month.name))) +
  geom_bar(col ="black",fill="#993333",alpha=0.5) +
  theme(axis.text.x = element_text(face="bold", size=8, angle=30)) +
  scale_y_continuous("Count",limits = c(0,15000),breaks=seq(0,15000,by=1500)) +
  scale_x_discrete("Month")    

#City Hotel Cancellation is more
data %>% group_by(data$hotel)  %>% summarise(length(is_canceled))
#Boxplot-Histograms
boxplot(data$lead_time ~ data$is_canceled)

#T-test
t.test(data$lead_time ~ data$is_canceled, mu=0, alt="two.sided", conf=0.95, var.eq=TRUE, paired=F)

#Levene Test
library(car)
leveneTest(lead_time ~ is_canceled, data = data)
#QQ plots
data_is_cancelled <- subset(data, is_canceled=="1")
data_isnt_cancelled <- subset(data, is_canceled=="0")
par(mfrow=c(1,2))
qqnorm(data_is_cancelled$lead_time, main="Is cancelled")
qqline(data_is_cancelled$lead_time, col="red")
qqnorm(data_isnt_cancelled$lead_time, main="Not cancelled")
qqline(data_isnt_cancelled$lead_time, col="red")
#Kolmogorov-Smirnov test
install.packages("dgof")
library(dgof)
ks.test(data_is_cancelled$lead_time,data_isnt_cancelled$lead_time)
#Wilcox test
wilcox.test(data$lead_time~data$is_canceled, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F)
#Boxplot 
boxplot(data$lead_time~data$hotel)
#Levene's test 2
leveneTest(lead_time ~ hotel, data = data)
#Anova one-way
data%>% group_by(data$hotel)  %>% summarize(mean_size = mean(lead_time, na.rm = TRUE))
anova1 <- aov(data$lead_time~ data$hotel)
summary(anova1)
anova1$coefficients
#Anova two-way
anova2 <- aov(lead_time ~ hotel + is_canceled, data = data)
summary(anova2)
#Linear Regression
model1 <- lm(data$lead_time ~ data$adr)
par(mfrow=c(2,2))
summary(model1)
plot(model1)
#Multiple linear regression
model2 <-lm(data$lead_time~data$adr + data$is_canceled)
summary(model2)
plot(model2)
#Multiple Logistic Reggresion)
str(data)
model3 <- glm(hotel ~ lead_time+total_nights+total_cost+is_canceled+is_repeated_guest,data=data,family=binomial)
summary(model3)
coef(model3)
#predict
glm_probs <- predict(model3, type="response")
glm_probs[1:10]
contrasts(data$hotel)
glm_pred <- rep('City Hotel', 56290)
table(glm_pred, data$hotel)
glm_pred[glm_probs > 0.5] <- "Resort Hotel"
table(glm_pred, data$hotel)
#Accuracy
accuracy <- mean(glm_pred==data$hotel)
print(accuracy)
#Error rate
error_rate <- print(1 - accuracy)
error_rate
#Sensitivity
sensitivity <- (36130/(36130+14161) )
sensitivity
#Specificity
specificity <- (4182/(4182+1817))
specificity
