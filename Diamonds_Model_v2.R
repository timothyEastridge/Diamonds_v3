#Objective:  Analyze relationships between diamond attributes to determine underprice vs overpriced diamonds
#This model runs all ~50k datapoints to predict new datapoints found online.
#With train/test set split, model had 90% accuracy within 25% interval & 74% accuracy within 10% interval.

#---------------------------
#---------------------------Load Libraries
libs <- c("rsconnect", "rpart","plyr","ggplot2","rpart.plot")
lapply(libs, require, character.only = T)

#---------------------------
#---------------------------Load Data
setwd("C:/Users/tesr/Documents/R/Data/Diamonds")
diamonds <- read.csv("diamonds.csv")
diamonds <- diamonds[sample(1:nrow(diamonds)), ] #randomize

#---------------------------
#---------------------------Analyze
str(diamonds) #view structure
summary(diamonds)
ggplot(data=diamonds) + geom_histogram(aes(x=diamonds$price)) + ggtitle("Price Distribution") + xlab("Price") + ylab("Frequency")

#---------------------------
#---------------------------Use regression trees to model the price of diamonds based on all attributes in the dataframe
m.rpart <- rpart(price ~ .,data = diamonds,cp = .0001)

rpart.plot(m.rpart, type = 4, extra = 101, digits = -3)

#---------------------------
#---------------------------Assign predictions back to training set
prediction <-  data.frame(predict(m.rpart, diamonds))
diamonds$prediction <- prediction

diamonds$percent <- (diamonds$price - diamonds$prediction)/diamonds$price

diamonds$correct <- ifelse(diamonds$percent < .25,1,0) #determine model accuracy
summary(diamonds$correct)
table(diamonds$correct)
sum(diamonds$correct) / nrow(diamonds) *100 #percent correct within 25% threshold

diamonds$bestDeal <- diamonds$prediction - diamonds$price
diamonds <- diamonds[order(diamonds$bestDeal),] #order dataframe

#---------------------------
#---------------------------Run  model on new diamonds found on the internet to determine which are undervalued
new_data <- read.csv("diamonds_test.csv")
results <- data.frame(predict(m.rpart, new_data))

new_data$prediction <- results
new_data$bestDeal <- new_data$prediction - new_data$price
new_data$percentDeal <- new_data$bestDeal / new_data$price *100
new_data <- new_data[order(new_data$bestDeal),]
new_data$Underpriced <- ifelse(new_data$percentDeal>0,"Good Deal","Bad Deal")
final <- data.frame(new_data$price,new_data$prediction,new_data$percentDeal,new_data$Underpriced)
final

#---------------------------
#---------------------------Write final dataframe as a csv
write.csv(final,"final.csv")
