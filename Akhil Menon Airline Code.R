# Analysis of Airline Code
# Name : Akhil Menon ; Email : menonakhil90@gmail.com
# College : Sri Venkateswara College of Engineering

# Setting Work directory to the folder containing the Dataset
setwd("C:/Users/menon/Desktop/New Stuff")

# Reading the dataset and storing onto a data frame
AirLine <- read.csv("SixAirlines.csv"
                    )
# Basic view of the Data Frame
View(AirLine)

# Data Frame Summary
summary(AirLine)

# Visualization Using BoxPlots
# Using PRICE_PREMIUM and PRICE_ECONOMY as Y 
boxplot(AirLine$PRICE_PREMIUM~AirLine$AIRLINE)
boxplot(AirLine$PRICE_ECONOMY~AirLine$AIRLINE)
boxplot(AirLine$PRICE_PREMIUM~AirLine$MONTH)
boxplot(AirLine$PRICE_ECONOMY~AirLine$MONTH)

# Using PRICE_RELATIVE as Y 
boxplot(AirLine$PRICE_RELATIVE~AirLine$MONTH) 
boxplot(AirLine$PRICE_RELATIVE~AirLine$INTERNATIONAL)                    
boxplot(AirLine$PRICE_RELATIVE~AirLine$AIRCRAFT)
boxplot(AirLine$PRICE_RELATIVE~AirLine$QUALITY) 

# From the above visualization the increase in quality leads to the increase in relative price

#Scatterplot for non-categorical variables
library(car)
scatterplot(AirLine$PRICE_RELATIVE,AirLine$FLIGHT_DURATION)       
scatterplot(AirLine$PRICE_RELATIVE,AirLine$SEATS_ECONOMY)              

#CORRGRAM VISUALIZATION
library(corrgram)
corrgram(AirLine, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

corrgram(AirLine, order=NULL, lower.panel=panel.shade,upper.panel=NULL, text.panel=panel.txt)

# Variance-Covariance Matrix
cor(AirLine[,-c(1)])                  

#PRICE_RELATIVE SHOULD BE TAKEN AS Y
A1 <-lm(PRICE_RELATIVE~.,data=AirLine)
summary(A1)

#HYPOTHESIS - 1
# Mean of the RELATIVE_PRICE should not be equal to zero.


#USING THE GLM COMMAND AND CHECKING AIC VALUE FOR EACH EXCLUSION OF VARIABLE

A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM,data=AirLine)
summary(A1)                                                
# AIC VALUE DECREASES SO IT CAN BE REMOVED 

A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-PITCH_ECONOMY,data=AirLine)
summary(A1)                                                
# AIC VALUE REMAINS THE SAME SO IT CANNOT BE REMOVED

A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-PITCH_PREMIUM,data=AirLine)
summary(A1)                                                
# AIC VALUE REMAINS THE SAME SO IT CANNOT BE REMOVED

A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-WIDTH_ECONOMY,data=AirLine)
summary(A1)                                                
# AIC VALUE INCREASES SO IT CANnOT BE REMOVED


A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-MONTH,data=AirLine)
summary(A1)                                                 
# AIC VALUE DECREASES SO IT CAN BE REMOVED 

A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL,data=AirLine)
summary(A1)                                                 
# AIC VALUE DECREASES SO IT CAN BE REMOVED

A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-AIRCRAFT,data=AirLine)
summary(A1)                                                 
# AIC VALUE REMAINS THE SAME AND IT CANNOT BE REMOVED 

A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-SEATS_ECONOMY,data=AirLine)
summary(A1)                                                 
# AIC VALUE REMAINS THE SAME AND IT CANNOT BE REMOVED 


A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-SEATS_PREMIUM,data=AirLine)
summary(A1)                                                 
# AIC VALUE REMAINS THE SAME AND IT CANNOT BE REMOVED 

A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-LAMBDA,data=AirLine)
summary(A1)                                                 
#AIC VALUE INCREASES AND SO CANNOT BE REMOVED

A1<-lm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-AIRLINE,data=AirLine)
summary(A1)                                                 
#AIC VALUE INCREASES AND SO IT CANNOT BE REMOVED

A1<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-QUALITY,data=AirLine)
summary(A1)                                                 
#AIC VALUE REMAINS THE SAME AND SO CANOT BE REMOVED


#AIC VALUE OBTAINED IS MINIMUM FOR THIS MODEL TILL NOW
A1<-lm(PRICE_RELATIVE~.-INTERNATIONAL-MONTH-WIDTH_PREMIUM,data=AirLine)
summary(A1)

#ADDING INTERACTION TERMS

A1<-glm(PRICE_RELATIVE~.-INTERNATIONAL-MONTH-WIDTH_PREMIUM+WIDTH_PREMIUM*WIDTH_ECONOMY,data=AirLine)
summary(A1) # AIC INCREASES SO IT SHOULD NOT BE ADDED

A1<-glm(PRICE_RELATIVE~.-INTERNATIONAL-MONTH-WIDTH_PREMIUM+PRICE_PREMIUM*PRICE_ECONOMY,data=AirLine)
summary(A1)# AIC DECREASES SO IT SHOULD BE ADDED

#INTUITIVELY,WIDTH_PREMIUM SHOULD NOT BE REMOVED, ALSO BECAUSE CORRELATION IS HIGH
A1<-glm(PRICE_RELATIVE~.-INTERNATIONAL-MONTH+PRICE_PREMIUM*PRICE_ECONOMY,data=AirLine)
summary(A1)

#To check the prediction values from this model
predict(A1)

#Alternate Hypothesis

#The mean of the PRICE_PREMIUM is higher than the mean of the PRICE_ECONOMY.

t.test(AirLine$PRICE_ECONOMY,AirLine$PRICE_PREMIUM,alternative = "greater")

# The p-value comes out -> 1 ( i.e >0.05)  
# Hence - Hypothesis is Incorrect and Alternate Hypothesis is correct


#Linear regression for Differnce of the prices as Y
B1<-AirLine$PRICE_PREMIUM- AirLine$PRICE_ECONOMY

#R-Sqaured value equals to 1 and residual error term reduced to ~Zero
A1<-lm(B1~.-INTERNATIONAL-MONTH+PRICE_PREMIUM*PRICE_ECONOMY,data=AirLine)
summary(A1)

#AIC Value is reduced to -2260
A1<-glm(B1~.-INTERNATIONAL-MONTH+PRICE_PREMIUM*PRICE_ECONOMY,data=AirLine)
summary(A1)

#PREDICTION OF VALUES 
predict(A1)

#According to summary report it is of  ~ a-b=a-b type
# Therefore Model not cosidered
