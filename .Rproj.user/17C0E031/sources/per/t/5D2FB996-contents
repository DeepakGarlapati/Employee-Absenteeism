##############################################          
#Project: Predication of bike rental count on daily based on the environmental and seasonal settings
#Author: Venkat Deepak Garlapati
#Language: R
###############################################

#Clean R environment
rm(list=ls())

#Installing required libraries
required_lib = c("MLmetrics","ggplot2","ggpubr","GGally","tree","ggcorrplot","outliers","caTools", "rpart","randomForest","inTrees","DMwR")
install.packages(required_lib)
lapply(required_lib,require,character.only = TRUE)

#Using required libraries
library(MLmetrics)
library(ggplot2)
library(ggpubr)
library(GGally)
library(ggcorrplot)
library(outliers)
library(caTools)
library(tree)
library(rpart)
library(DMwR)
library(randomForest)
library(inTrees)
library(ggplotify)

#Set working directory
setwd("C:/Users/Deepak Garlapati/Documents/R")

#Get working directory
getwd()

#Reading Data from a csv file
Bike_Rent_Data = read.csv("day.csv")

#Quickly checking the data in our data set imported
head(Bike_Rent_Data)

#Exploring data
str(Bike_Rent_Data)

#Summarize the data
summary(Bike_Rent_Data)


#########################-------> Missing Value Analysis <----------#######################

#Finding Missing values in data
table(is.na(Bike_Rent_Data))    ## No Missing Values present in the data


######################-----> Univariate Analysis <----------######################

#Understanding our variables with univariate analysis

#Plotting Histograms for the observations using ggplot function.
dev.new()  #Creates new plot

#Plotting Histograms
s_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$season))+
  geom_histogram() + xlab('Season') + ylab('Frequency') + ggtitle("Histogram of Season")
w_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$weathersit))+
  geom_histogram() + xlab('Weather') + ylab('Frequency') + ggtitle("Histogram of Weather")
h_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$hum))+ 
  geom_histogram() + xlab('Humidity') + ylab('Frequency') + ggtitle("Histogram of Humidity")
t_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$temp))+
  geom_histogram() + xlab('Temperature') + ylab('Frequency') + ggtitle("Histogram of Temperature")
at_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$atemp))+
  geom_histogram() + xlab('Feel like Temperature') + ylab('Frequency') + ggtitle("Histogram of Feel like Temperature")
wind_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$windspeed))+
  geom_histogram() + xlab('Windspeed') + ylab('Frequency') + ggtitle("Histogram of Windspeed")
hd_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$holiday))+
  geom_histogram() + xlab('Holiday') + ylab('Frequency') + ggtitle("Histogram of Holiday")
wd_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$workingday))+
  geom_histogram() + xlab('Working day') + ylab('Frequency') + ggtitle("Histogram of Working day")
m_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$mnth))+
  geom_histogram() + xlab('Month') + ylab('Frequency') + ggtitle("Histogram of Month")
y_h = ggplot(Bike_Rent_Data,aes(Bike_Rent_Data$yr))+
  geom_histogram() + xlab('Year') + ylab('Frequency') + ggtitle("Histogram of Year")

#Arrange all the plots above in a single page
ggarrange(s_h,w_h,h_h,t_h,at_h,wind_h,hd_h,wd_h,m_h,y_h,
         ncol = 5, nrow = 2)


############################-----> Bivariate Analysis <----------######################

# Season vs Total no. of Bikes rented 
dev.new()
boxplot(cnt ~ season,data = Bike_Rent_Data ,col="grey", xlab = "Season",
        ylab = "Count of Total rental bike", main = "Season vs Total bikes rented")

# Weather vs Total no. of Bikes rented 
dev.new() 
boxplot(cnt ~ weathersit,data = Bike_Rent_Data ,col="grey",  xlab = "Weather",
        ylab = "Count of Total rental bike", main = "Weather vs Total bikes rented")

# Humidity vs Total no. of Bikes rented
dev.new() 
ggplot(Bike_Rent_Data,aes(x=Bike_Rent_Data$hum,y=Bike_Rent_Data$cnt))+
  geom_point(stat = "identity") + xlab('Humidity') + ylab('Count of Total rental bikes') + ggtitle("Humidity vs Total bikes rented")

# Temperature vs Total no. of Biked rented
dev.new() 
ggplot(Bike_Rent_Data,aes(x=Bike_Rent_Data$temp,y=Bike_Rent_Data$cnt))+
  geom_line(stat = "identity",color = 'orange') + xlab('Normalised Temperature') + ylab('Count of Total rental bikes') + ggtitle("Normalised Temperature vs Total bikes rented")

# #Normalised temperature to Original temperatures
# t_min=-8
# t_max=39 
# t =  Bike_Rent_Data$temp*(t_max-t_min) + t_min
# 
# summary(t)
# 
# #Temperature vs Total. no of Bikes rented
# dev.new() 
# ggplot(Bike_Rent_Data,aes(x=t,y=Bike_Rent_Data$cnt))+
#   geom_line(stat = "identity",color = 'orange') + xlab('Temperature') + ylab('Count of Total rental bikes') + ggtitle("Temperature vs Total bikes rented")


# Feeling like Temperature vs Total no. of Biked rented
dev.new() 
ggplot(Bike_Rent_Data,aes(x=Bike_Rent_Data$atemp,y=Bike_Rent_Data$cnt))+
  geom_line(stat = "identity",color = 'orange') + xlab('Feeling like Normalised Temperature') + ylab('Count of Total rental bikes') + ggtitle("Feeling like Temperature vs Total bikes rented")

#WindSpeed vs Total Bikes rented
dev.new() 
ggplot(Bike_Rent_Data,aes(x=Bike_Rent_Data$windspeed,y=Bike_Rent_Data$cnt))+
  geom_line(stat = "identity",color = 'orange') + xlab('Windspeed') + ylab('Count of Total rental bikes') + ggtitle("Windspeed vs Total bikes rented")

#Holiday vs Total Bikes rented
dev.new() 
ggplot(Bike_Rent_Data,aes(x=Bike_Rent_Data$holiday,y=Bike_Rent_Data$cnt))+
  geom_bar(stat = "identity",color = 'orange') + xlab('Holiday') + ylab('Count of Total rental bikes') + ggtitle("Holiday vs Total bikes rented")

#Working day vs Total Bikes rented
dev.new() 
ggplot(Bike_Rent_Data,aes(x=Bike_Rent_Data$workingday,y=Bike_Rent_Data$cnt))+
  geom_bar(stat = "identity") + xlab('Working Day') + ylab('Count of Total rental bikes') + ggtitle("Working Day vs Total bikes rented")

# Months vs Total Bikes Rented
dev.new() 
boxplot( cnt ~ mnth, data = Bike_Rent_Data , xlab = "Month",
         ylab = "No. of Bikes Rented", main = "Months vs No. of Bikes rented")

dev.new() 
ggplot(Bike_Rent_Data,aes(x=Bike_Rent_Data$mnth,y=Bike_Rent_Data$cnt))+
  geom_bar(stat = "identity",color = 'orange') + xlab('Month') + ylab('Count of Total rental bikes') + ggtitle("Holiday vs Total bikes rented")


# Bike rental Demand over two years
dev.new() 
boxplot( cnt ~ yr, data = Bike_Rent_Data , xlab = "Year",
         ylab = "No. of Bikes Rented", main = "Year vs No. of Bikes rented")


#######################--> Outlier Analysis <------############################

# Detecting outliers
outliers_hum = boxplot(Bike_Rent_Data$hum, 
                       xlab = "Humidity", main = "Boxplot of humidity")$out           #Negative outliers found in hum
outliers_windspeed = boxplot(Bike_Rent_Data$windspeed, 
                             xlab = "Windspeed", main = "Boxplot of Windspeed")$out   #Positive Outliers found in windspeed
outliers_temp = boxplot(Bike_Rent_Data$temp,
                        xlab = "Temperature", main = "Boxplot of Temperature")$out    #No outliers found in temp                                     

# Printing outlier values
outliers_hum
outliers_windspeed
outliers_temp

# Removing Outliers

while((length(outliers_hum) > 0))
{
  Bike_Rent_Data[which(Bike_Rent_Data$hum %in% outliers_hum),]
  Bike_Rent_Data <- Bike_Rent_Data[-which(Bike_Rent_Data$hum %in% outliers_hum),]
  outliers_hum = boxplot(Bike_Rent_Data$hum, xlab = "hum")$out
}

while(length(outliers_windspeed) > 0){
    Bike_Rent_Data[which(Bike_Rent_Data$windspeed %in% outliers_windspeed),]
    Bike_Rent_Data <- Bike_Rent_Data[-which(Bike_Rent_Data$windspeed %in% outliers_windspeed),]  
    outliers_windspeed = boxplot(Bike_Rent_Data$windspeed)$out 
}

#Checking the boxplots for outliers

boxplot(Bike_Rent_Data$hum, 
        xlab = "Humidity", main = "Boxplot of humidity")$out          #No outliers found in hum
boxplot(Bike_Rent_Data$windspeed, 
        xlab = "Windspeed", main = "Boxplot of Windspeed")$out        #No Outliers found in windspeed
boxplot(Bike_Rent_Data$temp,
        xlab = "Temperature", main = "Boxplot of Temperature")$out    #No outliers found in temp
 
# **Hence all the outliers are removed.

#####################-----> Feature Engineering <-----############################

#Converting respective variables into required format

Bike_Rent_Data$season = as.factor(Bike_Rent_Data$season)
Bike_Rent_Data$yr = as.factor(Bike_Rent_Data$yr)
Bike_Rent_Data$mnth = as.factor(Bike_Rent_Data$mnth)
Bike_Rent_Data$holiday = as.factor(Bike_Rent_Data$holiday)
Bike_Rent_Data$weekday = as.factor(Bike_Rent_Data$weekday)
Bike_Rent_Data$workingday = as.factor(Bike_Rent_Data$workingday)
Bike_Rent_Data$weathersit = as.factor(Bike_Rent_Data$weathersit)

Bike_Rent_Data$temp = as.numeric(Bike_Rent_Data$temp)
Bike_Rent_Data$atemp = as.numeric(Bike_Rent_Data$atemp)
Bike_Rent_Data$hum = as.numeric(Bike_Rent_Data$hum)
Bike_Rent_Data$windspeed = as.numeric(Bike_Rent_Data$windspeed)
Bike_Rent_Data$casual = as.numeric(Bike_Rent_Data$casual)
Bike_Rent_Data$registered = as.numeric(Bike_Rent_Data$registered)
Bike_Rent_Data$cnt = as.numeric(Bike_Rent_Data$cnt)

####################-----> Feature Selection <---------###########################

# Correlation between numerical variables
dev.new()
sub_data = data.frame(Bike_Rent_Data$cnt,Bike_Rent_Data$registered,Bike_Rent_Data$casual,Bike_Rent_Data$temp,Bike_Rent_Data$atemp,Bike_Rent_Data$hum,Bike_Rent_Data$windspeed)

corr = cor(sub_data)
ggcorrplot(corr)

# Checking the relationship using pairplot
dev.new()
ggpairs(Bike_Rent_Data[,c('atemp','temp','hum','windspeed','cnt')])


# Instant is just a serial number and hence no significant value for predicting our dependent variable
Bike_Rent_Data$instant = NULL;

# There is a high correlation between temp and atemp, So we can remove anyone of them 
Bike_Rent_Data$atemp = NULL;

# As we can observe from data exploratory analysis, weekday, holiday doesn't constribute much as a independent variable as we have workingday
Bike_Rent_Data$holiday = NULL;
Bike_Rent_Data$weekday = NULL;

#* count = registered + casual, We have count already, so registered and casual gives no independent significance to our model
Bike_Rent_Data$registered = NULL;
Bike_Rent_Data$casual = NULL;

#As we have month and year extracted seperatedly, so the dteday gives no/least additional value to the model
Bike_Rent_Data$dteday = NULL;

### Therefore, We have reduced dimensions of our data from [731,16] to [714,9]


#Relationship between categorical varaiables using ANOVA

cat_var = c("season","yr","mnth","workingday","weathersit")

for(i in cat_var)
{
  Anova_results = aov(Bike_Rent_Data$cnt ~ Bike_Rent_Data[,i], data = Bike_Rent_Data)
  print(summary(Anova_results))}


############### Feature Scaling #############
Bike_Rent_Data$cnt = (Bike_Rent_Data$cnt-min(Bike_Rent_Data$cnt))/(max(Bike_Rent_Data$cnt)-min(Bike_Rent_Data$cnt))




############--------> Model Development <----------##########

############### Linear Regression ###############

# Dividing our data into train and test sets
set.seed(102)
ind = sample.split(Y = Bike_Rent_Data$cnt ,SplitRatio = 0.8)
trainDF = Bike_Rent_Data[ind,]
testDF = Bike_Rent_Data[!ind,]

# Creating a linear regression model
LR_Model = lm(cnt~., data =  trainDF)
summary(LR_Model)

# Prediciting cnt with our linear regression model
LR_predic = predict(LR_Model,testDF)
LR_predic

# Plotting actual values vs predicted values
dev.new()
plot(testDF$cnt, type = "l", col = "orange", main = "Linear Regression")
lines(LR_predic,type = "l", lty=2, col="blue")
legend("topright",legend = c("Actual","Predicted"), col = c("Orange","Blue"),lty=1:2, cex=0.8)

## Finding Accuracy
#R-square - 0.8481099
summary(LR_Model)$r.squared
##RMSE
sqrt(mean((testDF$cnt - LR_predic)^2))
#0.09849352

regr.eval(testDF$cnt,LR_predic,stats = c('mae','rmse','mape','mse'))
#Error rate = 1.99167358
#Accuracy = 80.0833




##################### Decision Tree #######################


#Creating Decision tree Model
Model_DT = rpart(cnt ~ . ,data = trainDF,method = "anova")
summary(Model_DT)

#Predicting cnt with our model
predict_DT = predict(Model_DT,testDF)
predict_DT

# Plotting actual values vs predicted values
dev.new()
plot(testDF$cnt, type = "l", col = "orange", main = "Decison Tree")
lines(predict_DT,type = "l", lty=2, col="blue")
legend("topright",legend = c("Actual","Predicted"), col = c("Orange","Blue"),lty=1:2, cex=0.8)

#Calculation Error Metrics
regr.eval(testDF$cnt,predict_DT,stats = c('mae','rmse','mape','mse'))
#Error rate =  2.9638788
#Accuracy = 70.362100

#Alternative Decision tree method
modelDT = tree(cnt~.,data = trainDF)
summary(modelDT)
modelDT
plot(modelDT)
text(modelDT,pretty = 0)
predictDT = predict(modelDT,testDF)

# test error 
#R-square and RMSE
summary(Model_DT)$r.squared ##
sqrt(mean(testDF$cnt- predict_DT)^2)
#0.009632196

# Plotting
dev.new()
plot(testDF$cnt, type = "l", col = "orange", main = "Alternative Decison Tree")
lines(predictDT,type = "l", lty=2, col="blue")
legend("topright",legend = c("Actual","Predicted"), col = c("Orange","Blue"),lty=1:2, cex=0.8)



################### Random Forest ####################

#Creating Random forest model
model_RF = randomForest(cnt ~ .,trainDF, importance = 500, ntree = 500)

#Checkig the variable importance in the model
varImpPlot(model_RF)
importance(model_RF)
 
#Predicting cnt values with Random Forest Model
predict_RF = predict(model_RF,testDF)

#Plotting actual values vs predicted values
dev.new()
plot(testDF$cnt, type = "l", col = "orange", main = "Random Forest")
lines(predict_RF,type = "l", lty=2, col="blue")
legend("topright",legend = c("Actual","Predicted"), col = c("Orange","Blue"),lty=1:2, cex=0.8)

#Calculation Error Metrics
regr.eval(testDF$cnt,predict_RF,stats = c('mae','rmse','mape','mse'))
#Error rate = 2.28778
#Accuracy = 77.1222

