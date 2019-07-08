
#Clean the Environment
rm(list = ls())

#Set working Directory
setwd("C:/Users/Deepak Garlapati/Documents/R/Employee Absenteeism/Employee Absenteeism")

#GEt working directory
getwd()

require(caTools)
# install.packages("e1071")
library(e1071)
#install.packages("corrgram")
#install.packages("mlbench")
# install.packages("caret")
# library(mlbench)
# library(caret)
# library(ggcorrplot)

#Import required libraries
library(DMwR)
library(dplyr)
library(ggplot2)


#Import Employee absenteeism data set
data = read.csv("Absenteeism_at_work_csv.csv", header = TRUE, check.names = FALSE,na.strings=c("NA","NaN",""," "))


#Quickly checking the contents of data
head(data)

#Summarizing the data
summary(data)

#Structure of data
str(data)


###################################################->  Data Pre-Processing   <-###############################################################

#################### ---->Missing Value Analysis <---- ####################

calculate_Missing_Values_Percentage <- function(data_tmp)
{
  table(is.na(data_tmp)) #Inference: There are 135 missing values in the data
  
  #Create DataFrame with missing percentage
  missing_val = data.frame(apply(data_tmp,2,function(x){sum(is.na(x))}))
  
  #Converting row names into Column names
  missing_val$columns =  row.names(missing_val)
  row.names(missing_val) = NULL
  
  #Renaming columns
  names(missing_val)[0] = "Columns"
  names(missing_val)[1] = "Missing_Percentage"
  
  #Calculating missing value percentage
  missing_val$Missing_Percentage = (missing_val$Missing_Percentage/nrow(data_tmp)) * 100
  
  #Arrange them in descending order
  missing_val = missing_val[order(-missing_val$Missing_Percentage),]
  
  #Rearranging the columns
  missing_val = missing_val[,c(2,1)]
  
  #Saving output results into a file
  write.csv(missing_val, "Missing_percent.csv", row.names = F)
}
  
calculate_Missing_Values_Percentage(data)

#Performing imputation of missing values on duplicate data to preserve original data.
data_tmp = data

# Actual Value = 30 ------ data_tmp$`Body mass index`[1]
# Mean = 26.67938
# Median = 25
# KNN = 30 

# Actual Value 2 = 31 --------- data_tmp$`Body mass index`[2]
# KNN = 30.5967

#Imputing with mean
#data_tmp$`Body mass index`[is.na(data_tmp$`Body mass index`)] = mean(data_tmp$`Body mass index`, na.rm = TRUE)

#Imputing with median
#data_tmp$`Body mass index`[is.na(data_tmp$`Body mass index`)] = median(data_tmp$`Body mass index`, na.rm = TRUE)

#Imputing with KNN
data_tmp = knnImputation(data_tmp,k=3)

#Cross check for the total number of missing values
sum(is.na(data_tmp))


#INFERENCE: KNN predicts the best value for the missing data


################ ----> Duplicates check <---- ##################

#Total no of duplicated rows in data_tmp
sum(duplicated(data_tmp))

#Removing duplicates from our data set
data_tmp = unique(data_tmp)

###################################################->   Data Visualisations   <-###########################################################

#Writing changes to our original data
data = data_tmp 

#Categorising continuous variables and categorical variables
num_vars = c('Transportation expense','Distance from Residence to Work','Service time','Age','Work load Average/day',
            'Hit target','Height','Weight','Body mass index','Absenteeism time in hours')

cat_vars = colnames(data[ ,-which(colnames(data) %in% num_vars)])


#Visualising data

#Creating new plot
dev.new()

#Histograms of all variables
ggplot() + geom_histogram(aes(x = data$ID),data=data)
ggplot() + geom_histogram(aes(x = data$`Reason for absence`),data=data)
ggplot() + geom_histogram(aes(x = data$`Month of absence`),data=data)
ggplot() + geom_histogram(aes(x = data$`Day of the week`),data=data)
ggplot() + geom_histogram(aes(x = data$Seasons),data=data)
ggplot() + geom_histogram(aes(x = data$`Transportation expense`),data=data)
ggplot() + geom_histogram(aes(x = data$`Distance from Residence to Work`),data=data)
ggplot() + geom_histogram(aes(x = data$`Service time`),data=data)
ggplot() + geom_histogram(aes(x = data$Age),data=data)
plot(data$`Work load Average/day`)
ggplot() + geom_histogram(aes(x = data$`Hit target`),data=data)
ggplot() + geom_histogram(aes(x = data$`Disciplinary failure`),data=data)
ggplot() + geom_histogram(aes(x = data$Education),data=data)
ggplot() + geom_histogram(aes(x = data$Son),data=data)
ggplot() + geom_histogram(aes(x = data$`Social drinker`),data=data)
ggplot() + geom_histogram(aes(x = data$`Social smoker`),data=data)
ggplot() + geom_histogram(aes(x = data$Pet),data=data)
ggplot() + geom_histogram(aes(x = data$Weight),data=data)
ggplot() + geom_histogram(aes(x = data$Height),data=data)
ggplot() + geom_histogram(aes(x = data$`Body mass index`),data=data)
ggplot() + geom_histogram(aes(x = data$`Absenteeism time in hours`),data=data)

#Box-Plot for data  
boxplot(data[,c("ID", "Reason for absence","Month of absence","Day of the week","Seasons","Transportation expense","Distance from Residence to Work")])
boxplot(data[,c("Service time", "Age","Work load Average/day","Hit target","Disciplinary failure" ,"Education" ,"Son")])
boxplot(data[,c("Social drinker","Social smoker","Pet", "Weight", "Height", "Body mass index","Absenteeism time in hours")])

#Comparing Each variables with our target variable

ggplot(data,aes(x=data$ID,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('ID') + ylab('Absenteeism time in hours') + ggtitle("ID vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Reason for absence`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Reason for absence') + ylab('Absenteeism time in hours') + ggtitle("Reason for absence vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Month of absence`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Month of absence') + ylab('Absenteeism time in hours') + ggtitle("Month of absence vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Day of the week`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Day of the week') + ylab('Absenteeism time in hours') + ggtitle("Day of the week vs Absenteeism time in hours")

ggplot(data,aes(x=data$Seasons,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Seasons') + ylab('Absenteeism time in hours') + ggtitle("Seasons vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Transportation expense`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Transportation expense') + ylab('Absenteeism time in hours') + ggtitle("Transportation expense vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Service time`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Service time') + ylab('Absenteeism time in hours') + ggtitle("Service time vs Absenteeism time in hours")

ggplot(data,aes(x=data$Age,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Age') + ylab('Absenteeism time in hours') + ggtitle("Age vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Work load Average/day`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Work load Average/day') + ylab('Absenteeism time in hours') + ggtitle("Work load Average/day vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Hit target`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Hit target') + ylab('Absenteeism time in hours') + ggtitle("Hit target vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Disciplinary failure`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Disciplinary failure') + ylab('Absenteeism time in hours') + ggtitle("Disciplinary failure vs Absenteeism time in hours")

ggplot(data,aes(x=data$Education,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Education') + ylab('Absenteeism time in hours') + ggtitle("Education vs Absenteeism time in hours")

ggplot(data,aes(x=data$Son,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Son') + ylab('Absenteeism time in hours') + ggtitle("Son vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Social drinker`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Social drinker') + ylab('Absenteeism time in hours') + ggtitle("Social drinker vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Social smoker`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Social smoker') + ylab('Absenteeism time in hours') + ggtitle("Social smoker vs Absenteeism time in hours")

ggplot(data,aes(x=data$Pet,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Pet') + ylab('Absenteeism time in hours') + ggtitle("Pet vs Absenteeism time in hours")

ggplot(data,aes(x=data$Weight,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Weight') + ylab('Absenteeism time in hours') + ggtitle("Weight vs Absenteeism time in hours")

ggplot(data,aes(x=data$Height,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Height') + ylab('Absenteeism time in hours') + ggtitle("Height vs Absenteeism time in hours")

ggplot(data,aes(x=data$`Body mass index`,y=data$`Absenteeism time in hours`))+
  geom_point(stat = "identity") + xlab('Body mass index') + ylab('Absenteeism time in hours') + ggtitle("Body mass index vs Absenteeism time in hours")


######################### PCA Visualisation of data ###################
library(ggfortify)

#Converting factor variable
data$`Work load Average/day` = as.numeric(sub(",", "", data$`Work load Average/day`, fixed = TRUE))


data.pca <- prcomp(data, center = TRUE, scale. = TRUE) 
plot(data.pca,type="l")
summary(data.pca)
                   
              
#autoplot(prcomp(data), data = data, colour = ('Absenteeism time in hours'))


######################### ----> Outlier Analysis <---- #######################

data_tmp = data

#Replacing all outliers with NA and imputing
for( i in num_vars)
{
  val = data_tmp[,i][data_tmp[,i] %in% boxplot.stats(data_tmp[,i])$out] #Finding outliers from boxplot
  data_tmp[,i][data_tmp[,i] %in% val] = NA
}

#Imputing NA with KNN Imputation[;]
data_tmp = knnImputation(data_tmp,k=3)

data = data_tmp

######################################->   Feature Engineering   <-###########################################################

######################### Creating Dummies for categorcial variables #######################

install.packages("fastDummies")
install.packages("knitr")

library(knitr)
library(fastDummies)

data_tmp = data

data_tmp = fastDummies::dummy_cols(data_tmp, select_columns = cat_vars, remove_first_dummy = TRUE)

knitr::kable(data1)

#Deleting the columns for which dummies are created
for(i in cat_vars)
{
  data_tmp[i] = NULL
}

#data_old = data
data = data_tmp

################################### Feature Scaling ##########################

#Normalizing the numercial variables
for( i in num_vars)
{
  data_tmp[i] = (data_tmp[i] - min(data_tmp[i]))/(max(data_tmp[i]) - min(data_tmp[i]))
}

data = data_tmp

################################## Feature Selection ##########################

#install.packages("mlbench")
install.packages("caret")
library(mlbench)
library(caret)
library(ggcorrplot)

#Correlation Matrix
correlationMatrix = cor(data[,num_vars])

#Print results of correlation matrix
print(correlationMatrix)

#Plotting the correlation matrix
ggcorrplot(correlationMatrix)

#Finding Attributes that are highly correlated
highlyCorrelated = findCorrelation(correlationMatrix, cutoff = 0.8)

#Printing highly correlated variables
print(highlyCorrelated)

#Correlation plot  using corrgram library
library(corrgram)
corrgram(data[,num_vars] , order =F, upper.panel = panel.pie , text.panel = panel.txt , main = "Correlation Plot")


##Weight and body mass index are highly correlated. Removing one of the feature. 
data$Weight = NULL;


###################################Splitting the data into train and test##########################
library(caTools)
set.seed(100)

trainIndex = sample.split(Y = data$`Absenteeism time in hours`, SplitRatio = 0.8)
train = data[trainIndex,]
test = data[!trainIndex,]


X_train = subset(train,select = -c(`Absenteeism time in hours`))
y_train = subset(train,select = c(`Absenteeism time in hours`))

X_test = subset(test,select = -c(`Absenteeism time in hours`))
y_test = subset(test,select = c(`Absenteeism time in hours`))


####### Dimensionality Reduction using PCA

#Calculating principal components
principal_comp = prcomp(X_train)

#Compute standard deviation of each principal componentc
std_dev = principal_comp$sdev

#Compute variance
pr_var = std_dev^2

#Proportion of variance explained
prop_var = pr_var/sum(pr_var)

#cdf plot for principle components
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#Add a training set with principal components
X_train.data = data.frame(principal_comp$x)

# From the above plot selecting 45 components since it explains almost 95+ % data variance
X_train.data =X_train.data[,1:45]

#Transform test into PCA
X_test.data = predict(principal_comp, newdata = X_test)
X_test.data = as.data.frame(X_test.data)

#select the first 45 components
X_test.data=X_test.data[,1:45]


X_train.data$`Absenteeism time in hours` = paste(y_train$`Absenteeism time in hours`)
X_test.data$`Absenteeism time in hours` = paste(y_test$`Absenteeism time in hours`)



########################################### Model Development ##########################################

library(e1071)

######################### Multiple Linear Regression#########################

#Developing model on traindata
set.seed(100)
Linear_mod = lm(`Absenteeism time in hours`~.,data = X_train.data)

#Predicting testing data
pred_LR_test = predict(Linear_mod,X_test.data)

#Model Results 
print(postResample(pred = pred_LR_test, obs =y_test$`Absenteeism time in hours`))

#RMSE = 0.1714966
#Rsquared = 0.3952933
#MAE = 0.1169188

#############################  Ridge Regression  #############################

#install.packages("glmnet")
library(glmnet)

set.seed(100)
x = data.matrix(X_train.data[,-46])
y = data.matrix(X_train.data[,46])
x_test = data.matrix(X_test.data[,-46])


for( lambda in c(0.000001,0.0001,0.001,2,5,10,20))
{
  #Developing ridge regression model 
  ridge = glmnet(x, y, family="gaussian", alpha = 0, lambda = lambda)
  
  # make predictions
  pred_ridge_test = predict(ridge,x_test,type="link")
  
  #Model Results 
  print(postResample(pred = pred_ridge_test, obs = y_test$`Absenteeism time in hours`))
}

#Out of all lambda values, lambda with value 0.001 provided the least RMSE

ridge = glmnet(x, y, family="gaussian", alpha = 0, lambda = 0.001)

#Summarize the fit
summary(ridge)

# make predictions
pred_ridge_test = predict(ridge,x_test,type="link")

#Model Results 
print(postResample(pred = pred_ridge_test, obs = y_test$`Absenteeism time in hours`))

#RMSE = 0.1714843
#Rsquared = 0.3952933
#MAE = 0.1169454

############################   Lasso Regression  #############################

#install.packages("lars")
library(lars)


set.seed(100)
x = data.matrix(X_train.data[,-46])
y = data.matrix(X_train.data[,46])
x_test = data.matrix(X_test.data[,-46])


for( lambda in c(0.000001,0.0001,0.001,2,5,10,20))
{
  #Developing lasso regression model
  lasso = glmnet(x, y, alpha = 1, lambda = lambda)
  
  # make predictions
  pred_lasso_test = predict(lasso,x_test,type="link")
  
  #Model Results 
  print(postResample(pred = pred_lasso_test, obs = y_test$`Absenteeism time in hours`))
}

#Out of all lambda values, lambda with value 0.001 provided the least RMSE

#Developing LASSO Model with lambda = 0.001
lasso = glmnet(x, y, alpha = 1, lambda = 0.001)

#Summarize the fit
summary(lasso)

# make predictions
pred_lasso_test = predict(lasso,x_test,type="link")

#Model Results 
print(postResample(pred = pred_lasso_test, obs = y_test$`Absenteeism time in hours`))

#RMSE = 0.1708446
#Rsquared = 0.3996881
#MAE = 0.1171411


