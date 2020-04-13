#clear previously available list 
rm(list = ls())
#Install required packages for implement of Bike renting prediction  
#Load Libraries
#we have to store vector in x object 
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", "sampling", "DataCombine", "dplyr","plyr","reshape","data.table","GGally","usdm")
#we have install packages using of command install.packages()
install.packages(x)
##################################################################################
#Require library to implement project
#library short description 
# ggplot2 library is a system for declaratively creating graphics, based on The Grammar of Graphics
library("ggplot2")
#corrgram is Calculates correlation of variables and displays the results graphically
library("corrgram")
#Functions and data for "Data Mining with R"
library("DMwR")
#This package provides a number of functions for exploring the impact of different sources of uncertainties (e.g.positional uncertainty) 
#on performance of species distribution models (SDMs).
library('usdm')
#The caret package (short for Classification And REgression Training) contains functions to streamline the 
#model training process for complex regression 
library("caret")
#Library  random forest algorithm works by aggregating the predictions made by multiple decision trees of varying depth.
library("randomForest")
#The R package unbalanced implements some well-known techniques for unbalanced classification tasks and 
#provides a racing strategy to adaptively select the best methods for a given dataset
library("unbalanced")
#The C50 package contains an interface to the C5.0 classification model
library("C50")
#Title Create dummy/indicator variables flexibly and efficiently.
library("dummies")
# e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071)
library("e1071")
#The Information package is specifically designed to perform data exploration by producing 
#easy-to-read tables and graphs
library("Information")
#MASS: Support Functions and Datasets for Venables and Ripley's MASS
library("MASS")
#rpart package keeps track of something called the complexity of a tree. The complexity measure is a combination of the size of 
#a tree and the ability of the tree to separate the classes of the target variable.
library("rpart")
#gbm package in R to fit gradient boosting model.
library("gbm")
#Generation of synthetic data by Randomly Over Sampling Examples (ROSE)
library("ROSE")
#In sampling module sample takes a sample of the specified size from the elements of 
#x using either with or without replacement.
library("sampling")
#Using of Data combine module A function for filling in missing values of a variable from one 
#data frame with the values from another variable
library("DataCombine")
# Dplyr aims to provide a function for each basic verb of data manipulation.
library("dplyr")
#plyr is an R package that makes it simple to split data apart, do stuff to it, and mash it back together. 
#This is a common data-manipulation step.
library("plyr")
#Different functions require different formats, and so the need to reshape a dataset.
library("reshape")
#Data. table is an extension of data. frame package in R. It is widely used for fast aggregation of large datasets, 
#low latency add/update/remove of columns, quicker ordered joins, and a fast file reader.
library("data.table")
#'GGally' module extends 'ggplot2' by adding several function or plot
library("GGally")
#########################################################################################

#get Working directory
#using of get function we check current working diroctory
getwd()
#Set working directory
#Using of set function we set our working diroctory
setwd("C:/Users/ASUS/Desktop")

#Load Day dataset 
Bike=read.csv("Day.csv",header =TRUE)
#Using of head function we get  five observation in our  Bike dataset
head(Bike)
#In Bike dataset available 16 variable 
#In this variable 15 are indepandent variable or cnt is target variable 
#Using of tail fucntion we get last five observation in our Bike dataset
tail(Bike)
#Measures of centrality are mean, median. Measures of spread are 
#standard deviation, quantiles, min and max, range, interquartile range.
#Using of summary function we get  description of Bike object
summary(Bike)
#str() It provides great information about the structure of some object.
#str() function is provide great information of Object 
str(Bike)
###################################################################################
#Univariate Analysis# 

#Univariate analysis is the simplest form of analyzing data. "Uni" means "one", 
#so in other words your data has only one variable. It doesn't deal with causes 
#or relationships (unlike regression) and it's major purpose is to describe; 
#it takes data, summarizes that data and finds patterns in the data

#glimpse function is helpful for a first inspection of the data set at hand.
glimpse(Bike)

#Univariate analysis in Numeric variable 

#Analyze the distribution of  target variable "cnt"
hist(Bike$cnt,right = FALSE,col = "blue")
#Above histogram graphical cnt variable show normal distribution

#Analyze the distribution of indepandent variable "casual" 
hist(Bike$casual,right = F,col = "blue")
#Aboove histogram graph showing casual variable is not normaly distributed and 
#chances of outlier is high in this variable

#Analyze the distribution of indepandent variable "temp"
hist(Bike$temp,right = F,col = "blue")

#Aboove histogram graph showing temp variable is normaly distributed 

#Analyze the distribution of indepandent variable "registered"
hist(Bike$registered,right = F,col = "blue")
#Aboove histogram graph showing ragistered variable is normaly distributed

#Analyze the distribution of indepandent variable "hum"
hist(Bike$hum,right = F,col = "blue")
#Aboove histogram graph showing hum variable is normaly distributed

####################################################################################
#Categorical variable Univariaite analysis

# Visualize categorical Variable "mnth" with target variable "cnt"
#Change x varable factor catogroy using of as.factor function 
#using of fill function we fill color in our plot
ggplot(Bike, aes(x=as.factor(mnth), y=cnt),fill="blue") + 
  stat_summary(fun="mean", geom="bar",color="red")
#Using of aes() aesthatic function we fill color visualize plot just like (boxplot,histogram)
#These visual caracteristics are known as aesthetics (or aes) and include: 
#color and fill. points shape.
ggplot(Bike)+
  geom_histogram(aes(x=cnt,y=..density..),
                 fill= "red")+
  geom_density(aes(x=cnt,y=..density..))

# Visualize categorical Variable 'holiday' 

ggplot(Bike) +
  geom_bar(aes(x=holiday),fill="red")
#Number of count is very high in holiday time renting bike 
# it is showing that  the mostly cycle rentals are happening  on holidays

# Visualize categorical Variable 'weekday' 

ggplot(Bike) +
  geom_bar(aes(x=weekday),fill="red")
#Above plot is showing number of counts is same in all weekdays

# Visualize categorical Variable 'weathersit' 
ggplot(Bike) +
  geom_bar(aes(x=weathersit),fill="blue") 
#Number of count is very high in whether is "clear"
#Number of count is average if whether "Few clouds"
#Number of count is less if whwther is  "Partly cloudy, Partly cloudy" 

#################################################################################
#Bivariate Analysis

#Bivariate relationship in numeric variable 
#Firt we check Bivariate relationship "temp" and "hum" variable 
#Using of geom_pointThe point geom is used to create scatterplots.
#geom_smooth understands the following aesthetics (required aesthetics are in bold):
#x,y,alpha,colour,fill,linetype,size,weight
ggplot(Bike, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()
#It is showing  graph  humadity increasing rapidly till 0.65 after after the 
#have decressing gredualy


#we check Bivariate relationship "temp" and "atemp" variable 
ggplot(Bike, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()
#It is showing graph "temp" or "atemp" varaible is strongly positive bivariate relationship
#temp or atemp variable is highly positive correlated

#we check Bivariate relationship "temp" and "windspeed" variable 
ggplot(Bike, aes(x= temp,y=windspeed)) +
  geom_point()+
  geom_smooth()
#It is showing graph "temp" or "windspeed" varaible is less negative correlation
#tempraure is increases thus time windspeed is decreass

#######################################################################

#Checking relationship between all numeric variable
#We check  thus numeric relationship using of pair plot 
ggpairs(Bike[,c('atemp','temp','hum','windspeed','cnt')])
#ggpairs  take some time to plot
#Above plot is showing positive correlation atemp or target variable cnt
#plot is showing positive correlation temp or target variable cnt
#plot is showing negative correlation hum or target variable cnt
#plot is showing positive correlation windspeed or target variable cnt

##########################################################################

#Catogirical Bivariate Relationship

#check relationship between  season and holiday
#table() performs categorical tabulation of data with the variable and its frequency.
rel_session_holiday= table(Bike$season,Bike$holiday)
#In barplot function We can supply a vector or matrix to this function. 
barplot(rel_session_holiday)

#If a day is neither weekend nor holiday is 1, otherwise is 0.  
#Here chances of renting working day is less or chances or renting 
# holiday is very high

#check relationship between  season and weekand
rel_season_weekday=table(Bike$season,Bike$weekday)
barplot(rel_season_weekday)
#weekday bike renting chancs all day are same

#Check relationship between season and weathersit
rel_season_weathersit=table(Bike$season,Bike$weathersit)
rel_season_weathersit
barplot(rel_season_weathersit)
#weathersit is three catigory 
# Clear, Few clouds, Partly cloudy
#Bike rental is very high in clear wether 
#Bike rental is normal in Few cloud season wether 
#Bike rental is less in partly cloudy wether

##############################################################################


#Missing Value Analysis 
# missing values, occur when no data value is stored for the variable in an observation
#WE find missing value in using of is.na function
#We calculate all missing value using of sum function
missing_value = data.frame(apply(Bike,2,function(x){sum(is.na(x))}))
missing_value$columns=row.names(missing_value)
#Object
missing_value
names(missing_value)[1] =  "Missing_value_percentage"
#Using of names function we define column name 
missing_value$Missing_value_percentage = (missing_value$Missing_value_percentage/nrow(Bike)) * 100
#Above formula get percentage of missing value in our variable 
missing_value = missing_value[order(-missing_value$Missing_value_percentage),]
row.names(missing_value) = NULL
missing_value = missing_value[,c(2,1)]

missing_value
 #In our Bike dataset is not available missing value   
 
########################################################################

#Outlier Analysis
#we perform outlier Analysis in numeric variable 
#Allready we done univariate analysis 
#The chances of getting outlier more in "casual","ragistered"or cnt variable 
#Outlier analysis in registered variable 
boxplot(Bike$registered,col = "yellow")
#plot  showing there is not available outlier value

#Outlier Analysis in "casual" variable 
boxplot(Bike$casual,col = "yellow")
#plot showing outlier available in casual variable 

#Outlier Analysis in "cnt" variable 
boxplot(Bike$cnt,col = "yellow")
#plot showing outlier not available in cnt variable

#############################################################################

#Handling outliers
#Remove outlier using of Boxplot method
df=Bike
#Bike = df 
#We remove outliers in "casual" variable
value=Bike$casual[Bike$casual%in% boxplot.stats(Bike$casual)$out]
Bike=Bike[which(!Bike$casual %in% value),]

#Now our Casual variable not available outliers
boxplot(Bike$casual,col = "red")
#Boxplot showing not available outliers in casual variable 
#Before outliers number of bservation is 731 but after removing outliers observation is 687

###################################################################################
#Feature selection 
#Correlation Analysid
#we are perform correlation analysis using of corrgram library
corrgram(Bike[,c("temp","atemp","hum","windspeed","registered" ,'cnt')], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#Blue color show highly positive correlationship between variable 
#Correlation plot is showing temp and atemp variable is highly correlated 
#"hum" variable or target variable "cnt" not available relationship betweent them

#####################################################################################


#Reduction of variable the are highly correlated or not related
#we know that "temp" or "atemp" variable is highly correlated "hum" variable is not related with target variable "cnt"
#We are remove here "atemp" or "hum" variable

Bike_features = subset(Bike,select=-c(atemp,hum))
View(Bike_features)
#our Bike dataset now available 687 observation or 14 variable 

########################################################################################

#Normalisation
#OUR DATASET  most numeric variable is normalization form 
#Normalized variable is "temp","atemp","hum","windspeed"
#so we perform normalisation "casual" or "registered" variable

cnames = c("casual","registered")
for(i in cnames){
  print(i)
  Bike_features[,i] = (Bike_features[,i] - min(Bike_features[,i]))/
    (max(Bike_features[,i] - min(Bike_features[,i])))
}
Bike$casual
Bike$registered

#########################################################################################

#Model development
#Bike dataset available variable we store columns name object
columns=c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")

#Divide data into train and test using stratified sampling method
#set.seed function in R is used to reproduce results i.e. it produces the same sample again and again. When we generate randoms numbers without set.seed()
#function it will produce different samples at different time of execution.
set.seed(1234)

#we split data intoo traing and test dataset 
#train dataset available 80% of data
#test dataset available 20% of data 
#we are using to perform operation createDatPartion() funtion the are available in caret() library
train.index = createDataPartition(Bike_features$cnt, p = .80, list = FALSE)
train = Bike_features[ train.index,]
test  = Bike_features[-train.index,]
train_feature = train[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
#train dataset
train_feature

test_features = test[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
#test dataset 
test_features

######################################################################################

#Decision Tree Regressor
#our dataset target variable is based on continuos prediction so we select Regresssion Algorithm 
#rpart of regression
#rpart keeps track of something called the complexity of a tree. The complexity measure is a combination of the size of
#a tree and the ability of the tree to separate the classes of the target variable
fit = rpart(cnt ~ ., data = train_feature, method = "anova")


#Predict for new test cases
DT_predictions = predict(fit, test_features[,-12])

print(fit)

DT_predictions
plot(DT_predictions,col="blue")
#  plotting decision tree
#using of pair function The	function	'par'	can	be	used	to	manipulate	
#the	current	plot( ) . pair function allow multiple plot in on one figure
#cex:	for	legends	and	other	independent	functions	
#outside	of	the	'plot'	family	
par(cex= 0.8)
#plot fit object 
plot(fit)
#print text in regression decision tree 
text(fit)

##############################################################################

#Decision Tree Model Evaluation 

#Root Mean Squared Error
#(RMSE): RMSE is a quadratic scoring rule that also measures the average magnitude of the error.
#Evaluate  Model using RMSE
#make RMSE function first the evaluate RMSE formula
RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}
RMSE(test_features[,12], DT_predictions)

#RMSE= 453.6728

##################################################################

#MAPE Mean Absolute Percentage Error
#MAPE:is a measure of prediction accuracy of a forecasting method in statistics
#First we create MAPE function
#calculate MAPE
MAPE = function(y, y_true){
  mean(abs((y - y_true)/y))
  }

MAPE(test_features[,12], DT_predictions)
#error rate = 0.1037541
#Accuracy of model is 90.63 

#######################################################################################

#train or test dataset and selection of variable 
train = Bike_features[ train.index,]
test  = Bike_features[-train.index,]
train_feature = train[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
#train dataset
train_feature

test_features = test[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
#test dataset 
test_features



###################################################################################

#Random Forest Model
#In the random forest approach, a large number of decision trees are created. 
#Every observation is fed into every decision tree.
RF_model=randomForest(cnt~.,data=train_feature,ntree=100)
RF_model
#Random forest model for train_feature dataset 
plot(RF_model,col="red")
#plot of random forest model 
#prediction of test case
RF_predictions = predict(RF_model, test_features[,-12])
RF_predictions
plot(RF_predictions,col="red")
#predicted value in test_feature dataset

####################################################################################

#Evaluation of Random Forest algorithm 

#Root Mean Squared Error
#RMSE 
RMSE(test_features[,12], RF_predictions)
#RMSE=207.7042
#MAPE Mean Absolute Percentage Error
MAPE(test_features[,12], RF_predictions)
#Error Rate :0.04592139
#Accuracy Rate :96.5 

#########################################################################################

#train or test dataset and selection of variable 
train = Bike_features[ train.index,]
test  = Bike_features[-train.index,]
train_feature = train[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
#train dataset
train_feature

test_features = test[,c("season" ,"yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
#test dataset 
test_features


#Linear Regression Model

#Variance Inflation Factor
#Calculates the variation inflation factors of all predictors in regression model
vif(train_feature[,-12])
vifcor(train_feature[,-12], th = 0.9)
# Correleation between two variables is 'season' and 'mnth' is 0.82 so,
#removing one variable from the model

#column variable name train dataset
train_feature = train[,c("yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
#Column variable name test dataset 
test_features = test[,c("yr" ,"mnth","holiday","weekday","workingday","weathersit","temp","windspeed","casual","registered","cnt")]
# the lm(), or "linear model," function can be used to create a simple regression model
LR_model = lm(cnt ~., data = train_feature)
print(LR_model)

#Predictions of Linear Regression Model
LR_predictions = predict(LR_model, test_features[,-11])
print(LR_predictions)
plot(LR_predictions,col="red")

################################################################################

#Evaluation of Linear Regression Model
##Root Mean Squared Error
#RMSE
RMSE(test_features[,11], LR_predictions)
#RMSE 1.591802e-12

##MAPE Mean Absolute Percentage Error
#MAPE
MAPE(test_features[,11], LR_predictions)
#Accuracy :99.9 +Accuracy
#Error rate : 1.065695e-15
#################################################################################

#Conclusion for Bike dataset Linear Regression model is a best model and give
#approximately 99.9 percent accuracy for Bike rental count on daily based 