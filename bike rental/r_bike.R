setwd("D:\\edwisor\\project_bike _count")
day_bike=read.csv("day.csv")
summary(day_bike)
#checking data type of columns
sapply(day_bike, class)
#converting data type of dteday_bike into datetime
day_bike$weathersit = as.factor(day_bike$weathersit)
day_bike$season = as.factor(day_bike$season)
day_bike$dteday = as.character(day_bike$dteday)
day_bike$mnth = as.factor(day_bike$mnth)
day_bike$weekday = as.factor(as.character(day_bike$weekday))
day_bike$workingday = as.factor(as.character(day_bike$workingday))
day_bike$yr = as.factor(day_bike$yr)
day_bike$holiday = as.factor(day_bike$holiday)
#MISSING VALUE analysis
missing_val = sapply(day_bike, function(x){sum(is.na(x))})
#No missing values
#finding corelation value of numerical data
library(corrplot)
library(RColorBrewer)
data=Filter(is.numeric, day_bike)
corr <-cor(data)
corrplot(corr, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
#it can be infered from curve as well as correlation matrix and plot that temp and atemp has strong relationship so oner should be dropped
#since casual and registered are to be predicted so we need to drop that also
day_bike=within(day_bike, rm(temp,casual,registered))
##dropping instant also as it is not needed
day_bike=within(day_bike, rm(instant))
target=subset(day_bike)

# creating scatterplot of numerical variables with cnt
#install.packages('ggplot2')
library(ggplot2)
scat1 = ggplot(data = day_bike, aes(x =atemp, y = cnt)) + ggtitle("absolute Temperature") + geom_point() + xlab("absolute_Temperature") + ylab("Bike_Count")
scat2 = ggplot(data = day_bike, aes(x =hum, y = cnt)) + ggtitle(" Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike_Count")

scat3 = ggplot(data = day_bike, aes(x =windspeed, y = cnt)) + ggtitle(" Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike _Count")
gridExtra::grid.arrange(scat1,scat2,scat3,ncol=2)
#creating bar plot of categorical_variable
season_bar = ggplot(data = day_bike, aes(x = season)) + geom_bar() + ggtitle("Season_count")
weathersit_bar = ggplot(data = day_bike, aes(x = weathersit)) + geom_bar() + ggtitle(" Weather_count")
holiday_bar = ggplot(data = day_bike, aes(x = holiday)) + geom_bar() + ggtitle("Holiday_count")
workingday_bar = ggplot(data = day_bike, aes(x = workingday)) + geom_bar() + ggtitle("Working day_count")
# ## Plotting plots together
gridExtra::grid.arrange(season_bar,weathersit_bar,holiday_bar,workingday_bar ,ncol=3)
###drawing boplot to get outliers.
boxplot(day_bike$hum)
boxplot(day_bike$windspeed)
boxplot(day_bike$atemp)
##outlier removal of hum and windspeed
outlier = day_bike[,'hum'][day_bike[,'hum'] %in% boxplot.stats(day_bike[,'hum'])$out]
day_bike = day_bike[which(!day_bike[,'hum'] %in% outlier),]
boxplot(day_bike$hum)
outlier = day_bike[,'windspeed'][day_bike[,'windspeed'] %in% boxplot.stats(day_bike[,'windspeed'])$out]
day_bike = day_bike[which(!day_bike[,'windspeed'] %in% outlier),]
boxplot(day_bike$windspeed)
#checking feature importance
library(caret)
library(Boruta)
boruta = Boruta(cnt~., data = na.omit(day_bike), doTrace = 2)
# removing unwanted variables
day_bike=within(day_bike, rm(holiday,dteday))
#split data into train and test
#Divide the data into train and test
index = sample(1:nrow(day_bike), 0.75 * nrow(day_bike))
train_data = day_bike[index,]
test_data = day_bike[index,]


#random forest
randomforest_model = randomForest(cnt~., data = train_data, ntree = 500)

#Predict 
preds = predict(randomforest_model, test_data[,-10])

#Create dataframe 
df_mat = cbind(preds)
head(df_mat)

#Calculate MAPE
x1=regr.eval(trues = test_data[,10], preds = preds, stats = c("mae","mse","rmse","mape"))


########################################LINEAR REGRESSION########################################
#MAPE: 24%
#RMSE: 315.6434
#Accuracy:  0.8574
#MAE: 225.2275 
#Adjusted R squared: 0.8498
#F-statistic: 112.9

#Train the data using linear regression
linear_regression = lm(formula = cnt~., data = train_data)

#summary of the model
summary(linear_regression)

#Predict 
predictions = predict(linear_regression, test_data[,-10])

#Create dataframe 
df_mat = cbind(df_mat,predictions)
head(df)

#MAPE
x2=x1=regr.eval(trues = test_data[,10], preds = preds, stats = c("mae","mse","rmse","mape"))
