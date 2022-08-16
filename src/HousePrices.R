setwd("/Users/timstaudinger/Desktop/Repository.nosync/MachineLearning-Exam/dataset")
data <- read.csv("housing.csv",header=TRUE,sep=",",fill=TRUE,stringsAsFactors=TRUE)
data[1:5,]

# Check data type and summary
data[,"ocean_proximity"] <- as.factor(data[,"ocean_proximity"])
summary(data)

# Handle missing values in "total_bedrooms" (median)
data$total_bedrooms[is.na(data$total_bedrooms)] <- median(data$total_bedrooms, na.rm=TRUE)


# ------------------------------------------------------------------------------
# Analyse original data set 
# Graphical plots
# Histograms
par(mfrow = c(4, 3))
hist(data$longitude, main = "longitude", col="slateblue1", breaks = 60)
hist(data$latitude, main = "latitude", col="slateblue1", breaks = 60)
hist(data$housing_median_age, main = "housing_median_age", col="slateblue1", breaks = 60)
hist(data$total_rooms, main = "total_rooms", col="slateblue1", breaks = 60)
hist(data$total_bedrooms, main = "total_bedrooms", col="slateblue1", breaks = 60)
hist(data$population, main = "population", col="slateblue1", breaks = 60)
hist(data$households, main = "households", col="slateblue1", breaks = 60)
hist(data$median_income, main = "median_income", col="slateblue1", breaks = 60)
hist(data$median_house_value, main = "median_house_value", col="slateblue1", breaks = 60)
barplot(height=c(9136, 6551, 5, 2290, 2658),names.arg=c("<1H OCEAN","INLAND","ISLAND",
                                                        "NEAR BAY", "NEAR OCEAN"), main="ocean_proximity", col="slateblue1")

# Box-plots
par(mfrow = c(2, 5))
boxplot(data$longitude, main = "longitude", col="slateblue1")
boxplot(data$latitude, main = "latitude", col="slateblue1")
boxplot(data$housing_median_age, main = "housing_median_age", col="slateblue1")
boxplot(data$total_rooms, main = "total_rooms", col="slateblue1")
boxplot(data$total_bedrooms, main = "total_bedrooms", col="slateblue1")
boxplot(data$population, main = "population", col="slateblue1")
boxplot(data$households, main = "households", col="slateblue1")
boxplot(data$median_income, main = "median_income", col="slateblue1")
boxplot(data$median_house_value, main = "median_house_value", col="slateblue1")

# Scatter-plots
par(mfrow = c(1, 1))
plot(data, col="slateblue1")

# Check for connections between the variables
par(mfrow = c(2,5))
plot(median_house_value ~ longitude, data=data,pch=19, col="slateblue1")
plot(median_house_value ~ latitude, data=data,pch=19, col="slateblue1")
plot(median_house_value ~ housing_median_age, data=data,pch=19, col="slateblue1")
plot(median_house_value ~ total_rooms, data=data,pch=19, col="slateblue1")
plot(median_house_value ~ total_bedrooms, data=data,pch=19, col="slateblue1")
plot(median_house_value ~ population, data=data,pch=19, col="slateblue1")
plot(median_house_value ~ households, data=data,pch=19, col="slateblue1")
plot(median_house_value ~ median_income, data=data,pch=19, col="slateblue1")
boxplot(median_house_value ~ ocean_proximity, data=data, col="slateblue1")

# Correlation between numeric variables
library(corrplot)
par(mfrow = c(1, 1))
corr_matrix <- round(cor(data[,1:9], method="spearman"), digits = 2)
corrplot(corr_matrix, method= "number", tl.cex = 1, type = "upper")

# Statistical indicators
# Calculate MAD since we have many outliers
mad(data[,"longitude"], constant = 1)
mad(data[,"latitude"], constant = 1)
mad(data[,"housing_median_age"], constant = 1)
mad(data[,"total_rooms"], constant = 1)
mad(data[,"total_bedrooms"], constant = 1)
mad(data[,"population"], constant = 1)
mad(data[,"households"], constant = 1)
mad(data[,"median_income"], constant = 1)
mad(data[,"median_house_value"], constant = 1)


# ------------------------------------------------------------------------------
# Cleaning data
# Create helper variables (easier to decide if value is possible)
data$people_per_household = data$population/data$households
data$rooms_per_person = data$total_rooms/data$population

# Add variable which contains two other and drop the others (Multikollinearität)
data$beds_per_room = data$total_bedrooms/data$total_rooms

# Remove improbable records
data<-data[!(data$median_house_value>=500001),]
data<-data[!(data$housing_median_age>=52),]
data<-data[!(data$median_income>=15.0001),]
data<-data[!(data$beds_per_room>1),]

# Export data -> easier to handle outliers
library("xlsx")
write.xlsx2(data, file = "/Users/timstaudinger/Desktop/Repository.nosync/MachineLearning-Exam/dataset/housing_new.xlsx",
            sheetName = "Sheet1", append = FALSE, row.names = TRUE)

# Import cleaned data set and check if everything correct
setwd("/Users/timstaudinger/Desktop/Repository.nosync/MachineLearning-Exam/dataset")
data <- read.csv2("housing_cleaned.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)
data[1:5,]
summary(data)

# Remove (helper-) variables
drops = c('total_bedrooms', 'total_rooms', 'people_per_household', 'rooms_per_person', 'X')
data = data[ , !(names(data) %in% drops)]
summary(data)
