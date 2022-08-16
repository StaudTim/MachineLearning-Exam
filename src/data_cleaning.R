setwd("/Users/timstaudinger/Desktop/Repository.nosync/MachineLearning-Exam/dataset")
data <- read.csv("housing.csv",header=TRUE,sep=",",fill=TRUE,stringsAsFactors=TRUE)
data[1:5,]

# Check data type and summary
data[,"ocean_proximity"] <- as.factor(data[,"ocean_proximity"])
summary(data)



# Data cleaning
# Handle missing values in "total_bedrooms" (median)
data$total_bedrooms[is.na(data$total_bedrooms)] <- median(data$total_bedrooms, na.rm=TRUE)

# Create helper variables (easier to decide if value is possible)
data$people_per_household = data$population/data$households
data$rooms_per_person = data$total_rooms/data$population

# Create new variable out of two others (Multikollinearität)
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
