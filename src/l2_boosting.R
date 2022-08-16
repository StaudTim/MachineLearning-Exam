# Import cleaned data set and check if everything correct
setwd("/Users/timstaudinger/Desktop/Repository.nosync/MachineLearning-Exam/dataset")
data <- read.csv2("housing_cleaned.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)
data[1:5,]
summary(data)

# Remove (helper-) variables
drops = c('total_bedrooms', 'total_rooms', 'people_per_household', 'rooms_per_person', 'X')
data = data[ , !(names(data) %in% drops)]
summary(data)



# Swap the order of the data
n <- length(data[,1])
index <- sample(1:n,n,replace=FALSE)
data <- data[index,]



# L2-Boosting
library(mboost)

# Split into training and test data set
data.train <- data[1:12774,]
data.test <- data[12775:18248,]

# Model
model <- gamboost(median_house_value ~ longitude + latitude + housing_median_age + beds_per_room +  
                    population + households + median_income + ocean_proximity, data=data.train, 
                  dfbase = 4, control = boost_control(mstop = 5000, trace=TRUE))

# Cross validation
cv10f <- cv(model.weights(model), type = "kfold")
cvm <- cvrisk(model, folds = cv10f, papply = lapply)
mstop(cvm)

model <- gamboost(median_house_value ~ longitude + latitude + housing_median_age + beds_per_room + 
                    population + households + median_income + ocean_proximity, data=data.train, 
                  dfbase = 4, control = boost_control(mstop = mstop(cvm), trace = TRUE))

# Calculate forecast quality
X.test <- data.test[,c("longitude","latitude","housing_median_age","beds_per_room",
                       "population", "households", "median_income", "ocean_proximity")]
prog <- predict(model,X.test)
y.test <- data.test[,"median_house_value"]
mad(y.test-prog, constant = 1) # MAE =  26720.3

# Plot to see if we over or underestimate the price
plot(prog - y.test, X.test$ocean_proximity,
     xlab = "Schätzung unter / über tatsächlichen Marktwert",
     ylab = "",
     main = "L2-Boosting",
     col = "dodgerblue", yaxt = "none")
axis(2, at = 1:4, labels = levels(data$ocean_proximity))
