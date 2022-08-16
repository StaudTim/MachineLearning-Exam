# Import cleaned data set and check if everything correct
setwd("/Users/timstaudinger/Desktop/Repository.nosync/MachineLearning-Exam/dataset")
data <- read.csv2("housing_cleaned.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)
data[1:5,]
summary(data)

# Remove (helper-) variables
drops = c('total_bedrooms', 'total_rooms', 'people_per_household', 'rooms_per_person', 'X')
data = data[ , !(names(data) %in% drops)]
summary(data)



# Prepare training/test data set
# Swap the order of the data
n <- length(data[,1])
index <- sample(1:n,n,replace=FALSE)
data <- data[index,]



# Neural network
library(ANN2)

# Split into training and test data set
data.train <- data[1:9124,]
data.val <- data[9122:12774,]
data.test <- data[12775:18248,]

# Tuning parameters
X.train <- model.matrix( median_house_value ~ longitude + latitude + 
                           housing_median_age + beds_per_room  + population + 
                           households + median_income + ocean_proximity, 
                         data=data.train)

X.train <- X.train[,-1]
y.train <- data.train[,"median_house_value"]

guete <- 0
parameters <- list( 
  c(11, 15, 18, 19, 21), 
  c(64, 32, 16, 8), 
  c(10, 30, 5), 
  c(24, 48, 24, 12, 6)) 

# Try different parameters and calculate forecast quality on validation data set
for(k in 1:length(parameters)){
  # Train model
  units  <- unlist(parameters[k], use.names = FALSE)
  model <- neuralnetwork(X.train, y.train, hidden.layers=units, regression = TRUE, 
                         loss.type = "absolute", learn.rates = 1e-04,n.epochs = 100,
                         verbose=-TRUE)
  
  # Calculate forecast quality on validation data set
  X.val <- model.matrix( median_house_value ~ longitude + latitude + 
                           housing_median_age + beds_per_room + 
                           population + households + median_income + ocean_proximity, data=data.val)
  
  X.val <- X.val[,-1]
  prog <- predict(model,X.val)$predictions
  y.val <- data.val[,"median_house_value"]   
  guete[k] <- mad(y.val-prog, constant = 1)
}

# Combine train and val data set
data.train <- data[1:12774,]
X.train <- model.matrix( median_house_value ~ longitude + latitude + 
                           housing_median_age + beds_per_room + 
                           population + households + median_income + ocean_proximity, 
                         data=data.train)

X.train <- X.train[,-1]
y.train <- data.train[,"median_house_value"]

# Train model with tuned parameters on combined data set
best <- which.min(guete)
units <- unlist(parameters[best], use.names = FALSE)
model <- neuralnetwork(X.train, y.train, hidden.layers=c(units), regression = TRUE, 
                       loss.type = "absolute", learn.rates = 1e-04,n.epochs = 1000,
                       verbose=-TRUE)

# Calculate forecast quality on test data set
X.test <- model.matrix( median_house_value ~ longitude + latitude + 
                          housing_median_age + beds_per_room + 
                          population + households + median_income + ocean_proximity, data=data.test)

X.test <- X.test[,-1]
prog <- predict(model,X.test)$predictions
y.test <- data.test[,"median_house_value"]   
mad(y.test-prog, constant = 1) # MAE = 19543.55

# Plot to see if we over or underestimate the price
plot(prog - y.test, data.test[,c("ocean_proximity")],
     xlab = "Schätzung unter / über tatsächlichen Marktwert",
     ylab = "",
     main = "Neuronales Netz",
     col = "dodgerblue", yaxt = "none")
axis(2, at = 1:4, labels = levels(data$ocean_proximity))

