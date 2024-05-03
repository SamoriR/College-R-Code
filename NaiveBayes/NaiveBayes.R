# Import Bayes.csv from class webpage
Bayes <- read.csv("../bayes.csv")
iris <- read.csv("../iris.csv", header=TRUE)
# Select training data
traindata <- Bayes[1:14,]

# Select test data
testdata <- Bayes[15,]

# Calculate the Prior for Play
Pplay <- table(traindata$Play)
Pplay <- Pplay/sum(Pplay)

# Calculate P(Sunny | Play) 
sunny <- table(traindata[,c("Play", "Sunny")]) 
sunny <- sunny/rowSums(sunny)

# Calculate P(Hot | Play)
hot <- table(traindata[,c("Play", "Hot")]) 
hot <- hot/rowSums(hot)

# and Calculate P(Windy | Play)
windy <- table(traindata[,c("Play", "Windy")])
windy <- windy/rowSums(windy)

# Evaluate testdata
Pyes <- sunny["Yes","Yes"] * hot["Yes","No"] * windy["Yes","Yes"]

Pno <- sunny["No","Yes"] * hot["No","No"] * windy["No","Yes"]

# Do we play or not?
max(Pyes, Pno)


#naiveBayes package
install.packages("e1071")

#load package
library(e1071)

#train model
m <- naiveBayes(traindata[,1:3], traindata[,4])

#evaluate testdata
predict(m,testdata[,1:3])

# use the naïveBayes classifier on the iris data
m <- naiveBayes(iris[,1:4], iris[,5])
table(predict(m, iris[,1:4]), iris[,5])
