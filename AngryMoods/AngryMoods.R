#Loading data
angerData <- read.csv("../angry_moods.csv", header=TRUE)
attach(angerData)
library(ggplot2)
library(plyr)
library(reshape2)

#Summary
summary(angerData)


#All anger-out
AO = angerData$Anger.Out


#Calculating confidence interval for all
AO_Len = length(AO)
AO_M = mean(AO)
AO_SD = sd(AO)
z = 1.96

error = z * (AO_SD/sqrt(AO_Len))
lowerCI = AO_M - error
upperCI = AO_M + error
lowerCI
upperCI


#6a. Spliting data by sex and boxplot
dataBySex = split(angerData, Gender)
boxplot(dataBySex)

maleData = dataBySex$`1`
femaleData = dataBySex$`2`
mdAO = maleData$Anger.Out
fdAO = femaleData$Anger.Out


##These two do the same thing
boxplot(mdAO, fdAO, main = "Anger-Out: Male and Female", names = c("Male", "Female"), xlab = "Sex", ylab = "AO Index")
boxplot(Anger.Out~Gender)


#6b. btb stem and leaf
summary(mdAO)
summary(fdAO)
mdAO
fdAO


#6c. Confidence Interval for male and female
mLen = length(mdAO)
mMean = mean(mdAO)
mSD = sd(mdAO)
error = z * (mSD/sqrt(mLen))
lowerCIm = mMean - error
upperCIm = mMean + error

fLen = length(fdAO)
fMean = mean(fdAO)
fSD = sd(fdAO)
error = z * (fSD/sqrt(fLen))
lowerCIf = fMean - error
upperCIf = fMean + error

lowerCIm
upperCIm

lowerCIf
upperCIf


#6d. T-test on sex means
t.test(mdAO, fdAO)

#6e. ANOVA Test
anova1 = aov(Anger.Out~Gender)
summary(anova1)

#7. Anger-In Scores
summary(angerData$Anger.In)

#8. Parallel box lots Anger-In Scores
boxplot(Anger.In~Sports, main = "Anger-In: Sports Participation", names = c("Athlete", "Non-Ath"), ylab = "AI Index")

#9. CI for Anger-In Athlete vs non-Ath
dataByPart = split(angerData, Sports)
athe = dataByPart$`1`
nonA = dataByPart$`2`

aData = athe$Anger.In
aLen = length(aData)
aMean = mean(aData)
aSD = sd(aData)
error = z * (aSD/sqrt(aLen))
lowerCIa = aMean - error
upperCIa = aMean + error

naData = nonA$Anger.In
naLen = length(naData)
naMean = mean(naData)
naSD = sd(naData)
error = z * (naSD/sqrt(naLen))
lowerCIna = naMean - error
upperCIna = naMean + error

lowerCIa
upperCIa
lowerCIna
upperCIna


#10 Ploting a histogram
hist(angerData$Control.Out)


#11 Means for Control-Out Score
summary(angerData$Control.Out)
summary(athe$Control.Out)
summary(nonA$Control.Out)


#12 Are the means statistically significant
t.test(athe$Control.Out, nonA$Control.Out)


#13 Bar Graphs for Control-In
dataByPartCI <- split(angerData$Control.In, angerData$Sports)
dbpciMean <- sapply(dataByPartCI, mean)
barplot(dbpciMean, main="CI for Athletes and Non", ylab = "Mean", names = c("Athlete", "Non-Ath"))


#14 Variance
var(athe$Control.In)
var(nonA$Control.In)

#15 Standard error of mean for Control-In
aDataCI = athe$Control.In
aLenCI = length(aDataCI)
aMeanCI = mean(aDataCI)
aSDCI = sd(aDataCI)
aErrorCI = z * (aSDCI/sqrt(aLen))

naDataCI = nonA$Control.In
naLenCI = length(naDataCI)
naMeanCI = mean(naDataCI)
naSDCI = sd(naDataCI)
naErrorCI = z * (naSDCI/sqrt(naLenCI))

aErrorCI
naErrorCI


#16 t-test to see if one calms down more
t.test(aDataCI, naDataCI)
mean(aDataCI)

#17 Anger Expression index box plot by participation
boxplot(Anger_Expression~Sports, data=angerData, main="Exp Data", xlab="count", ylab="people", names = c("Athlete", "Non-Ath"))

#18 Anger Expression index box plot by sex
boxplot(Anger_Expression~Gender, data=angerData, main="Exp Data", xlab="count", ylab="people", names = c("Male", "Female"))



