df <- read.csv("../classroom_data_probability.csv", header=TRUE)

summary(df)

hist(df$score3)

length(AO)
mean(AO)
sd(AO)
z = 1.96

#Confidence Interval
error = z * (AO_SD/sqrt(AO_Len))
lowerCI = AO_M - error
upperCI = AO_M + error

#Spliting Data
dataBySex = split(angerData, sex)
maleData = dataBySex$`1`
femaleData = dataBySex$`2`


mdAO = maleData$Anger.Out
fdAO = femaleData$Anger.Out

names(angerData)
class(Gender)

boxplot(mdAO, fdAO, main = "Anger-Out: Male and Female", names = c("Male", "Female"), xlab = "Sex", ylab = "AO Index")

#Installing
install.packages("")
