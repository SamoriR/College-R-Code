goldData <- read.csv("../elementgold_istarget.csv", header=TRUE)

attach(goldData)

as_Level = goldData$col1
sb_Level = goldData$col2

#1.a.
plot(sb_Level, as_Level)

#1.b.
fit = lm(as_Level~sb_Level)

#1.c.
par(mfrow=c(2,2))
plot(fit)

#1.d.
summary(fit)

#1.f.
predicted = predict(fit)
par(mfrow=c(1,1))
plot(predicted, as_Level)

#2.a.
gold_Dep = goldData$col4
plot(as_Level, gold_Dep)

#2.b.
plot(sb_Level, gold_Dep)

#2.c.
fitg = glm(gold_Dep~sb_Level)
summary(fitg)

#2.d.
plot(gold_Dep~sb_Level)
lines(sb_Level,fitg$fitted,type="l", col="red")

#2.e.
fitb = glm(gold_Dep~sb_Level+as_Level)
summary(fitb)



