NetherlandsDeathsByAge_1000 = read.csv("data/NetherlandsDeathsByAge.csv")
NetherlandsPopulationByAge_1000 = read.csv("NetherlandsPopulationByAge.csv")


NetherlandsProbDeathsByAge_1000 = NetherlandsPopulationByAge_1000 
NetherlandsProbDeathsByAge_1000[2] = (NetherlandsDeathsByAge_1000[2] + NetherlandsDeathsByAge_1000[3]) / (NetherlandsPopulationByAge_1000[2] + NetherlandsDeathsByAge_1000[2] + NetherlandsDeathsByAge_1000[3])
NetherlandsProbDeathsByAge_1000[3] = NetherlandsDeathsByAge_1000[4]/ (NetherlandsPopulationByAge_1000[3] + NetherlandsDeathsByAge_1000[4])
NetherlandsProbDeathsByAge_1000[4] = NetherlandsDeathsByAge_1000[5]/ (NetherlandsPopulationByAge_1000[4] + NetherlandsDeathsByAge_1000[5])
NetherlandsProbDeathsByAge_1000[5] = NetherlandsDeathsByAge_1000[6]/ (NetherlandsPopulationByAge_1000[5] + NetherlandsDeathsByAge_1000[6])
NetherlandsProbDeathsByAge_1000[6] = NetherlandsDeathsByAge_1000[7]/ (NetherlandsPopulationByAge_1000[6] + NetherlandsDeathsByAge_1000[7])

write.csv(NetherlandsProbDeathsByAge_1000, "NetherlandsProbDeathsByAge_1000.csv")

library(ggplot2)
probDeathByAgeGroup = data.frame(ageGroup = c("0_20","20_45","45_65","65_80","80_","0_20","20_45","45_65","65_80","80_"), probDeath = c(0.000678528,0.00087799,0.007178527,0.105390335,2.669333333,0.001498961,0.002021953,0.009212828,0.113659359,2.69382716), year = c(1940,1940,1940,1940,1940,1945,1945,1945,1945,1945))
fit1940 = c(0.03918391838669777,33.38881301879883,-27.568927764892578)
fit1945 = c(0.04838055372238159, 26.322864532470703, 31.86722755432129)
mortalityOfPersonByAge = function(fit, age){
  return (fit[1] * exp(((age - fit[2]) / fit[3]) ^ 2))
}
probDeathByAgeGroup$ModelFit = c(mortalityOfPersonByAge(fit1940, 10),mortalityOfPersonByAge(fit1940, 30),mortalityOfPersonByAge(fit1940, 50),mortalityOfPersonByAge(fit1940, 72.5),mortalityOfPersonByAge(fit1940, 90),mortalityOfPersonByAge(fit1945, 10),mortalityOfPersonByAge(fit1945, 30),mortalityOfPersonByAge(fit1945, 50),mortalityOfPersonByAge(fit1945, 72.5),mortalityOfPersonByAge(fit1945, 90))

library(reshape2)
probDeathByAgeGroup = melt(probDeathByAgeGroup, id.vars = c("ageGroup", "year"), variable.name = "source", value.name = "probDeath")
probDeathByAgeGroup$source = ifelse(probDeathByAgeGroup$source == "probDeath", "Data", "GA + Exponential Distribution")
ggplot(probDeathByAgeGroup, aes (ageGroup, probDeath, group = source)) + geom_point(aes(color = source)) + theme(text = element_text(size = 16)) + xlab("Age") + ylab("Annual Death Rate") + guides(size=FALSE) + facet_grid(.~year) 


####1940 model fit####
ageGroup.1940 = c(10,30,50,72.50)#,90)
probDeath.1940 = c(0.000678528,0.00087799,0.007178527,0.105390335)#,2.669333333)
exponential.model.1940 <- lm(probDeath.1940 ~ exp(ageGroup.1940))

age_values.1940 = seq(1, 100, 1)
probDeath.predicted.1940 <- predict(exponential.model.1940,list(ageGroup.1940 = age_values.1940))
plot(ageGroup.1940, probDeath.1940,pch=16)
lines(age_values.1940,probDeath.predicted.1940,lwd=2, col = "red", xlab = "Age Group", ylab = "Prob Death")

####1945 model fit####
ageGroup.1945 = c(10,30,50,72.5)#,90)
probDeath.1945 = c(0.001498961,0.002021953,0.009212828,0.113659359)#,2.69382716)
exponential.model.1945 <- lm(probDeath.1945 ~ exp(ageGroup.1945))

age_values.1945 = seq(1, 100, 1)
probDeath.predicted.1945 <- predict(exponential.model.1945,list(ageGroup.1945 = age_values.1945))
plot(ageGroup.1945, probDeath.1945,pch=16)

lines(age_values.1945,probDeath.predicted.1945,lwd=2, col = "red", xlab = "Age Group", ylab = "Prob Death")



########################using aggregated Fertility Data##############################
fertility19402017 = read.csv("data/FertilityNetherlandsFromMaria.csv")
fertility19402017 = aggregate(fertility19402017[,3], list(fertility19402017$TimeMid, fertility19402017$AgeGroup), mean)
colnames(fertility19402017) = c("Year", "AgeGroup", "AnnualFertilityRate.BirthsPerThousandWomen")
ggplot(fertility19402017, aes(Year, AnnualFertilityRate.BirthsPerThousandWomen)) + geom_point() + facet_grid(.~AgeGroup) + ylab("Fertility Rate")
write.csv(fertility19402017,"Data/NetherlandsFertility1940to2017.csv")

########################Stein and Susser 1975 study on Dutch Famine#################
SteinAndSusser1975 = read.csv("data/SteinAndSusser1975Table3.csv", header = TRUE)
SteinAndSusser1975$FamineArea.TotalBirths = SteinAndSusser1975$FamineArea.MaleBirths + SteinAndSusser1975$FamineArea.FemaleBirths
SteinAndSusser1975$NorthControlArea.TotalBirths = SteinAndSusser1975$NorthControlArea.MaleBirths + SteinAndSusser1975$NorthControlArea.FemaleBirths
SteinAndSusser1975$SouthControlArea.TotalBirths = SteinAndSusser1975$SouthControlArea.MaleBirths + SteinAndSusser1975$SouthControlArea.FemaleBirths
library(reshape2)
SteinAndSusser1975.TotalBirths = melt(subset(SteinAndSusser1975, select = c("Date", "FamineArea.TotalBirths", "NorthControlArea.TotalBirths", "SouthControlArea.TotalBirths")), id.vars = c("Date"), value.name = "TotalBirths")
ggplot(SteinAndSusser1975.TotalBirths, aes(Date, TotalBirths)) + geom_point() + facet_grid(.~variable) + theme(axis.text.x = element_text(angle = 90,hjust = -1))#+scale_x_datetime(labels = date_format("%b")
##########################Deaths by Age#############################################

NetherlandsDeathsByAge_1000 = na.omit(melt(NetherlandsDeathsByAge_1000, id.vars = c("Year")))
colnames(NetherlandsDeathsByAge_1000) = c("Year","AgeGroup","AnnualMortalityRate.DeathsPerThousandPeople")
NetherlandsDeathsByAge_1000$AgeGroup = substr(NetherlandsDeathsByAge_1000$AgeGroup, 2,6)
write.csv(NetherlandsDeathsByAge_1000$AgeGroup, "NetherlandsDeathsByAge_1000$AgeGroup.csv")