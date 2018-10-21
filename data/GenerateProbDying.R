NetherlandsDeathsByAge_1000 = read.csv("NetherlandsDeathsByAge.csv")
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
