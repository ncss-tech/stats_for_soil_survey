# this is a demo of how to use R editor with an R Commander Script
sand2 <- read.table("C:/R_data/sand_example.csv", header=TRUE, sep=",", 
  na.strings="NA", dec=".", strip.white=TRUE)
library(relimp, pos=4)
showData(sand2, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, 
  maxheight=30)
Hist(sand2$sand, scale="frequency", breaks="Sturges", col="blue")

Boxplot(sand~landuse, data=sand2, id.method="y")
summary(sand2)
library(multcomp, pos=4)
library(abind, pos=4)
AnovaModel.1 <- aov(sand ~ landuse, data=sand2)
summary(AnovaModel.1)
numSummary(sand2$sand , groups=sand2$landuse, statistics=c("mean", "sd"))

