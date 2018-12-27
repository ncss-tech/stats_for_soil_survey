#This script will demonstrate the commands used in the R editor section

sand <- read.table("C:/XXXXX/sand_example.csv", header = TRUE, sep=",")
names(sand)

plot(depth~sand, data = sand, ylim =c(50,-1), main = "Total Sand (%) by depth", 
	xlab = "Sand", ylab = "Depth")

install.packages("lattice", dep=TURE)
library(lattice)

windows()
xyplot(depth~sand,data = sand, groups=master, main = "Total Sand by depth and Master Horizon",
       auto.key=list(columns = 2), ylim=c(35,-5))
windows()
boxplot(sand~landuse,data = sand)

windows()
hist(sand$sand)


#notice you will need to change the path of the file if you do not want it to save in your working directory
png ("sand_boxplot.png")
bwplot(sand~master|landuse, data=sand)
dev.off

windows()
pdf ('depth_histogram.pdf')
hist(sand$depth)
dev.off
