sand <- read.table("C:/XXXXX/sand_example.csv", header = TRUE, sep=",")
 names(sand)

plot(depth~sand, data = sand, ylim =c(50,-1), main = "Total Sand (%) by depth", 
	xlab = "Sand", ylab = "Depth")

lattice::xyplot(depth~sand,data = sand, groups=master, main = "Total Sand by depth and Master Horizon",
       auto.key=list(columns = 2), ylim=c(35,-5))

boxplot(sand~landuse,data = sand)

hist(sand$sand)

hist (sand$depth)


#notice you will need to change the path of the jpg file if you do not want it to save in your working directory
jpeg ("sand_boxplot.jpg")
bwplot(sand~master|landuse,data = sand)
dev.off()


