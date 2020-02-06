plot(x, y) # This text will not affect the plot function because of the comment

# Addition
1 + 1

# Multiplication
10 * 10

# Compute Logarithm
log10(100)

# Print Text
"Hello World"

# Plot Histogram
hist(npk$yield)

# Assignment
test <- 1

# or

test = 1


setwd("C:/workspace2")

getwd()

## sand <- read.csv("C:/workspace2/sand_example.csv")
## 
## # if your workspace was already set you could simply use the filename, like so
## 
## sand <- read.csv("sand_example.csv")

sand <- read.csv("https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/sand_example.csv")

write.csv(sand, file = "sand_example2.csv")

# or use the write.table() function to export other text file types

str(sand)

names(sand)

head(sand)

ls()

# Remove all R objects
rm(list = ls(all = TRUE)) 

# Remove individal objects
rm(sand)

# Help file for a function
help(read.csv) # or ?read.csv

# Help files for a package
help(package = "soiltexture")

library() 

# or

installed.packages()

# CRAN (static version)
installed.packages(c("aqp", "soilDB", "soilReports", "soiltexture"))

# GitHub (development version)
devtools::install_github("ncss-tech/soilDB", dependencies = FALSE, upgrade_dependencies = FALSE, build = FALSE)

library(soiltexture)

help(package = "soiltexture")

# Copied from soiltexture vignette
# Create a dummy data frame of soil textures:
example <- data.frame(
CLAY = c(05,60,15,05,25,05,25,45,65,75,13,47),
SILT = c(05,08,15,25,55,85,65,45,15,15,17,43),
SAND = c(90,32,70,70,20,10,10,10,20,10,70,10),
OC = c(20,14,15,05,12,15,07,21,25,30,05,28)
) 

TT.plot(class.sys = "USDA-NCSS.TT", tri.data = example)





savehistory(file = "sand.Rhistory")  
loadhistory(file = "sand.Rhistory")  
history(max.show=Inf) #displays all previous commands



png(file = "npk_yield.png")
plot(npk$yield)
dev.off()
