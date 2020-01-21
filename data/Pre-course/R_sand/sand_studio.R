# this is a demo of how to use a saved R script in R studio

sand2 <- read.table("C:/R_data/sand_example.csv", header=TRUE, sep=",", 
  na.strings="NA", dec=".", strip.white=TRUE)

plot(depth~sand, data = sand2, ylim =c(50,-1), main = "Total Sand (%) by depth", 
     xlab = "Sand", ylab = "Depth")

hist(sand2$sand)

#change color and labels

hist(sand2$sand, col = 'blue', main = 'Histogram of Sand Content', xlab = 'Sand Content (%)')



install.packages(ggplot2)
library(ggplot2)

#simple histogram
ggplot(data = sand2, aes(x = sand )) + geom_histogram()

#we can control the number of bins
ggplot(data = sand2, aes(x = sand )) + geom_histogram(bins = 10)

#or the width of bins
ggplot(data = sand2, aes(x = sand )) + geom_histogram(binwidth = 1)

#we can also change the color
ggplot(data = sand2, aes(x = sand )) + geom_histogram(binwidth = 1, fill = 'blue')


#let's create a boxplot with ggplot
l <- ggplot(data = sand2, aes(x = landuse, y = sand)) + geom_boxplot()

#wait no plot showed up - that's because we assigned the plot the object 'l'

l

#we can add color by assigning fill to a variable
lc <- b <- ggplot(data = sand2, aes(x = landuse, y = sand)) + geom_boxplot(aes(fill = landuse))
lc

#we can make a more complicated graph by filling with another variable
lm <- b <- ggplot(data = sand2, aes(x = landuse, y = sand)) + geom_boxplot(aes(fill = master))
lm

#we can add new command to the exsisting object 
#note that was are using fill because that is what's in the aes(), sometimes you'll use scale_color_
lm + scale_fill_brewer(palette = "Set1")
lm + scale_fill_grey()


#Notice that there is a + as you build elements - you can have a return after a comma within a () without a +
#use R studio to help you track and find pairs of '' and ()
#and we can add labels and adjust ticks
lm + scale_fill_grey() + labs(x = "Landuse", y = "Sand Content (%)") +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45),
        axis.text.y = element_text(face="bold", color="#993333", size=14, angle=45))

#our additions are necessarily better..........
#there are many ways edit graphs for presentations and sharing
#there are many helpful websites


