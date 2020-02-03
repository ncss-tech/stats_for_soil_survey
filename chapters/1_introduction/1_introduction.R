knitr::opts_chunk$set(message = FALSE, warning = FALSE,  results = 'asis', eval=FALSE)

library(aqp)
library(soilDB)
data("loafercreek")

n <- c("A",
       "BAt",
       "Bt1",
       "Bt2",
       "Cr",
       "R")
# REGEX rules
p <- c("A",
       "BA|AB",
       "Bt|Bw",
       "Bt3|Bt4|2B|C",
       "Cr",
       "R")

# Compute genhz labels and add to loafercreek dataset
loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)

plot(loafercreek[1:5], color = "genhz", label = "pedon_id")



s <- aqp::slice(loafercreek, 1:100 ~ clay + phfield + total_frags_pct)
s <- aqp::slab(s, fm = ~ clay + phfield + total_frags_pct, slab.fun = function(x) quantile(x, c(0, 0.5, 1), na.rm = TRUE))
names(s) <- gsub("\\.", "", names(s))
names(s) <- gsub("^X", "p", names(s))


library(ggplot2)
ggplot(s, aes(x = top, y = p50)) +
  geom_line() +
  geom_ribbon(aes(ymin = p0, ymax = p100, x = top), alpha = 0.2) +
  xlim(c(100, 0)) + xlab("depth (cm)") + ylab("") +
  coord_flip() +
  facet_wrap(~ variable, scales = "free_x") +
  ggtitle("Loafercreek")


library(knitr)

h <- horizons(loafercreek)
h$genhz <- factor(h$genhz, levels = rev(levels(h$genhz)))

vars <- c("clay", "phfield", "total_frags_pct")
h2 <- reshape(h[c("genhz", vars)],
              direction = "long",
              timevar = "variable", times = vars,
              v.names = "value",    varying = vars
              )
# h3 <- select(h, genhz, clay, phfield, total_frags_pct) %>% 
#   gather(key = "variable", value = "value", clay, phfield, total_frags_pct)

# ggplot(h2, aes(x = genhz, y = value)) +
#   geom_boxplot() +
#   facet_wrap(~ variable, scales = "free_x") +
#   coord_flip()

vars <- c("variable", "genhz")
test <- {
  split(h2, h2[vars]) ->.;
  lapply(., function(x) data.frame(
    x[1, vars],
    pct10  = round(quantile(x$value,  0.1,  na.rm = TRUE)),
    median = round(median(x$value, na.rm = TRUE)),
    pct90  = round(quantile(x$value, 0.9,  na.rm = TRUE))
    )) ->.;
  do.call("rbind", .) ->.;
  .[order(.$variable, rev(.$genhz)), ] ->.;
  }

# test <- group_by(h2, variable, genhz) %>% 
#   summarize(
#     pct10    = round(quantile(value,  0.1,  na.rm = TRUE)),
#     median = round(median(value, na.rm = TRUE)),
#     pct90    = round(quantile(value, 0.9,  na.rm = TRUE))
#     ) %>%
#   arrange(variable, rev(genhz)) %>%
#   as.data.frame()

test <- test[!is.na(test$median), ]
rownames(test) <- NULL
kable(test[1:8, ])


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


rfiles <- data.frame(
  'File Type' = c("R Script", "R Markdown", "R Workspace", "R History"),
  'File Extension' = c(".R", ".Rmd", ".RData", ".RHistory"),
  Description = c("text file of R commands",
                  "text file of R commands formatted in Markdown",
                  "copy R objects from your workspace",
                  "copy all of commands sumbitted to the R Console"
                  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
  )

knitr::kable(rfiles)


save.image(file="workspace.RData")

savehistory(file = "sand.Rhistory")  
loadhistory(file = "sand.Rhistory")  
history(max.show=Inf) #displays all previous commands

library(knitr)
test <- data.frame(
  Format = c("pdf", "window metafile", "png", "jpeg", "bmp", "postscript"),
  Function = c('pdf("graphic.pdf")', 'win.metafile("graphic.wmf")', 'png("graph.png")', 'jpeg("graph.jpg")', 'bmp("graph.bmp")', 'postscript("graph.ps")')
  )
kable(test)

png(file = "npk_yield.png")
plot(npk$yield)
dev.off()

## install.packages(Rcmdr)
## library(Rcmdr)
