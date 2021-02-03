library(soilDB)
library(aqp)


data("gopheridge", package = "soilDB")

# tabulate original horizon designations
sort(
        table(gopheridge$hzname), 
        decreasing = TRUE
)

# generalized horizon labels (GHL)
n <- c('O', 'A', 'Bt1', 'Bt2', 'Bt3','Cr','R')

# patterns matching each GHL
# "^A": left-anchor, string must start with "A"
# "Bt1|Bw": "Bt1" or "Bw"
# "Bt$": right-anchor, string must end at end of "Bt" 
p <- c('O', '^A|BA$', 'Bt1|Bw','Bt$|Bt2', 'Bt3|CBt$|BCt','Cr','R')

# assign GHL via pattern-matching
gopheridge$genhz <- generalize.hz(gopheridge$hzname, n, p)

# cross-tabulate
tab <- table(gopheridge$genhz, gopheridge$hzname)

# check
tab

# quick viz
par(mar = c(0, 0, 3, 0))
plotSPC(gopheridge, color = 'genhz', plot.depth.axis = FALSE, print.id = FALSE, name = NA)


