#test correlation 'stuff

#Correlation Matrix
#clean dataset
#prop carbon calculation
#set null CaCarb to zero
dsp$CaCarb[is.na(dsp$CaCarb)]<- 0
dsp$CaCarb[dsp$CaCarb<0]<- 0
summary(dsp$CaCarb)

# calculate SOC - soil organic carbon
SOC_calc <- dsp$Tot_C - 0.11*dsp$CaCarb


dsp$SOC <- as.numeric(
  ifelse(!is.na(dsp$Est_totOC) & dsp$Est_totOC>=0, dsp$Est_totOC,   
         ifelse(!is.na(dsp$Tot_C), dsp$Tot_C,                       
                ifelse(!is.na(dsp$Total.C), dsp$Total.C, 
                       NA ))))

summary(dsp$SOC)

#set values less than zero to zero
dsp$SOC[dsp$SOC<0]<- 0

summary(dsp$SOC)

dsp$TN <- as.numeric(ifelse(!is.na(dsp$Tot_N), dsp$Tot_N, dsp$Total.N))  

summary(dsp$TN)

# row count for each type of bulk density
apply(dsp[, grep("^BD", names(dsp))], 2, function(x) length(which(!is.na(x))))

#name of new combined bulk density column
bd <- "BulkDensity"

#combine multiple bulk density methods
bd_1 <- "BD_core_fld"
bd_2 <- "BD_clod_13"
bd_3 <- "BD_compcav"
bd_4 <- "BD_recon13"
bd_5 <- "BD_other"
dsp$bd <- as.numeric(
  ifelse(!is.na(dsp[,bd_1]), dsp[,bd_1], 
         ifelse(!is.na(dsp[,bd_2]), dsp[,bd_2],
                ifelse(!is.na(dsp[,bd_3]), dsp[,bd_3],
                       ifelse(!is.na(dsp[,bd_4]), dsp[,bd_4],
                              dsp[,bd_5])
                ))))
#add source of bd
dsp$bd_source <-  ifelse(!is.na(dsp[,bd_1]), bd_1, 
                         ifelse(!is.na(dsp[,bd_2]), bd_2,
                                ifelse(!is.na(dsp[,bd_3]), bd_3,
                                       ifelse(!is.na(dsp[,bd_4]), bd_4,
                                              bd_5))))

logOC <- log(dsp$SOC)

#Transform distibutions
#function that returns values <1

signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

dsp$logOC <- signedlog10(dsp$SOC)
dsp$logPOX <- signedlog10(dsp$POX_C)
dsp$logBgluc <- log10(dsp$Bgluc)
dsp$logPOMC <- log10(dsp$Pom_C)

dsp$logBgluc[is.infinite(d$logBgluc)] <- NA

#package
library(corrplot)
library(gpairs)

library("GGally")

#find number data
D <- c("SOC", "TN", "Bgluc", "Pom_C", "Pom_N", "POX_C", "CEC_ph7", "CEC_NH4OAc", "ph_h20", "Clay", "AggStab", "bd", "logBgluc", "logPOMC", "logOC", "logPOX")
d <- dsp[,D]
# num <- sapply(dsp, is.numeric)
# N <- d[num]


M <- cor(d, use = "pairwise.complete.obs")


# work around to ignore NA's
p = M
p[is.na(M)]=0.2 
p[is.na(M)==F]=0
M[is.na(M)]=0

corrplot(M, method = "circle")
corrplot(M, method = "ellipse")

MM <- corrplot(M, method = "ellipse")

scatter <- pairs()
gpairs(d[,1:6])

D <- c("SOC", "Bgluc", "Pom_C", "POX_C",  "Clay", )
L <- c("logBgluc", "logPOMC", "logOC", "logPOX")
Dd <- d[,D]
ggpairs(Dd, lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="bar", params=c(colour="blue")), 
        upper=list(params=list(corSize=6)), axisLabels='show')

Dl <- d[,L]
DL <- Dl[complete.cases(Dl),]
ggpairs(Dl, 
        lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="bar", params=c(colour="blue")), 
        upper=list(params=list(corSize=6)), axisLabels='show)

pm <- ggpairs(
  diamonds.samp[,1:5],
  upper = list(continuous = "density", combo = "box"),
  lower = list(continuous = "points", combo = "dot"),
  color = "cut",
  alpha = 0.4,
  title = "Diamonds"
)


data(tips, package = "reshape")
pm <- ggpairs(tips[,1:3])
pm <- ggpairs(tips, 1:3, columnLabels = c("Total Bill", "Tip", "Sex"))
pm
pm <- ggpairs(tips, upper = "blank")
pm

data(diamonds, package="ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1],200),]

# Custom Example
pm <- ggpairs(
  diamonds.samp[,1:5],
  upper = list(continuous = "density", combo = "box"),
  lower = list(continuous = "points", combo = "dot"),
  color = "cut",
  alpha = 0.4,
  title = "Diamonds"
)
pm
