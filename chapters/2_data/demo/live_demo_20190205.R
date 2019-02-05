# Chapter 2A - Working with Tabular Data
# Live Demo Code

# See Also Pedon Data with R Cheatsheet
## http://ncss-tech.github.io/AQP/cheatsheets/spc-cheatsheet.html

# vectors, objects and inspection
subgroup <- c("typic haplocryepts","andic haplocryepts","typic dystrocryepts")  
andic <- c(FALSE, TRUE, FALSE) 

# data.frames
## creation
df <- data.frame(subgroup, andic)

names(df) <-c('tax_subgroup', 'andic.soil.properties')

## subsets
df[2,]

df[2,2]

## save to CSV (comma-separated value)
write.csv(df, file = "chapter2_demo.csv", row.names=F)
df2 <- read.csv(file = "chapter2_demo.csv")

df == df2

# NCSS soil-related packages

# Ways of creating SPCs from databases
## fetchNASIS
nasis.comp <- fetchNASIS_components()
nasis.pedons <- fetchNASIS_pedons()

### QC output
coiid.bad <- get('component.hz.problems', envir=soilDB.env)
peiid.bad <- get('top.bottom.equal', envir=soilDB.env)

site(nasis.comp)$coiid %in% coiid.bad
site(nasis.pedons)$pedon_id[site(nasis.pedons)$pedon_id %in% peiid.bad]

length(nasis.comp$coiid)
length(site(nasis.comp)$coiid)

## fetchKSSL

lab.data <- fetchKSSL(series="Holland")
site(lab.data)
horizons(lab.data)

str(lab.data)

lab.data$clay

## fetchSDA_component() & SDA_query
sda.comp <- fetchSDA_component(WHERE = "compname = 'Mantree'")

## fetchNASISWebReport()
web.report <- fetchNASISWebReport(projectname = 'MLRA 22A - CA731 and CA630 exact-join')
str(web.report)

plot(web.report$spc)

## fetchOSD
### extended=TRUE

# The contents of an SPC

# Filtering SPCs

# Saving site data to a shapefile