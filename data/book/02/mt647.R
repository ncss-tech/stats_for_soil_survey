library(aqp)
library(soilDB)

# use _PedonPC_Plus_DataDump_select (MLRA04 Bozeman)
#  User Site ID: %MT647%
#  NASIS Site: SSRO_Northwest
#  NASIS Group: NW-MIS Point Data
#  
#  Note that this NASIS group now contains duplicate pedons. 
#  Exclude 1 set, should have 538 records in site/pedon

mt647err <- fetchNASIS(rmHzErrors = FALSE)
mt647 <- fetchNASIS(rmHzErrors = TRUE)
save(mt647, mt647err, soilDB.env, file =  "../data/book/02/mt647.rda")
