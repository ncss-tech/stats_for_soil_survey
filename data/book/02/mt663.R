library(aqp)
library(soilDB)
# use _PedonPC_Plus_DataDump_select (SSRO_Northwest)
#  User Site ID: 2015MT663%
#  NASIS Site: SSRO_Northwest
#  NASIS Group: NW-MIS Point Data
#  
mt663 <- fetchNASIS(rmHzErrors = TRUE)

mt663ext <- get_extended_data_from_NASIS_db()

soilDB.env

save(mt663, mt663ext, soilDB.env, file =  "../data/book/02/mt663.rda")
