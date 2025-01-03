library(aqp)
library(soilDB)

# use _PedonPC_Plus_DataDump_select (MLRA04 Bozeman)
#  User Site ID: %MT647%
#  NASIS Site: MLRA04%
#  NASIS Group: 4-MIS Pedons

mt647err <- fetchNASIS(rmHzErrors = FALSE)
mt647 <- fetchNASIS()
save(mt647, mt647err, soilDB.env, file =  "../data/book/02/mt647.rda")
