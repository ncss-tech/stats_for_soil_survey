library(aqp)
library(soilDB)

# use _PedonPC_Plus_DataDump_select (MLRA04 Bozeman)
#  User Site ID: %MT647%
#  NASIS Site: MLRA04%
#  NASIS Group: 4-MIS Pedons

mt647 <- fetchNASIS(rmHzErrors = FALSE)
save(mt647, soilDB.env, file =  "../data/book/02/mt647.rda")
zip("../data/book/02/mt647.zip", "../data/book/02/mt647.rda")
