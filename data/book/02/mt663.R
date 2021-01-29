library(aqp)
library(soilDB)

mt663 <- fetchNASIS(rmHzErrors = FALSE)

mt663ext <- get_extended_data_from_NASIS_db()

soilDB.env

save(mt663, mt663ext, soilDB.env, file =  "data/book/02/mt663.rda")
zip("data/book/02/mt663.zip", "data/book/02/mt663.rda")
