
library(duckplyr)

#Acessar a base SIM
con <- duckdb::dbConnect(duckdb::duckdb(),
                 dbdir = paste0(dirname(getwd()),"/bases/sim/duckdb/sim_1996_2024.duckdb"), #Nome do database que armazena o SIH
                 read_only = FALSE)
DBI::dbListTables(con)
data <- tbl(con, "sim_br")



#Extração de base, mortes por causa externa
data |>
  filter(ano %in% c(2014:2024) & 
       intencao_homic %in% c("Homicídio","Indeterminado", 
                             "Acidente", "Suicídio") ) |>
  collect() -> sim_doext
DBI::dbDisconnect(con) ; gc()

rm(list = setdiff(ls(),"sim_doext") ); gc()

#Exportando.
save.image(dirname(getwd(),"/bases/sim/RData/sim_doext_14_24.Rdata")); gc()
