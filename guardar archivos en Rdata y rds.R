

datos_semanal <- read.csv2("BD_informe_semanal.csv")
datos_semanal



# Guardar como .rds
saveRDS(datos_semanal, file = "prueba_RDS_info_semanal.rds")

# Leer un objeto y cargarlo en memoria
bd_semanal <- readRDS(file = "./prueba_RDS_info_semanal.rds")
bd_semanal


#Guardar como .Rdata
save(datos_semanal, file = "bd_semanal_rdata_prueba.Rdata")
load("bd_semanal_rdata_prueba.Rdata")


