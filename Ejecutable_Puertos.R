###########################################################################################
###       Script para obtener datos en forma automática desde DIRECTEMAR (web)          ###
###          "Información en tiempo real sobre el estado de los puertos"                ###
###                                                                                     ###
###                             < Amadeo Guzmán >                                       ###
###                       última mdificación: 21-06-2018                                ###
###########################################################################################


#Librerias
library(XML)
library(lubridate)

#=====================================================================================
### Opción cargando SOLO LA TABLA QUE NECESITAMOS - TABLA 3
#(revisar cada cierto tiempo que no cambie el sistemade tablas en la pagina web)
#=====================================================================================


### Webscraping desde la página de DIRECTEMAR
#------------------------------------------------

#Dirección web
url <- "http://meteoarmada.directemar.cl/site/estadopuertos/estadopuertos.html"

#Si ya sabemos que tabla queremos, vamos directo a ella para evitar cargar todas las tablas de una página web (TABLA 3)
tabla3 <- readHTMLTable(url, which = 3) 

#Creación de data frame con las columnas que necesitamos
bd.puertos<- tabla3[,c(1,2,3,5)]
names(bd.puertos)
colnames(bd.puertos) <- c("Capitania.Puerto", "Estado.Puerto", "Condicion","Horario")
head(bd.puertos)

#Ajustando formatos de fecha y creación de nuevas columnas
bd.puertos$Fecha.Completa <- parse_date_time(bd.puertos$Horario, orders="dmy HM") #esta función <parse_date_time> convierte un vector en un objeto POSIXct, especificando sus formatos en el argumento orders=""
bd.puertos$Dia <- day(bd.puertos$Fecha.Completa)
bd.puertos$Mes <- month(bd.puertos$Fecha.Completa)
bd.puertos$Año <- year(bd.puertos$Fecha.Completa)
bd.puertos$Fecha.OK <- make_date(day = bd.puertos$Dia, month = bd.puertos$Mes, year = bd.puertos$Año)
bd.puertos$Lectura <- now()



### Pasos para generar nuevo archivo .csv
#--------------------------------------------
setwd("C:/Users/aguzman/Desktop/Estado de puertos - R")
BD <- read.csv2("BD_Estado_Puertos.csv", header = TRUE)
BD2 <- BD[,-1]

#Agregar en la base de datos existente los datos (filas) de la nueva lectura
BD_acum <- rbind(bd.puertos,BD2)

#Generar archivo actualizado
write.csv2(BD_acum, "BD_Estado_Puertos.csv")



## Instrucciones para Programar la tarea automaticamente en Windows
#--------------------------------------------------------------------
#Para que el archivo se ejecute como una tarea programada se tuvo que instalar previamente la libreria "taskscheduleR" y crear la tarea 
#mediante el complemento de Rstudio (Addins > Schedule R Script on Windows)
#install.packages("taskscheduleR", dependencies = TRUE)
#mas info de esta libreria en:
#https://github.com/bnosac/taskscheduleR
#https://www.rdocumentation.org/packages/taskscheduleR/versions/1.1/topics/taskscheduler_create

