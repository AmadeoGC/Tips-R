#######################################################
###  Formas de filtrar elementos en bases de datos  ###
###       diferencias entre OR (|) y AND (&)        ###
###           método clásico y dplyr                ###
###            <Amadeo Guzmán C.>                   ###
#######################################################

#datos
var1 <- c("a","b","c","d","b")
var2 <- c(1,2,4,5,15)

bdatos <- data.frame(var1, var2)
bdatos


library(dplyr)

## Filtrar datos dentro de una columna -> OR = |
#método clásico
bdatos[bdatos$var1=="b" | bdatos$var1=="d",]
#dplyr
bdatos %>% 
  filter(var1=="b" | var1=="d")


## Filtrar datos con criterios de diferentes columnas  -> AND = &
#método clásico
bdatos[bdatos$var1=="b" & bdatos$var2==15,]
#dplyr
bdatos %>% 
  filter(var1=="b" & var2==15)

