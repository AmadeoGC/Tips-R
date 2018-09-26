#######################################################
###  Formas de filtrar elementos en bases de datos  ###
###       diferencias entre OR (|) y AND (&)        ###
###            método clásico y dplyr               ###
###             <Amadeo Guzmán C.>                  ###
#######################################################

#datos
var1 <- c("a","b","c","d","b")
var2 <- c(1,2,4,5,15)

bdatos <- data.frame(var1, var2)
bdatos

library(dplyr)


#-----------------------------------------------------------------------
##  OR = | (usar principalmente para filtrara dentro de una columna)
#-----------------------------------------------------------------------
#método clásico
bdatos[bdatos$var1=="b" | bdatos$var1=="d",] #en una misma columna
bdatos[bdatos$var1=="b" | bdatos$var2==1,]  #en diferentes variables

#dplyr (filter)
bdatos %>% 
  filter(var1=="b" | var1=="d")

bdatos %>% 
  filter(var1=="b" | var2==1)


#--------------------------------------------------------------------
##  AND = & (filtrar datos con criterios de DIFERENTES columnas)
#--------------------------------------------------------------------
#método clásico
bdatos[bdatos$var1=="b" & bdatos$var2==15,]

#dplyr (filter)
bdatos %>% 
  filter(var1=="b" & var2==15)
#Utilizar la coma "," entre cada condición es equivalente a la expresión "&"
bdatos %>% 
  filter(var1=="b", var2==15)
