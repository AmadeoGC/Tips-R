###############################################################
###                   Libreria <dplyr>                      ###
###     manipulación y transformación de bases de datos     ###
###                 <Amadeo guzmán C.>                      ### 
###############################################################

library(dplyr)

#links de ayuda:
#https://dplyr.tidyverse.org/
#https://rpubs.com/joser/dplyr
#https://www.datacamp.com/community/tutorials/pipe-r-tutorial


#-----------------
### filter()
#-----------------
#Seleccionar las filas que cumplen con alguna condición( ==, >=, <=, <, >, !=)
?filter 

#BD
head(starwars,10)
filter(starwars, name == "Darth Vader")
filter(starwars, species == "Human")
filter(starwars, mass > 1000)
#es similar a la forma "tradicional de hacer estos filtros
starwars[starwars$name == "Darth Vader",]
starwars[starwars$species=="Human",]
starwars[starwars$mass>1000,]

# filtrar en base a multiples variables
filter(starwars, hair_color == "black" & gender == "female") #y
filter(starwars, hair_color == "black" | gender == "female") #o
# Utilizar la coma "," entre cada variable es equivalente a la expresión "&"
filter(starwars, hair_color == "black", gender == "female")


#-----------------
### select()
#-----------------
#Seleccionar un subconjunto de variables de nuestra base de datos
?select


###
#Aqui ir agregando las otras funciones de dplyr 



#---------------
# Uso de %>%
#---------------
# %>% este operador se podría traducir como "luego"... vincula lo que se va haciendo en cada linea de código
starwars %>%  
  filter(homeworld=="Naboo")

# determinar el promedio de altura de los habitantes de Naboo por genero
starwars %>%  
  filter(homeworld=="Naboo") %>% 
  group_by(gender) %>% 
  summarise(total= mean(height)) 

# determinar el promedio de altura de los humanos
starwars %>%  
  filter(species=="Human") %>% 
  group_by(gender) %>% 
  summarise(total= mean(height,na.rm = TRUE))

# determinar el promedio de edad de los droides
starwars %>%  
  filter(species=="Droid") %>% 
  summarise(total= mean(birth_year,na.rm = TRUE))

