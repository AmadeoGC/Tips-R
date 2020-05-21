
# LINKs
#https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html


#install.packages(c("naniar","visdat"))

library(naniar) #Trabajar con datos faltantes 'NA'
library(visdat) #Visualizar los datos y estructura de manera general
library(tidyverse)
library(DataExplorer) #Para hacer tabla de resumen de datos faltantes



#
# VISDAT ----
#

# Graficos con la visión general de cada variable
vis_dat(airquality)
vis_dat(iris)

vis_miss(airquality) #Para ver especificamente datos faltantes 'NA'
vis_miss(iris)

#vis_expect() visualiza ciertas condiciones o valores en sus datos.
#Este ejemplo muestra la proporción de veces que hay valores superiores, inferiores o igual a X valor, así como los datos faltantes.
vis_expect(airquality, ~.x <= 5)




#
# NANIAR ==========================================================================================================================
#

# Resumen númerico de valores flatantes ------------------------------------------------------
n_miss(airquality)
n_miss(airquality$Ozone)

n_complete(airquality)
n_complete(airquality$Ozone)

prop_miss_case(airquality) #proporción de datos faltantes
pct_miss_case(airquality) #Porcentaje de datos faltantes

#RESUMEN de datos faltantes x variable 
miss_var_summary(airquality)

#se puede incorporar a la gramatica de tidyverse y explorar a diferentes niveles
airquality %>% 
  group_by(Month) %>% 
  miss_var_summary()

airquality %>% 
  group_by(Month) %>% 
  miss_var_summary() %>% 
  filter(variable =="Ozone") 


#identificación de las filas con datos faltantes "casos"
miss_case_summary(airquality)
miss_case_table(airquality) #tabla resumen con cada "caso" (el nivel de información es por fila)
gg_miss_upset(airquality) #la tabla anterior se puede detallar aún más con el siguiente gráfico de intersección



# Graficos de datos faltantes x variable -------------------------------------------------
gg_miss_var(airquality)
gg_miss_var(airquality) + theme_bw() 
gg_miss_var(airquality, facet = Month) + theme_light() #se pueden usar facets para ver distribución de na en diferentes categorias de una vriable (en este caso en la variable 'Month')


# Como graficar datos faltantes en un gráfico de dispersión... para no perder la información parcial disponible
ggplot(airquality,  aes(x = Solar.R, y = Ozone)) + 
  geom_miss_point(size=2)

ggplot(airquality, aes(x = Solar.R, y = Ozone)) + 
  geom_miss_point(size = 2) + 
  facet_wrap(~Month)

gg_miss_upset(airquality)


# Explorar en detalle los valores NA y ver como se distribuyen sus datos en otras variables
#en este ejemplo se muestra un resumen estadistico de la variable Solar.R en lo datos observados y faltantes de Ozone
#la función 'bind_shadow()' crea variables auxiliares (dummy) de datos faltantes y observados (NA y !NA = no NA)
airquality %>%
  bind_shadow() %>%
  group_by(Ozone_NA) %>%
  summarise_at(.vars = "Solar.R",
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)



# Imputar valores faltantes y visualizarlos ---------------------------------------------

#install.packages("simputation")
library(simputation)

#normal
airquality %>%
  ggplot(aes(x = Temp,
             y = Ozone)) + 
  geom_point()+
  scale_y_continuous(limits = c(-10,200))

#Imputando por reg lineal
airquality %>%
  impute_lm(Ozone ~ Temp + Wind) %>%
  ggplot(aes(x = Temp,
             y = Ozone)) + 
  geom_point() +
  scale_y_continuous(limits = c(-10,200))


#colorear datos imputados
bind_shadow(airquality) %>% 
  as.data.frame() %>% 
  impute_lm(Ozone ~ Temp + Wind) %>%
  ggplot(aes(x = Temp,
             y = Ozone,
             colour = Ozone_NA)) + 
  geom_point(size=2)




