#######################################################################################+
###       EVALUACIÓN DE CONCESIONES: ANALISIS DE CLUSTER - PCA - A. Riesgo          ####
###                       Empresa: CERMAQ | Especie: Salar                          ###+
###                              <Amadeo Guzmán>                                    ###+
###                   Fecha última actualización: 20-02-2020                        ###+
#######################################################################################+


###
### Librerias generales------------------------------------------------------------------------------------------------------------------------
###

library(tidyverse)
library(readxl)
library(janitor)
library(VIM) #para funcion aggr() -> ver datos faltantes
library(ggpubr)
library(ggtext)
library(scales)



###
### Cargar BBDD, verificar info y hacer transfomraciones necesarias ----------------------------------------------------------------------------
###

datos <- read.csv2("BD_salar_para_cluster.csv", header=TRUE, row.names=1) %>% 
  clean_names()

head(datos)
glimpse(datos)

#crear nueva BD seleccionando las variables a utilizar en análisis de clister
(datos_ok <- datos %>% 
  select(lapso_engorda, mort_acum, sgr, gf3, kg_cosecha_smolt, gr_ab_ton_cosecha, banos_caligus)
)

aggr(datos_ok) #rev. datos faltants (NA)

# estandarizar datos (media 0 y desv. est. 1)
datos_est<- scale(datos_ok)  
datos_est



########################################################+
###
###       ETAPA 1: ACP y Análisis de CLUSTERS     ============================================================================================================================
###
########################################################+

###
### ...1] Librerias especificas ---- 
###

library(clustertend)
library(factoextra) # clustering algorithms & visualization
library(cluster)    # clustering algorithms & funcion daisy() para calcular distancias
library(clValid)
library(NbClust)
library(fpc) #funciones para obtener indices de evaluación de clusters



###
### ...2] Estadístico de Hopkins. Tiene sentido aplicar un análisis Cluster? ------------------------------------------------------------------------
###

# Valores cercanos a 0 indica que tiene sentido aplicar clustering. valores cercanos a 0.5 indica que 
# tienden a distribuir uniforme  y que no tiene sentido hacer cluster.

hopkins(data = datos_est, n = nrow(datos_est) - 1)

replicate(10, hopkins(data = datos_est, n = nrow(datos_est) - 1)) #probar 10 veces con diferentes subconjuntos



###
### ...3] Obteniendo la matriz de distancias ---------------------------------------------------------------------------------------------------------
###

distance1 <- daisy(datos_est, metric ="euclidean")
head(distance1)
summary(distance1)
str(distance1)
class(distance1)

distance_mat1 <- as.matrix(distance1)
distance_mat1
summary(distance_mat1)
class(distance_mat1)



#otra funcion para calcular distancias
res.dist <- get_dist(datos_est, stand = FALSE, method = "euclidean")



###
### ...4]. Hay indicios de una agrupación subyacente? --------------------------------------------------------------------------------------------------
###

# Visualización de una posible tendencia de Clusters
# In the plot below, similar objects are close to one another. check legend with distance.

(cor_dist <- fviz_dist(dist.obj = distance1, show_labels = TRUE, 
                       gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
  labs(title = "Matriz de correlaciones - Distancias por centro / ciclo de cultivo",
       fill="Distancias") + 
  theme(legend.position = "right",
        axis.text = element_text(size=6))
)

ggsave("cor_dist.png", cor_dist, units = "cm", dpi=500, width = 27, height = 16)


'
#gráfico de distancias obtenidas con la función get_dist()  -> se obtiene el mismo resultado que con función daisy()
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) #se obtiene el mismo resultado
'



###
### ...5] Determinando la cantidad óptima de clusters y método a elegir -------------------------------------------------------------------------------
###

#prueba método 1 - paquete clValid
intern <- clValid(datos_est, nClust = 2:10, 
                  clMethods = c("hierarchical","kmeans","pam",'clara'),
                  validation = "internal")
summary(intern)
#plot(intern)


#prueba método 2 - paquete NbClust link -> https://degreesofbelief.roryquinn.com/clustering-analysis-r-part-1
res.nbclust <- NbClust(datos_est, distance = "euclidean",
                       min.nc = 2, max.nc = 10, 
                       method = "complete", index ="all") 

(num_cluster_index <- factoextra::fviz_nbclust(res.nbclust))

ggsave("num_cluster_index.png", num_cluster_index, units = "cm", dpi=500, width = 16, height = 12)

res.nbclust$Best.nc


## Pruebas definitivas para seleccionar el número de clusters: Silhouette & Elbow
# Silhouette method: n° óptimo de clusters valor más alto,
# corresponde al ancho máximo de la silueta
# Cuán buena es la asignación que se ha hecho de una observación comparando su similitud
# con el resto de observaciones de su cluster frente a las de los otros clusters
(psil<-fviz_nbclust(datos_est, pam, method = "silhouette"))
fviz_nbclust(datos_est, hcut, method = "silhouette")
fviz_nbclust(datos_est, kmeans, method = "silhouette")
fviz_nbclust(datos_est, clara, method = "silhouette")

# Elbow method: n° óptimo donde se "dobla" la curva, 
# corresponde a la menor suma de cuadrados/variación intracluster
# la curva se "quiebra" cuando añadir más cluster no sigue reduciendo la var. intracluster
(pelbow<-fviz_nbclust(datos_est, pam, method = "wss"))
fviz_nbclust(datos_est, hcut, method = "wss")
fviz_nbclust(datos_est, kmeans, method = "wss")
fviz_nbclust(datos_est, clara, method = "wss")

(num_cluster_elbow_sil <- ggarrange(psil,pelbow, ncol=1))

ggsave("num_cluster_elbow_sil.png", num_cluster_elbow_sil, units = "cm", dpi=500, width = 16, height = 12)




###
### ...6] Aplicando PCA y métodos de clustering -------------------------------------------------------------------------------------------------------
###

#
# ... - 6.1. Primero PCA ----
#

#Para conocer la relación que existe entre las variabls evaluadas

#Ejecutar PCA
pca_datos <- prcomp(datos_est)
summary(pca_datos)

#Aporte de cada componnete a la explicación de la varianza total
(pca_varianza <- fviz_eig(pca_datos,
                          barfill = c("steelblue","steelblue","grey70","grey70","grey70","grey70","grey70"),
                          barcolor = c("steelblue","steelblue","grey70","grey70","grey70","grey70","grey70")) +
        theme(axis.line = element_line(size=.5))
)

ggsave("pca_varianza.png", pca_varianza, units = "cm", dpi = 300, width = 15, height = 10)



#Gráfico de variables (circulo de correlaciones)
fviz_pca_var(pca_datos)
fviz_pca_contrib(pca_datos, choice = c("var"), 1)
fviz_pca_contrib(pca_datos, choice = c("var"), 2)

(pca_variables <- fviz_pca_var(pca_datos,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +     # Avoid text overlapping
    theme(panel.grid = element_blank(),
          axis.line.x = element_line(size=.5))
)

ggsave("pca_variables.png", pca_variables, units = "cm", dpi = 400, width = 16, height = 11)


#Gráfico de individuos - No ocupar (por ahora)
fviz_pca_ind(pca_datos)



#
##
####  ... - 6.2. PAM ==========
##
#

#PAM cluster
(pam_datos <- pam(datos_est, k = 2)
)

pam_datos$medoids
pam_datos$id.med
pam_datos$clustering
pam_datos$objective
pam_datos$isolation
pam_datos$clusinfo
pam_datos$silinfo
pam_datos$diss
pam_datos$call
pam_datos$data

# Gráfico con agrupación de datos para primeras dos dimensiones
(pam_grafico <- fviz_cluster(object = pam_datos, data = datos_est,
                   ellipse.type = "norm", geom = "point", main = "Datos CERMAQ - Visualización Cluster",
                   stand = FALSE, 
                   palette = c("gold3","steelblue","grey40"),
                   addEllipses=TRUE, 
                   ellipse.level=0.7) +
  theme_bw() + 
  theme(legend.position = "bottom"))

#sin elipse
fviz_cluster(object = pam_datos, data = datos_est,
             main = "Datos CERMAQ - Visualización Cluster",
             stand = FALSE, 
             palette = c("gold3","steelblue","grey40")) +
  theme_bw() + 
  theme(legend.position = "bottom")


#sin elipse y nombre
fviz_cluster(object = pam_datos, data = datos_est,
             geom = "point", 
             main = "Datos CERMAQ - Visualización Cluster",
             stand = FALSE, 
             palette = c("gold3","steelblue","grey40")) +
  theme_bw() + 
  theme(legend.position = "bottom")



### Biplot (con las dos primeras componentes del PCA) 
#con nombres de centro
fviz_pca_biplot(pca_datos,
                title="Variables y Clusters",
                #label="var", 
                habillage = as.factor(pam_datos$clustering),
                palette=c("gold3","steelblue","grey40"), 
                col.var="black")

#sin nombres de centro
fviz_pca_biplot(pca_datos,
                title="Variables y Clusters",
                label="var",
                repel = TRUE,
                habillage = as.factor(pam_datos$clustering),
                palette=c("gold3","steelblue","grey40"), 
                col.var="black")



### ................6.2.1. Validación PAM ----

### Silhouette

# Valores cercanos a cero hablan de observaciones que están en las
# fronteras de ambos clusters, solapadas.

fviz_silhouette(sil.obj = pam_datos, print.summary = TRUE, palette = c("gold3","steelblue","grey40"),
                ggtheme = theme_classic()) 


# Silhouette width of observations
sil <- pam_datos$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]


### Índice Dunn

# (Separación mín interclust / separación máxima intracluster)
# Se busca maximizar el indicador, numerador alto y denominador bajo
pam_indices <- cluster.stats(d = dist(datos_est, method = "euclidean"), 
                             clustering = pam_datos$clustering)

pam_indices$average.within
pam_indices$average.between
pam_indices$dunn




#
##
#### ... - 6.3. CLUSTER JERARQUICO (HCLUST ) ==========
##
#

# obtener distancias
d <- dist(datos_est, method = "euclidean")
# Hierarchical clustering using Ward's method
(res.hc <- hclust(d, method = "ward.D2" )
)

# Cut tree into 2 groups
grp <- cutree(res.hc, k = 2)
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 2, border = 2:5) # add rectangle


# lo mismo pero con mejor visualización
# Compute hierarchical clustering and cut into 4 clusters
(res <- hcut(datos_est, k = 2, stand = TRUE)
)

res$cluster
res$merge
res$height
res$order
res$labels
res$method
res$call
res$dist.method
res$cluster
res$nbclust
res$silinfo
res$size


# Vis 1
(res_2 <- fviz_dend(res, rect = TRUE, cex = 0.5,
                   k_colors = c("gold3","steelblue"))
)

#vis 2
fviz_dend(res, cex = 0.5, k = 2,
          k_colors = "jco", type = "circular")


#PCA
pca_BD.Aqch.selec <- prcomp(datos_est)
pca_BD.Aqch.selec
summary(pca_BD.Aqch.selec)

fviz_pca_var(pca_BD.Aqch.selec)
fviz_pca_contrib(pca_BD.Aqch.selec, choice = c("var"), 1)
fviz_pca_contrib(pca_BD.Aqch.selec, choice = c("var"), 2)


fviz_pca_ind(pca_BD.Aqch.selec)



fviz_pca_biplot(pca_BD.Aqch.selec, title="Variables y Clusters",label="var", habillage = as.factor(res$cluster),
                palette=c("gold3","steelblue","grey40"), col.var="black")



### ................6.3.1. Validación HCLUST ----

### Silhouette

fviz_silhouette(sil.obj = res, print.summary = TRUE, palette = c("gold3","steelblue","grey40"),
                ggtheme = theme_classic()) 

### Índice Dunn

hclust_indices <- cluster.stats(d = dist(datos_est, method = "euclidean"), 
                                clustering = res$cluster)

hclust_indices$average.within
hclust_indices$average.between
hclust_indices$dunn





#
##
#### ... - 6.4. KMEANS ==========
##
#

#VER ESTE LINK --> https://rpkgs.datanovia.com/factoextra/reference/eclust.html

(res.km <- eclust(datos_est, "kmeans", 2, graph = TRUE)
)

res.km$cluster
res.km$centers
res.km$totss
res.km$withinss
res.km$tot.withinss
res.km$betweenss
res.km$size
res.km$iter
res.km$ifault
res.km$clust_plot
res.km$silinfo
res.km$nbclust


(kmeans_cluster <- fviz_cluster(object = res.km, data = datos_est,
             stand = FALSE, 
             palette = c("gold3","steelblue","grey40"),
             labelsize = 8,
             repel = TRUE,
             show.clust.cent = TRUE) +
  labs(title="Identificación de clusters en la empresa Cermaq",
       subtitle="") +
  theme_minimal() + 
  theme(axis.line = element_line(size=.7),
        axis.ticks = element_line(size=.6)) 
)

ggsave("kmeans_cluster.png", kmeans_cluster, units = "cm", dpi=600, width = 28, height = 13)


'fviz_cluster(object = res.km, data = datos_est,
             geom = "point", 
             main = "Datos CERMAQ - Visualización Cluster",
             stand = FALSE, 
             palette = c("gold3","steelblue","grey40")) +
  theme_bw() + 
  theme(legend.position = "bottom")
'

(biplot_kmeans_pca <- fviz_pca_biplot(pca_datos,
                title="Identificación de Clusters y Componentes Principales",
                stand = FALSE,
                label="var", 
                habillage = as.factor(res.km$cluster),
                palette=c("gold3","steelblue","grey40"), 
                col.var="grey40",
                repel = TRUE) +
    labs(subtitle="Ciclos cerrados de salmón del Atlántico en la empresa **CERMAQ**",
         color="Clusters",
         fill="Clusters",
         shape="Clusters") +
    theme(plot.title = element_text(size=15, face="bold", color = "grey25"),
          panel.grid = element_line(linetype = 3),
          axis.line = element_line(color = "grey60"),
          legend.position = "none",
          plot.subtitle = element_markdown()) +
    geom_label(aes(x =  2.85, y = 2, label = "Cada punto representa \n1 centro/ciclo de cultivo. \n\nUn centro puede tener entre \n1 y 3 ciclos."),  
               hjust = 0, vjust = 0.5, 
               lineheight = 1, label.size = NA, size = 3.5, color="grey60") +
    geom_curve(aes(x = 2.8, y = 2.26, xend = 1.95, yend =2.05 ), colour = "grey65", 
               curvature = 0.2, size=0.3,
               arrow = arrow(length = unit(0.015, "npc")))
)

ggsave("biplot_kmeans_pca.png", biplot_kmeans_pca, units = "cm", dpi=600, width = 28, height = 14)




#biplot con geom_polygon para IMPRIMIR - hacer cuadro final con la descripción de cada variable (usar paquete  patchwork())
(biplot_kmeans_pca_polygon <- fviz_pca_biplot(pca_datos,
                                      title="Identificación de Clusters y Componentes Principales",
                                      stand = FALSE,
                                      label="var", 
                                      habillage = as.factor(res.km$cluster),
                                      palette=c("gold3","steelblue","grey40"), 
                                      col.var="grey40",
                                      repel = TRUE) +
    geom_polygon(stat = "density_2d", alpha = 0.05, aes(fill=as.factor(res.km$cluster)), colour = NA) +
    labs(subtitle="Ciclos cerrados de salmón del Atlántico en la empresa **CERMAQ**",
         color="Clusters",
         fill="Clusters",
         shape="Clusters") +
    theme(plot.title = element_text(size=15, face="bold", color = "grey25"),
          panel.grid = element_line(linetype = 3),
          axis.line = element_line(color = "grey60"),
          axis.ticks = element_line(color = "grey60"),
          legend.position = "none",
          plot.subtitle = element_markdown()) +
    geom_label(aes(x =  2.85, y = 2, label = "Cada punto representa \n1 centro/ciclo de cultivo. \n\nUn centro puede tener entre \n1 y 3 ciclos."),  
               hjust = 0, vjust = 0.5, 
               lineheight = 1, label.size = NA, size = 3.5, color="grey60") +
    geom_curve(aes(x = 2.8, y = 2.26, xend = 1.95, yend =2.05 ), colour = "grey65", 
               curvature = 0.2, size=0.3,
               arrow = arrow(length = unit(0.015, "npc")))
)


### ................6.4.1. Validación KMEANS----

### Silhouette
fviz_silhouette(res.km)
fviz_silhouette(sil.obj = res.km, print.summary = TRUE, palette = c("gold3","steelblue","grey40"), 
                ggtheme = theme_classic())


sil_2 <- res.km$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index_2 <- which(sil_2[, 'sil_width'] < 0)
sil_2[neg_sil_index_2, , drop = FALSE]


kmeans_indices2 <- cluster.stats(d = dist(datos_est, method = "euclidean"), 
                                clustering = res.km$cluster)

kmeans_indices2$average.within
kmeans_indices2$average.between
kmeans_indices2$dunn






###
### ...7] Comparación de modelos ---------------------------------------------------------------------------------------------------------------------
###

cbind(pam_indices, hclust_indices, kmeans_indices2)



###
### ...8] Asignar valores de cluster a BBDD original -------------------------------------------------------------------------------------------------
###


### Nueva BBDD

# juntar los resultados de los diferentes metodlogias en un DF
(res_clusters <- data.frame(cbind(pam_datos$clustering, res$cluster, res.km$cluster)) %>% 
  rename(PAM_res=X1, HCLUST_res=X2, KMEANS_res=X3)
)

class(res_clusters)

#
# ... - 8.1. Unir con BBDD original ----
#

bd_cluster_final <- data.frame(cbind(datos, res_clusters))

head(bd_cluster_final, 10)


### Categorizar centros segun sus resultados ----
(x <- bd_cluster_final %>% 
  group_by(KMEANS_res, centro_codificado, centro) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = KMEANS_res, values_from = n) %>% 
  clean_names() %>% 
  rename(c1=x1, c2=x2) %>% 
  mutate(c1 = ifelse(is.na(c1),0,c1),
         c2 = ifelse(is.na(c2),0,c2),
         total = c1 + c2) %>% 
  arrange(desc(total)) 
)

(x2 <- x %>% 
    rename(ciclos_c1 = c1,
           ciclos_c2 = c2) %>% 
  mutate(prop_c1 = ciclos_c1 / total,
         prop_c2 = ciclos_c2 / total,
         cat_centro = case_when(
           prop_c1 > prop_c2 ~ "Cat. B",
           prop_c1 < prop_c2 ~ "Cat. A",
           prop_c1 == prop_c2 ~ "Cat. Mix"
           )
         )
)


(tipo_centro_graf <- x2 %>% 
  group_by(cat_centro) %>% 
  count() %>% 
   #gráfico
  ggplot(aes(cat_centro, n, fill=cat_centro, color=cat_centro)) +
  geom_col(alpha=.75) +
  geom_label(aes(label=n, color=cat_centro), fill="white") +
  scale_fill_manual(values = c("steelblue", "gold3", "grey60")) +
  scale_color_manual(values = c("steelblue", "gold3", "grey60")) +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0,25)) +
  labs(title="Tipo de centro - Salar",
       subtitle="Centros de cultivo de la empresa CERMAQ",
       caption ="Clasificación en base a resultados de análisis de cluster",
       x="\nTipo de centro",
       y="centros (n°)\n") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "none",
        plot.caption = element_text(color = "grey70", size=8))
)

ggsave("tipo_centro_graf.png", tipo_centro_graf, units = "cm", dpi=300, width = 21, height = 11)




#generar archivos según tipo de centro
(cat_A <- x2 %>% 
  filter(cat_centro =="Cat. A") %>% 
    data.frame())
knitr::kable(cat_A)


(cat_B <- x2 %>% 
    filter(cat_centro =="Cat. B") %>% 
    data.frame())
knitr::kable(cat_B)


(cat_Mix <- x2 %>% 
    filter(cat_centro =="Cat. Mix") %>% 
    data.frame())
knitr::kable(cat_Mix)


'library(openxlsx)
header_style <- createStyle(halign = "center", textDecoration = "bold")
wb <- createWorkbook()

addWorksheet(wb, "Data")
writeData(wb, "Data", cat_A, headerStyle = header_style)
freezePane(wb, "Data", firstRow = TRUE)
setColWidths(wb, "Data", cols= 1:ncol(cat_A), widths = "auto")
saveWorkbook(wb, file = "cat_a.xlsx", overwrite = TRUE)
'

#
# xlsx.writeMultipleData - Función para generar excel con resultados en varias pestañas por tipo de centro (A, B y MIX)
#link -> http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r

# file : the path to the output file
# ... : a list of data to write to the workbook
library(xlsx)

xlsx.writeMultipleData <- function (file, ...) {
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
}

xlsx.writeMultipleData("RESULTADOS_tipo_de_centro_salar_desde_R.xlsx",
                       cat_A, cat_B, cat_Mix)



### unir base de datos original con la de centros categorizados ----
(bd_cluster_salar_excel <- bd_cluster_final %>% 
  left_join(x2, by="centro_codificado") %>% 
  select(-c(centro.y, ciclos_c1, ciclos_c2)) %>% 
  rename(total_ciclos_eval = total)
)


### Generar excel FINAL con resultados de cluster y bbdd original
write.xlsx(bd_cluster_salar_excel, "RESULTADOS_bdfull_cluster_salar_desde_R.xlsx")





###
### ...9] Descripción de clusters  -------------------------------------------------------------------------------------------------------------------
###

### Descripción genral de los clusters

# Gráfico de barras
(descrip_gral_clusters <- bd_cluster_final %>% 
  group_by(KMEANS_res) %>% 
  summarize(sgr= mean(sgr),
            gf3= mean(gf3),
            kg_cosecha_smolt= mean(kg_cosecha_smolt),
            gr_ab_ton_cosecha= mean(gr_ab_ton_cosecha),
            lapso_engorda= mean(lapso_engorda),
            baños_caligus= mean(banos_caligus),
            mortalidad_acum = mean(mort_acum)*100
            ) %>% 
  pivot_longer(-KMEANS_res, names_to = "variables", values_to = "resultados") %>% 
  ungroup() %>% 
  ggplot(aes(as.factor(KMEANS_res), resultados, fill=as.factor(KMEANS_res), color=as.factor(KMEANS_res)))+
  geom_col(alpha=.8) +
  geom_label(aes(label=round(resultados,2)), fill="white")+
  scale_fill_manual(values = c("gold3","steelblue")) +
  scale_color_manual(values = c("gold3","steelblue")) +
  facet_wrap(~variables, scales = "free_y", ncol=4)+
  labs(x="Clusters",
       y="resultados por variable",
       color="Cluster",
       fill="Cluster") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        strip.background.x = element_rect(fill = "grey70", color = "grey95"),
        strip.text.x = element_text(color="white", size=11),
        axis.line.x = element_line(color = "grey30"),
        axis.ticks.x = element_line(color = "grey30"),
        legend.position = "none")
)

ggsave("descrip_gral_clusters.png", descrip_gral_clusters, units = "cm", dpi = 350, width = 28, height = 12)



# Boxplot
(descrip_clusters_boxplot <- bd_cluster_final %>% 
  select(KMEANS_res, sgr,gf3,kg_cosecha_smolt,gr_ab_ton_cosecha,lapso_engorda,banos_caligus,mort_acum) %>% 
  mutate(mort_acum = mort_acum*100) %>% 
  group_by(KMEANS_res) %>% 
  pivot_longer(-KMEANS_res, names_to = "variables", values_to = "resultados") %>% 
  ungroup() %>% 
  ggplot(aes(as.factor(KMEANS_res), resultados, fill=as.factor(KMEANS_res)))+
  geom_boxplot(outlier.colour = NA, alpha=.8) +
  geom_jitter(shape=21, size=3, alpha=.35, width = 0.2) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  facet_wrap(~variables, scales = "free", ncol=4) +
  labs(x="\nClusters",
         y="resultados por variable\n") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.background.x = element_rect(fill = "grey70", color = "grey60"),
        strip.text.x = element_text(color="white", size=11),
        axis.line.x = element_line(color = "grey30"),
        axis.ticks.x = element_line(color = "grey30")
  )
)


ggsave("descrip_clusters_boxplot.png", descrip_clusters_boxplot, units = "cm", dpi = 400, width = 28, height = 12)



# Gráfico de DENSIDAD - Usar en versión final para IMPRIMIR
(plot_cluster_variable <- bd_cluster_final %>% 
  select(KMEANS_res, sgr,gf3,kg_cosecha_smolt,gr_ab_ton_cosecha,lapso_engorda,banos_caligus,mort_acum) %>% 
  group_by(KMEANS_res) %>% 
  pivot_longer(-KMEANS_res, names_to = "variables", values_to = "resultados") %>% 
  ungroup() %>% 
  ggplot(aes(resultados, fill=as.factor(KMEANS_res), color=as.factor(KMEANS_res)))+
  geom_density(alpha = 0.6, colour = "black") +
  geom_rug() +
  scale_fill_manual(values = c("gold3","steelblue")) +
  scale_color_manual(values = c("gold3","steelblue")) +
  facet_wrap(~variables, scales = "free", ncol = 3) +
  labs(title="Resultados productivos según cluster",
       subtitle = "Ciclos del <span style='color:steelblue'>**Cluster 2**</span> se asocian a mejores resultados productivos y sanitarios  
       en comparación a los del <span style='color:gold3'>**Cluster 1**</span>",
       y="",
       x="\nEn el **Eje X** se muestran los valores de cada variable evaluada\n",
       fill="Clusters",
       color="Clusters") +
  theme_minimal()+
  theme(panel.grid = element_line(linetype = 3),
        #strip.background.x = element_rect(fill = "grey97", color = "grey95"),
        strip.text.x = element_text( size=10),
        axis.line = element_line(color = "grey60"),
        axis.text.y = element_text(size=7),
        plot.title = element_text(size=15, face="bold", color = "grey25"),
        plot.subtitle = element_markdown(size=10, color = "grey30"),
        #axis.title.x = element_text(size=10, color = "grey40"),
        axis.title.x = element_markdown(size=10, color = "grey40"),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.direction = "horizontal")
)


###
### ... - 9.1. Gráfico de resumen final - Versión para INFORME y para IMPRIMIR ----
###

library(patchwork)

# Versión para informe
(gráfico__PCA_CLUSTER_descr_FINAL <- (biplot_kmeans_pca_polygon + plot_cluster_variable))
ggsave("gráfico__PCA_CLUSTER_descr_FINAL.png", gráfico__PCA_CLUSTER_descr_FINAL, units = "cm", dpi = 700, width = 33, height = 14)


# Versión para imprimir
(gráfico__PCA_CLUSTER_descr_FINAL_imprimir <- (plot_cluster_variable / biplot_kmeans_pca_polygon) +  plot_layout(nrow = 2, heights = c(1.3, 2)))
ggsave("gráfico__PCA_CLUSTER_descr_FINAL_imprimir.png", gráfico__PCA_CLUSTER_descr_FINAL_imprimir, units = "cm", dpi = 700, width = 19, height = 25)




###
### ... - 9.2. Descripción de cluster por año de cierre ----
###

# Participación porcentual de clustr por año de cierre

(cluster_año_gral <- bd_cluster_final %>% 
  group_by(ano_cierre_final, KMEANS_res) %>%
  summarize(n = n()) %>% 
  mutate(Proporcion = n/sum(n),
         position_text_y = cumsum(Proporcion)-Proporcion/2,
         texto = str_glue("{scales::percent(Proporcion)}
                           n: {n}")) %>% 
  ggplot(aes(ano_cierre_final, n, fill=as.factor(KMEANS_res))) +
  geom_col(position = "fill", alpha=0.75) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  scale_color_manual(values = c("gold3","steelblue")) +
  labs(title="Participación de cada cluster según año de cierre",
       subtitle="Salar - Ciclos cerrados", 
       x="\nAño cierre",
       y="Proporción (%)\n",
       fill="Cluster",
       color0="Cluster") +
  scale_y_continuous(labels = percent, expand = c(0, 0.04))+
  scale_x_continuous(breaks = seq(2013, 2020,1))+
  theme_minimal() +
  theme(axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "right",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.x = element_rect(fill = "grey70", color = "grey60"),
        strip.text.x = element_text(color="white", size=11)) +
  geom_label(aes(label = texto), position="fill",alpha=.9, color= "white", size=3.5)
)

ggsave("cluster_año_gral.png", cluster_año_gral, units="cm", dpi=300, width=25, height=14)





'bd_cluster_final %>% 
  ggplot(aes(as.factor(ano_cierre_final), fill=as.factor(KMEANS_res), color=as.factor(KMEANS_res))) +
  geom_bar(alpha=.8)+
  #geom_label(aes(label=round(resultados,2)), fill="white")+
  scale_fill_manual(values = c("gold3","steelblue")) +
  scale_color_manual(values = c("gold3","steelblue")) +
  scale_y_continuous(breaks = seq(0,50,2), expand = c(0.01, 0.01))+
  facet_wrap(~KMEANS_res) +
  theme_light() +
  theme(#axis.line.x = element_line(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.x = element_rect(fill = "grey70", color = "grey60"),
        strip.text.x = element_text(color="white", size=11))
'



# Gráfico resumen de resultados x año de cierre (todas las variables juntas) - Exploratorio - NO USAR
bd_cluster_final %>% 
  select(ano_cierre_final, KMEANS_res, sgr,gf3,kg_cosecha_smolt,gr_ab_ton_cosecha,lapso_engorda,banos_caligus, mort_acum) %>% 
  group_by(ano_cierre_final, KMEANS_res) %>% 
  pivot_longer(-c(KMEANS_res, ano_cierre_final), names_to = "variables", values_to = "resultados") %>% 
  ungroup() %>% 
  ggplot(aes(as.factor(KMEANS_res), resultados, fill=as.factor(KMEANS_res)))+
  geom_boxplot(outlier.colour = NA, alpha=.7) +
  geom_jitter(shape=21, size=3, alpha=.5, width = 0.2) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  facet_grid(variables~ano_cierre_final, scales = "free",space = "free_x") +
  labs(title="SGR",
       x="\n Clusters",
       y="SGR\n ") +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(color="white", size=11),
        axis.ticks.x = element_line(color = "grey30"))



# Grafico individual por variable

#SGR x año de cierre
(sgr_año <- bd_cluster_final %>% 
  ggplot(aes(as.factor(KMEANS_res), sgr, fill=as.factor(KMEANS_res)))+
  geom_boxplot(outlier.colour = NA, alpha=.7) +
  geom_jitter(shape=21, size=3, alpha=.5, width = 0.2) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  facet_grid(~ano_cierre_final, scales = "free",space = "free_x") +
  labs(title="SGR",
       x="\n Clusters",
       y="SGR\n ") +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(color="white", size=11),
        axis.ticks.x = element_line(color = "grey30"))
)

ggsave("sgr_año.png", sgr_año, units="cm", dpi=300, width=25, height=12)


#GF3 x año de cierre
(gf3_año <- bd_cluster_final %>% 
  ggplot(aes(as.factor(KMEANS_res), gf3, fill=as.factor(KMEANS_res)))+
  geom_boxplot(outlier.colour = NA, alpha=.7) +
  geom_jitter(shape=21, size=3, alpha=.5, width = 0.2) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  facet_grid(~ano_cierre_final, scales = "free",space = "free_x") +
  labs(title="GF3",
       x="\n Clusters",
       y="GF3\n ") +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(color="white", size=11),
        axis.ticks.x = element_line(color = "grey30"))
)

ggsave("gf3_año.png", gf3_año, units="cm", dpi=300, width=25, height=12)


#Productividad x año de cierre
(prod_año <- bd_cluster_final %>% 
  ggplot(aes(as.factor(KMEANS_res), kg_cosecha_smolt, fill=as.factor(KMEANS_res)))+
  geom_boxplot(outlier.colour = NA, alpha=.7) +
  geom_jitter(shape=21, size=3, alpha=.5, width = 0.2) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  facet_grid(~ano_cierre_final, scales = "free",space = "free_x") +
  scale_y_continuous(limits = c(0,6))+
  labs(title="Productividad",
       x="\n Clusters",
       y="kg_cosecha_smolt\n") +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(color="white", size=11),
        axis.ticks.x = element_line(color = "grey30"))
)

ggsave("prod_año.png", prod_año, units="cm", dpi=300, width=25, height=12)


#Uso AB x año de cierre
(ab_año <- bd_cluster_final %>% 
  ggplot(aes(as.factor(KMEANS_res), gr_ab_ton_cosecha, fill=as.factor(KMEANS_res)))+
  geom_boxplot(outlier.colour = NA, alpha=.7) +
  geom_jitter(shape=21, size=3, alpha=.5, width = 0.2) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  scale_y_continuous(limits = c(-0.1,1800))+
  facet_grid(~ano_cierre_final, scales = "free",space = "free_x") +
  labs(title="Indicador uso de antibióticos",
       x="\n Clusters",
       y="gr_ab_ton_cosecha\n") +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(color="white", size=11),
        axis.ticks.x = element_line(color = "grey30"))
)

ggsave("ab_año.png", ab_año, units="cm", dpi=300, width=25, height=12)


#Baños caligus x año de cierre
(baños_año <- bd_cluster_final %>% 
  ggplot(aes(as.factor(KMEANS_res), banos_caligus, fill=as.factor(KMEANS_res)))+
  geom_boxplot(outlier.colour = NA, alpha=.7) +
  geom_jitter(shape=21, size=3, alpha=.5, width = 0.2) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  scale_y_continuous(limits = c(0,28))+
  facet_grid(~ano_cierre_final, scales = "free",space = "free_x") +
  labs(title="Baños Caligus",
       x="\n Clusters",
       y="n°\n") +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(color="white", size=11),
        axis.ticks.x = element_line(color = "grey30"))
)

ggsave("baños_año.png", baños_año, units="cm", dpi=300, width=25, height=12)


#lapso engorda x año de cierre
(lapso_año <- bd_cluster_final %>% 
  ggplot(aes(as.factor(KMEANS_res), lapso_engorda, fill=as.factor(KMEANS_res)))+
  geom_boxplot(outlier.colour = NA, alpha=.7) +
  geom_jitter(shape=21, size=3, alpha=.5, width = 0.2) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  scale_y_continuous(limits = c(0,20))+
  facet_grid(~ano_cierre_final, scales = "free",space = "free_x") +
  labs(title="Lapso engorda",
       x="\n Clusters",
       y="Meses (n°)\n") +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(color="white", size=11),
        axis.ticks.x = element_line(color = "grey30"))
)

ggsave("lapso_año.png", lapso_año, units="cm", dpi=300, width=25, height=12)


#lapso engorda x año de cierre
(mort_año <- bd_cluster_final %>% 
  ggplot(aes(as.factor(KMEANS_res), mort_acum, fill=as.factor(KMEANS_res)))+
  geom_boxplot(outlier.colour = NA, alpha=.7) +
  geom_jitter(shape=21, size=3, alpha=.5, width = 0.2) +
  scale_fill_manual(values = c("gold3","steelblue")) +
  scale_y_continuous(limits = c(0,1), labels = percent)+
  facet_grid(~ano_cierre_final, scales = "free",space = "free_x") +
  labs(title="Mortalidad Acumulada",
       x="\n Clusters",
       y="Mortalidad (%)\n") +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(color="white", size=11),
        axis.ticks.x = element_line(color = "grey30"))
)

ggsave("mort_año.png", mort_año, units="cm", dpi=300, width=25, height=12)






#########################################################+
###
###       ETAPA 2: ANÁLISIS DE RIESGO     ------------------------------------------------------------------------------------------------------------------------------------
###
########################################################+


#
##
#### CLUSTER 1 =================================================================
##
#


# Librerias especificas 
library(rriskDistributions)

# Base de datos
bd_cluster_final

names(bd_cluster_final)


#Histograma datos originales
(bd_cluster1 <- bd_cluster_final %>% 
  filter(KMEANS_res == 1)
)

dim(bd_cluster1)

###
### ...1] Mortalidad Acumulada ------------------------------------------------------------
###

### Histograma descriptivo de la variable

#hist(bd_cluster_final[bd_cluster_final$KMEANS_res == 1,]$mort_acum*100)
hist(bd_cluster1$mort_acum * 100)



### Ajustar a dist. de probabilidad ----

res1.mort<-fit.cont(bd_cluster1$mort_acum) 
res1.mort

res1.mort$fittedParams[["shape"]]
res1.mort$fittedParams[["rate"]]



### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos
simular<- rgamma(5000, shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]]) 

hist(simular, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Gamma; Repet=5000",
     breaks = 25)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dgamma(x, shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]]),col="blue",lwd=2,add=TRUE)


#Percentil 5
(valor_5pct<- qgamma(0.05,shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]], lower.tail=TRUE))
#Percentil 50
(valor_50pct<- qgamma(0.50,shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]], lower.tail=TRUE))
#Percentil 95
(valor_95pct<- qgamma(0.95,shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]], lower.tail=TRUE))


quantile(simular, c(0.05, 0.50, 0.95))


(valor_inf<- qgamma(0.0001,shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico
(valor_sup<- qgamma(0.9999,shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico


min(simular)
max(simular)
mean(simular)
median(simular)


# Grafico resultados simulados
(clu1.sim.mort.salar <- ggplot(as.data.frame(simular))+
   geom_histogram(aes(simular),fill="gold3", color="grey95", binwidth = 0.025) +
   scale_y_continuous(expand = c(0,0)) +
   scale_x_continuous(breaks = seq(0,1,0.05), labels = percent, expand = c(0,0)) +
   labs(title = "Análisis de riesgo: Mortalidad Acumulada (%)",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nMortalidad Acum. (%)",
        y="Frecuencia\n",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = valor_5pct, color="red", lty=2, lwd=.7) +
   geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=12, color="#333333")) +
   #comentario percentil 5
   annotate("rect", xmin = 0, xmax = valor_5pct, ymin = 675, ymax = 725, alpha = .25) +
   annotate("text", x = valor_5pct, y = 700, label="Percentil 5", size=3, hjust=1.25) +
   #comentario percentil 95
   annotate("rect", xmin = valor_95pct, xmax = 1, ymin = 675, ymax = 725, alpha = .25) +
   annotate("text", x = valor_95pct, y = 700, label="Percentil 95", size=3, hjust=-1)
)

ggsave("cluster_1_sim_hist_Mort_salar.png", clu1.sim.mort.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 



### Curvas de probabilidad acumulada ----

# F(x) de densidad
dgamma(0.098, shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]], 
       log=FALSE) #probabilidad de tener un resultado = 12p mort acum

# F(x) de probabilidad (area bajo la curva)
pgamma(0.098, shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]], 
       lower.tail=TRUE, log.p = FALSE) #Prob resultado <9,8p

(valor_ref_mort <- pgamma(0.098, shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]], 
       lower.tail=FALSE, log.p = FALSE) #Prob resultado >9,8p
)

mean(simular>= 0.098) #valor objetivo CERMAQ
mean(simular>= 0.12)
mean(simular>= 0.15)
mean(simular>= 0.20)
mean(simular>= 0.25)
mean(simular>= 0.30)



#Probabilidad resultado entre 2 valores
vProbX=pgamma(c(0.10,0.15), shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


curve(pgamma(x, shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]]),col="blue",lwd=2, add=TRUE)
curve(1-pgamma(x, shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]]),col="blue",lwd=2, add=TRUE)




#datos probailidad acum.
prob.mort <- curve(1-pgamma(x, shape=res1.mort$fittedParams[["shape"]], rate = res1.mort$fittedParams[["rate"]]))
prob.mort <- curve(1-pgamma(simular))

class(prob.mort)

bd.prob.mort <- data.frame(prob.mort$x, prob.mort$y)
head(bd.prob.mort)


# Grafico Prob. Acum. Mortalidad
(clu1.prob.mort.salar <- ggplot(bd.prob.mort)+
   geom_line(aes(prob.mort.x, prob.mort.y), lwd=1.5, color="gold3") +
   geom_area(aes(prob.mort.x, prob.mort.y), fill="gold3", alpha=.15) +
   scale_y_continuous(limits = c(0,1.05),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   labs(title = "Análisis de riesgo: Probabilidad de mortalidad > 9,8%",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nMortalidad Acum. (%)",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 0.098, color="red", lty=2, lwd=.7) +
   #geom_vline(xintercept = c(0.145, 0.05), color="grey60", lty=2, lwd=.5) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor de referencia
   geom_label(aes(x = 0.20 , y = valor_ref_mort, label = str_glue("Probabilidad:\n {round(valor_ref_mort*100, 1)}%")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 0.20, y = valor_ref_mort, xend = 0.11, yend = valor_ref_mort), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc"))) +
   annotate("segment", x = 0.124, xend = 0.124, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 0.05, xend = 0.05, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_1.prob.mort.salar.png", clu1.prob.mort.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 


#agregar puntos y lineas que muestran la mediana para cada producto

'med.surv <- data.frame(eje_x = c(.12), eje_y = c(valor_ref_mort,0))
med.surv.y <- data.frame(eje_x = c(valor_ref_mort), eje_y = c(.12,0))

clu2.prob.mort.salar + 
  geom_line(data = med.surv, aes(eje_x, eje_y, group = 1),col = "darkgreen", linetype = 3, size=.8) +
  #geom_line(data = med.surv.y, aes(eje_y, eje_x, group = 1),col = "darkgreen", linetype = 3, size=.8) +
  #geom_point(data = med.surv.y, aes(eje_y, eje_x, group =1), col = "darkgreen", alpha=.8) +
  geom_point(data = med.surv, aes(eje_x, eje_y, group =1), col = "darkgreen", alpha=.8)
'









###
###  ...2] Productividad  -----------------------------------------------------------------------------------------------------------------------------------------  
###

#Histograma datos originales
hist(bd_cluster1$kg_cosecha_smolt)


### Ajustar a dist. de probabilidad ---
res1.prod<-fit.cont(bd_cluster1$kg_cosecha_smolt) 
res1.prod

res1.prod$fittedParams[["location"]]
res1.prod$fittedParams[["scale"]]


### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos 
simular.prod <- rlogis(5000, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]]) 



hist(simular.prod, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
#agregar curva teorica
x<- seq(0,100, by=1)
curve(dlogis(x, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]]), col="blue",lwd=2,add=TRUE)


#Percentil 5
(valor_5pct<- qlogis(0.05, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]], lower.tail=TRUE) )
#Percentil 95
(valor_95pct<- qlogis(0.95, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]], lower.tail=TRUE))

quantile(simular.prod, c(0.05, 0.50, 0.95))


(valor_inf<- qlogis(0.0001, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico
(valor_sup<- qlogis(0.9999, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico


#estadisticos
max(simular.prod)
min(simular.prod)
mean(simular.prod)
median(simular.prod)


# Grafico resultados simulados
(clu1.sim.prod.salar <- ggplot(as.data.frame(simular.prod))+
   geom_histogram(aes(simular.prod),fill="gold3", color="grey95", binwidth = 0.2) +
   scale_y_continuous(expand = c(0,0)) +
   scale_x_continuous(expand = c(0,0), breaks = seq(1,7,0.5)) +
   labs(title = "Análisis de riesgo: Simulación de Productividad (Kg. Cosecha/smolt)",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nKg. Cosecha/smolt",
        y="Frecuencia\n",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = valor_5pct, color="red", lty=2, lwd=.7) +
   geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario percentil 5
   annotate("rect", xmin = min(simular.prod), xmax = valor_5pct, ymin = 975, ymax = 1025, alpha = .25) +
   annotate("text", x = valor_5pct, y = 1000, label="Percentil 5", size=3, hjust=2) +
   #comentario percentil 95
   annotate("rect", xmin = valor_95pct, xmax = max(simular.prod), ymin = 975, ymax = 1025, alpha = .25) +
   annotate("text", x = valor_95pct, y = 1000, label="Percentil 95", size=3, hjust=-1)
)

ggsave("cluster_1.sim.hist.prod.salar.png", clu1.sim.prod.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 




### Curvas de probabilidad acumulada ----

# F(x) de densidad
dlogis(4.57, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]], 
       log=FALSE) #probabilidad de tener un resultado = 4.2

# F(x) de probabilidad
plogis(4.57, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]], 
       lower.tail=FALSE, log.p = FALSE) #Prob resultado > 4.2


(ref_prod <- plogis(4.57, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]], 
       lower.tail=TRUE, log.p = FALSE) #Prob resultado < 4.2
)


#Probabilidad resultado entre 2 valores
vProbX=plogis(c(4, 4.6), location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


mean(simular.prod <= 4)
mean(simular.prod <= 4.2)
mean(simular.prod <= 4.4)
mean(simular.prod <= 4.57)
mean(simular.prod <= 5)



#datos probailidad acum.
prob.prod <- curve(plogis(x, location = res1.prod$fittedParams[["location"]], scale = res1.prod$fittedParams[["scale"]]), xlim=c(0,6))

bd.prob.prod <- data.frame(prob.prod$x, prob.prod$y)
head(bd.prob.prod)


# Grafico Prob. Acum. Mortalidad ----
(clu1.prob.prod.salar <- ggplot(bd.prob.prod)+
   geom_line(aes(prob.prod.x, prob.prod.y), lwd=1.5, color="gold3") +
   geom_area(aes(prob.prod.x, prob.prod.y), fill="gold3", alpha=.15) +
   scale_y_continuous(limits = c(0,1.05),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(1,6), breaks = seq(0,7,0.5), expand = c(0,0)) +
   labs(title = "Análisis de riesgo: Probabilidad de Productividad < 4.57 Kg.cosecha/smolt",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nKg.cosecha/smolt",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 4.57, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 3.2 , y = ref_prod, label = str_glue("Probabilidad:\n {round(ref_prod*100,2)}%")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 4, y = ref_prod, xend = 4.35, yend = ref_prod), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc"))) +
   annotate("segment", x = 4.55, xend = 4.55, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 5.46, xend = 5.46, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_1.prob.prod.salar.png", clu1.prob.prod.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 









###
###   ...3] Uso de antibióticos  ---------------------------------------------------------------------------------------------------------------------------  
### 

#Histograma datos originales
hist(bd_cluster1$gr_ab_ton_cosecha)


### Ajustar a dist. de probabilidad ----
res1.ab<-fit.cont(data2fit = bd_cluster1$gr_ab_ton_cosecha) 
res1.ab

res1.ab$fittedParams[["location"]]
res1.ab$fittedParams[["scale"]]



### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos 
simular.ab<- rlogis(5000, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]]) 
class(simular.ab)
if_else(simular.ab < 0, "NA", simular.ab)




hist(simular.ab, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
x<- seq(0,100, by=1)
curve(dlogis(x, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]]), col="blue",lwd=2,add=TRUE)


#Percentil 5
valor_5pct<- qlogis(0.05, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]], lower.tail=TRUE) 
#Percentil 95
valor_95pct<- qlogis(0.95, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]], lower.tail=TRUE)

valor_inf<- qlogis(0.0001, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]], lower.tail=TRUE) #solo para ubicar el texto en el gráfico
valor_sup<- qlogis(0.999, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]], lower.tail=TRUE) #solo para ubicar el texto en el gráfico


#transformar a data frame
max(simular.ab)
min(simular.ab)
mean(simular.ab)
median(simular.ab)



# Grafico resultados simulados ----
(clu1.sim.ab.salar <- ggplot(as.data.frame(simular.ab))+
   geom_histogram(aes(simular.ab),fill="gold3", color="grey95", binwidth = 100) +
   scale_y_continuous(expand = c(0,0)) +
   scale_x_continuous(expand = c(0,50), breaks = seq(0,3000,200), limits = c(-100,2900)) +
   labs(title = "Análisis de riesgo: Simulación uso de Antibióticos (Gr.Ab./ton. Cosechada)",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nGr.Ab./ton. Cosechada",
        y="Frecuencia\n",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = valor_5pct, color="red", lty=2, lwd=.7) +
   geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario percentil 5
   annotate("rect", xmin = valor_5pct, xmax = -100, ymin = 580, ymax = 620, alpha = .25) +
   annotate("text", x = (valor_5pct-50), y = 600, label="P5", size=3) +
   #comentario percentil 95
   annotate("rect", xmin = valor_95pct, xmax = max(simular.ab), ymin = 580, ymax = 620, alpha = .25) +
   annotate("text", x = (valor_95pct +200), y =600, label="Percentil 95", size=3)
)

ggsave("cluster_1.sim.hist.ab.salar.png", clu1.sim.ab.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 




### 2.2.- curvas de probabilidad acumulada ----

# F(x) de densidad
dlogis(250, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]], log=FALSE) #probabilidad de tener un resultado = 

# F(x) de probabilidad
plogis(250, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]], 
       lower.tail=TRUE, log.p = FALSE) #Prob resultado <

(ref_ab <- plogis(250, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]], 
       lower.tail=FALSE, log.p = FALSE) #Prob resultado > 250
)

#Probabilidad resultado entre 2 valores
vProbX=plogis(c(200,500), location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


mean(simular.ab >= 250)
mean(simular.ab >= 300)
mean(simular.ab >= 400)
mean(simular.ab >= 600)
mean(simular.ab >= 1000)


#datos probailidad acum.
prob.ab <- curve(1-plogis(x, location = res1.ab$fittedParams[["location"]], scale = res1.ab$fittedParams[["scale"]]), xlim=c(0,2800))

bd.prob.ab <- data.frame(prob.ab$x, prob.ab$y)
head(bd.prob.ab)


# Grafico Prob. Acum. Mortalidad ----
(clu1.prob.ab.salar <- ggplot(bd.prob.ab)+
   geom_line(aes(prob.ab.x, prob.ab.y), lwd=1.5, color="gold3") +
   geom_area(aes(prob.ab.x, prob.ab.y), fill="gold3", alpha=.15) +
   scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(-1,2600), breaks = seq(0,2600,200), expand = c(0.002,0.002)) +
   labs(title = "Análisis de riesgo: Probabilidad de uso de antibióticos > 250 Gr.Ab/ton.cosechada",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nGr.Ab/ton.cosechada",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 250, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 450 , y = ref_ab, label = str_glue("Probabilidad:\n  {round(ref_ab*100, 2)}%")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 445, y = ref_ab, xend = 308, yend = ref_ab), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc"))) +
   annotate("segment", x = 113, xend = 113, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 489, xend = 489, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_1.prob.ab.salar.png", clu1.prob.ab.salar, width = 22.8, height = 11.5, units = "cm", dpi=500)







### 
### ... 4] Baños Caligus  ------------------------------------------------------------------------------------------------------------------------------------  
### 


#Histograma datos originales
hist(bd_cluster1$banos_caligus)

#ajustar a dist. de probabilidad
res1.baño<-fit.cont(data2fit = bd_cluster1$banos_caligus) 
res1.baño

res1.baño$fittedParams[["mean"]]
res1.baño$fittedParams[["sd"]]

#---------------------------------------------------+
## 4.1- Simular 5000 resultado en base a Dist.Prob. ----
#---------------------------------------------------+
#Simular 5000 resultados posibles en base a parametros fijos 
simular.baño<- rnorm(5000, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]]) 

hist(simular.baño, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
x<- seq(0,100, by=1)
curve(dnorm(x, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]]), col="blue",lwd=2,add=TRUE)


#Percentil 5 y 95
valor_5pct<- qnorm(0.05, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]], lower.tail=TRUE) 
valor_95pct<- qnorm(0.95, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]], lower.tail=TRUE)

#Valor inferior y superior
valor_inf<- qnorm(0.0001, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]], lower.tail=TRUE) #solo para ubicar el texto en el gráfico
valor_sup<- qnorm(0.999, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]], lower.tail=TRUE) #solo para ubicar el texto en el gráfico


#transformar a data frame
max(simular.baño)
min(simular.baño)
mean(simular.baño)
median(simular.baño)


# Grafico resultados simulados ----
(clu1.sim.baño.salar <- ggplot(as.data.frame(simular.baño))+
   geom_histogram(aes(simular.baño),fill="gold3", color="grey95", binwidth = 2) +
   scale_y_continuous(expand = c(0,0)) +
   scale_x_continuous(breaks = seq(0,36,2), limits = c(-1,max(simular.baño))) +
   labs(title = "Análisis de riesgo: Simulación total baños antiparasitarios por ciclo",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nBaños (n°)",
        y="Frecuencia\n",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 0, color="red", lty=2, lwd=.7) +
   geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario percentil 5
   annotate("rect", xmin = -1, xmax = 0, ymin = 1230, ymax = 1280, alpha = .25) +
   annotate("text", x = -0.5, y = 1255, label="P5", size=3) +
   #comentario percentil 95
   annotate("rect", xmin = valor_95pct, xmax = max(simular.baño), ymin = 1230, ymax = 1280, alpha = .25) +
   annotate("text", x = (valor_95pct+2), y =1255, label="Percentil 95", size=3)
)

ggsave("cluster_1.sim.hist.baño.salar.png", clu1.sim.baño.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 




### Curvas de probabilidad acumulada ----

# F(x) de densidad
dnorm(10, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]], log=FALSE) #probabilidad de tener un resultado = 

# F(x) de probabilidad
pnorm(10, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]], 
       lower.tail=TRUE, log.p = FALSE) #Prob resultado <

(ref_baño <- pnorm(10, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]], 
       lower.tail=FALSE, log.p = FALSE) #Prob resultado > 10
)


#Probabilidad resultado entre 2 valores
vProbX <- pnorm(c(3,6), mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


mean(simular.baño >= 6)
mean(simular.baño >= 8)
mean(simular.baño >= 10)
mean(simular.baño >= 12)
mean(simular.baño >= 15)




#datos probailidad acum.
prob.baño <- curve(1-pnorm(x, mean = res1.baño$fittedParams[["mean"]], sd = res1.baño$fittedParams[["sd"]]), xlim=c(-1,26))

bd.prob.baño <- data.frame(prob.baño$x, prob.baño$y)
head(bd.prob.baño)


# Grafico Prob. Acum. Mortalidad ----
(clu1.prob.baño.salar <- ggplot(bd.prob.baño)+
   geom_line(aes(prob.baño.x, prob.baño.y), lwd=1.5, color="gold3") +
   geom_area(aes(prob.baño.x, prob.baño.y), fill="gold3", alpha=.15) +
   scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(-1,36), breaks = seq(0,36,2), expand = c(0,0)) +
   labs(title = "Análisis de riesgo: Probabilidad de baños antiparasitarios en el ciclo > 10",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nbaños (n°)",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 10, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 12.5 , y = ref_baño, label = str_glue("Probabilidad:\n {round(ref_baño*100,2)}% ")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 12.4, y = ref_baño, xend = 10.2, yend = ref_baño), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc"))) +
   annotate("segment", x = 0, xend = 0, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 9.1, xend = 9.1, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_1.prob.baño.salar.png", clu1.prob.baño.salar, width = 22.8, height = 11.5, units = "cm", dpi=500)






###
### ...5] GF3  --------------------------------------------------------------------------------------------------------------------------------------------------  
###

#Histograma datos originales
hist(bd_cluster1$gf3)

### Ajustar a dist. de probabilidad ----
res1.gf3<-fit.cont(data2fit = bd_cluster1$gf3) 
res1.gf3

res1.gf3$fittedParams[["mean"]]
res1.gf3$fittedParams[["sd"]]


### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos 
simular.gf3<- rnorm(5000, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]]) 

hist(simular.gf3, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
#agregar curva teorica
x<- seq(0,100, by=1)
curve(dnorm(x, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]]), col="blue",lwd=2,add=TRUE)

#Percentil 5 y 95
(valor_5pct<- qnorm(0.05, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]], lower.tail=TRUE) )
(valor_95pct<- qnorm(0.95, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]], lower.tail=TRUE) )

(valor_inf<- qnorm(0.0001, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico
(valor_sup<- qnorm(0.999, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico


#transformar a data frame
max(simular.gf3)
min(simular.gf3)
mean(simular.gf3)
median(simular.gf3)


# Grafico resultados simulados ----
(clu1.sim.gf3.salar <- ggplot(as.data.frame(simular.gf3))+
   geom_histogram(aes(simular.gf3),fill="gold3", color="grey95", binwidth = 0.1) +
   scale_y_continuous(expand = c(0,0), breaks = seq(0,1500,250)) +
   scale_x_continuous(expand = c(0,0), breaks = seq(0,2.8,0.2)) +
   labs(title = "Análisis de riesgo: Simulación de GF3",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nGF3",
        y="Frecuencia\n",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = valor_5pct, color="red", lty=2, lwd=.7) +
   geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario percentil 5
   annotate("rect", xmin = min(simular.gf3), xmax = valor_5pct, ymin = 1220, ymax = 1280, alpha = .25) +
   annotate("text", x = (valor_5pct - 0.1), y = 1250, label="Percentil 5", size=3) +
   #comentario percentil 95
   annotate("rect", xmin = valor_95pct, xmax = max(simular.gf3), ymin = 1220, ymax = 1280, alpha = .25) +
   annotate("text", x = (valor_95pct + 0.1), y = 1250, label="Percentil 95", size=3)
)

ggsave("cluster_1.sim.hist.gf3.salar.png", clu1.sim.gf3.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 




### Curvas de probabilidad acumulada ----

# F(x) de densidad
dnorm(2.1, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]], 
      log=FALSE) #probabilidad de tener un resultado = 

# F(x) de probabilidad
(ref_gf3 <- pnorm(2.1, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]], 
      lower.tail=TRUE, log.p = FALSE) #Prob resultado <
)

pnorm(2.1, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]], 
      lower.tail=FALSE, log.p = FALSE) #Prob resultado >


#Probabilidad resultado entre 2 valores
vProb <- pnorm(c(1.9,2.2), mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100

#datos probailidad acum.
prob.prod <- curve(pnorm(x, mean = res1.gf3$fittedParams[["mean"]], sd = res1.gf3$fittedParams[["sd"]]), xlim=c(1,3))

bd.prob.prod <- data.frame(prob.prod$x, prob.prod$y)
head(bd.prob.prod)


# Grafico Prob. Acum. Mortalidad ----
(clu1.prob.gf3.salar <- ggplot(bd.prob.prod)+
   geom_line(aes(prob.prod.x, prob.prod.y), lwd=1.5, color="gold3") +
   geom_area(aes(prob.prod.x, prob.prod.y), fill="gold3", alpha=.15) +
   scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(1.2,2.8), breaks = seq(1,3,0.2), expand = c(0,0)) +
   labs(title = "Análisis de riesgo: Probabilidad de GF3 < 2.1",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nGF3",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 2.1, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 1.76 , y = ref_gf3, label = str_glue("Probabilidad:\n {round(ref_gf3*100,2)}%")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 2, y = ref_gf3, xend = 2.08, yend = ref_gf3), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc"))) +
   annotate("segment", x = 2.14, xend = 2.14, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 2.42, xend = 2.42, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_1.prob.gf3.salar.png", clu1.prob.gf3.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 





###
### ...6] SGR  --------------------------------------------------------------------------------------------------------------------------------------------------  
###

#Histograma datos originales
hist(bd_cluster1$sgr)

### Ajustar a dist. de probabilidad ----
res1.sgr <- fit.cont(data2fit = bd_cluster1$sgr) 
res1.sgr

res1.sgr$fittedParams[["shape"]]
res1.sgr$fittedParams[["scale"]]


### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos 
simular.sgr<- rweibull(5000, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]]) 

hist(simular.sgr, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
#agregar curva teorica
x<- seq(0,100, by=1)
curve(dweibull(x, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]]), col="blue",lwd=2, add=TRUE)

#Percentil 5 y 95
(valor_5pct<- qweibull(0.05, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]], lower.tail=TRUE) )
(valor_95pct<- qweibull(0.95, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]], lower.tail=TRUE) )

(valor_inf<- qweibull(0.0001, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico
(valor_sup<- qweibull(0.999, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico


#transformar a data frame
max(simular.sgr)
min(simular.sgr)
mean(simular.sgr)
median(simular.sgr)


# Grafico resultados simulados ----
(clu1.sim.sgr.salar <- ggplot(as.data.frame(simular.sgr))+
   geom_histogram(aes(simular.gf3),fill="gold3", color="grey95", binwidth = 0.05) +
   scale_y_continuous(expand = c(0,0), breaks = seq(0,3000,250)) +
   scale_x_continuous(expand = c(0,0), breaks = seq(0, 2.8, 0.1)) +
   labs(title = "Análisis de riesgo: Simulación de SGR",
        subtitle = "CLUSTER 1 - SALAR",
        x="\n SGR",
        y="Frecuencia\n",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = valor_5pct, color="red", lty=2, lwd=.7) +
   geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario percentil 5
   annotate("rect", xmin = min(simular.sgr), xmax = valor_5pct, ymin = 1450, ymax = 1550, alpha = .25) +
   annotate("text", x = valor_5pct, y = 1500, label="Percentil 5", size=3, hjust=1.1) +
   #comentario percentil 95
   annotate("rect", xmin = valor_95pct, xmax = max(simular.sgr), ymin = 1450, ymax = 1550, alpha = .25) +
   annotate("text", x = valor_95pct, y = 1500, label="Percentil 95", size=3, hjust=-1.1)
)

ggsave("cluster_1.sim.hist.sgr.salar.png", clu1.sim.sgr.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 




### Curvas de probabilidad acumulada ----

# F(x) de densidad
dweibull(0.85, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]], log=FALSE) #probabilidad de tener un resultado = 

# F(x) de probabilidad
(ref_sgr <- pweibull(0.85, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]], 
                  lower.tail=TRUE, log.p = FALSE) #Prob resultado <
)

pweibull(0.85, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]], 
      lower.tail=FALSE, log.p = FALSE) #Prob resultado >


#Probabilidad resultado entre 2 valores
vProb <- pweibull(c(0.6, 0.8), shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100



#datos probailidad acum.
prob.sgr <- curve(pweibull(x, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]]), xlim=c(0,1.2))

bd.prob.sgr <- data.frame(prob.sgr$x, prob.sgr$y)
head(bd.prob.sgr)


# Grafico Prob. Acum. Mortalidad ----
(clu1.prob.sgr.salar <- ggplot(bd.prob.sgr)+
   geom_line(aes(prob.sgr.x, prob.sgr.y), lwd=1.5, color="gold3") +
   geom_area(aes(prob.sgr.x, prob.sgr.y), fill="gold3", alpha=.15) +
   scale_y_continuous(limits = c(0,1.05),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(0.2, 1.2), breaks = seq(0.2,3,0.2), expand = c(0.01,0.01)) +
   labs(title = "Análisis de riesgo: Probabilidad de SGR < 0.85",
        subtitle = "CLUSTER 1 - SALAR",
        x="\nSGR",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 0.85, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="gold3", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 0.58 , y = ref_sgr, label = str_glue("Probabilidad:\n {round(ref_sgr*100,2)}%")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 0.72, y = ref_sgr, xend = 0.84, yend = ref_sgr), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc")))+
   annotate("segment", x = 0.69, xend = 0.69, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 0.8, xend = 0.8, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_1.prob.sgr.salar.png", clu1.prob.sgr.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 









#
##
#### CLUSTER 2 ===================================================================================================================================================================
##
#

# Librerias especificas 
library(rriskDistributions)

# Base de datos
bd_cluster_final

names(bd_cluster_final)


#Histograma datos originales
(bd_cluster2 <- bd_cluster_final %>% 
    filter(KMEANS_res == 2)
)

dim(bd_cluster2)

###
### ...1] Mortalidad Acumulada ------------------------------------------------------------
###

### Histograma descriptivo de la variable

#hist(bd_cluster_final[bd_cluster_final$KMEANS_res == 1,]$mort_acum*100)
hist(bd_cluster2$mort_acum * 100)


### Ajustar a dist. de probabilidad ----

res2.mort<-fit.cont(bd_cluster2$mort_acum) 
res2.mort

res2.mort$fittedParams[["shape"]]
res2.mort$fittedParams[["rate"]]



### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos
simular<- rgamma(5000, shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]]) 

hist(simular, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Gamma; Repet=5000",
     breaks = 25)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dgamma(x, shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]]),col="blue",lwd=2,add=TRUE)


#Percentil 5, 50 y 95
(valor_5pct<- qgamma(0.05,shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], lower.tail=TRUE))
(valor_50pct<- qgamma(0.50,shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], lower.tail=TRUE))
(valor_95pct<- qgamma(0.95,shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], lower.tail=TRUE))


quantile(simular, c(0.05, 0.50, 0.95))


(valor_inf<- qgamma(0.0001,shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico
(valor_sup<- qgamma(0.9999,shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico


min(simular)
max(simular)
mean(simular)
median(simular)


# Grafico resultados simulados
(clu2.sim.mort.salar <- ggplot(as.data.frame(simular))+
    geom_histogram(aes(simular),fill="steelblue", color="grey95", binwidth = 0.025) +
    scale_y_continuous(expand = c(0,0), breaks = seq(0,900, 200)) +
    scale_x_continuous(breaks = seq(0,1,0.05), labels = percent, expand = c(0,0)) +
    labs(title = "Análisis de riesgo: Mortalidad Acumulada (%)",
         subtitle = "CLUSTER 2 - SALAR",
         x="\nMortalidad Acum. (%)",
         y="Frecuencia\n",
         caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
    geom_vline(xintercept = valor_5pct, color="red", lty=2, lwd=.7) +
    geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_line(color = "grey50"),
          axis.ticks.y=element_blank(),
          axis.line.x = element_line(colour="#333333"),
          plot.caption = element_text(color = "grey60", size=8),
          axis.title = element_text(color = "grey40", size=10),
          plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
          plot.title=element_text(size=12, color="#333333")) +
    #comentario percentil 5
    annotate("rect", xmin = 0, xmax = valor_5pct, ymin = 775, ymax = 825, alpha = .25) +
    annotate("text", x = valor_5pct, y = 800, label="Percent 5", size=3, hjust=1.25) +
    #comentario percentil 95
    annotate("rect", xmin = valor_95pct, xmax = 1, ymin = 775, ymax = 825, alpha = .25) +
    annotate("text", x = valor_95pct, y = 800, label="Percentil 95", size=3, hjust=-1)
)

ggsave("cluster_2_sim_hist_Mort_salar.png", clu2.sim.mort.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 



### Curvas de probabilidad acumulada ----

# F(x) de densidad
dgamma(0.098, shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], 
       log=FALSE) #probabilidad de tener un resultado = 12p mort acum

# F(x) de probabilidad (area bajo la curva)
pgamma(0.098, shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], 
       lower.tail=TRUE, log.p = FALSE) #Prob resultado <12p

(valor_ref_mort <- pgamma(0.098, shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], 
                          lower.tail=FALSE, log.p = FALSE) #Prob resultado >12p
)

pgamma(0.30, shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], 
       lower.tail=FALSE, log.p = FALSE) #Prob resultado >15p


mean(simular>= 0.098)
mean(simular>= 0.15)
mean(simular>= 0.20)
mean(simular>= 0.25)
mean(simular>= 0.30)





#Probabilidad resultado entre 2 valores
vProbX=pgamma(c(0.10,0.15), shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


curve(pgamma(x, shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]]),col="blue",lwd=2, add=TRUE)
curve(1-pgamma(x, shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]]),col="blue",lwd=2, add=TRUE)



#datos probailidad acum.
prob.mort <- curve(1-pgamma(x, shape=res2.mort$fittedParams[["shape"]], rate = res2.mort$fittedParams[["rate"]]))
prob.mort <- curve(1-pgamma(simular))

class(prob.mort)

bd.prob.mort <- data.frame(prob.mort$x, prob.mort$y)
head(bd.prob.mort)


# Grafico Prob. Acum. Mortalidad
(clu2.prob.mort.salar <- ggplot(bd.prob.mort)+
    geom_line(aes(prob.mort.x, prob.mort.y), lwd=1.5, color="steelblue") +
    geom_area(aes(prob.mort.x, prob.mort.y), fill="steelblue", alpha=.15) +
    scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
    scale_x_continuous(breaks = seq(0,1,0.1), labels = percent, expand = c(0,0.01)) +
    labs(title = "Análisis de riesgo: Probabilidad de mortalidad > 9,8%",
         subtitle = "CLUSTER 2 - SALAR",
         x="\nMortalidad Acum. (%)",
         y="Prob. Acum.",
         caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
    geom_vline(xintercept = 0.098, color="red", lty=2, lwd=.7) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_line(color = "grey50"),
          axis.ticks.y=element_blank(),
          axis.line.x = element_line(colour="#333333"),
          plot.caption = element_text(color = "grey60", size=8),
          axis.title = element_text(color = "grey40", size=10),
          plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
          plot.title=element_text(size=14, color="#333333")) +
    #comentario y flecha: valor de referencia
    geom_label(aes(x = 0.19 , y = valor_ref_mort, label = str_glue("Probabilidad:\n {round(valor_ref_mort*100, 1)}%")),  hjust = 0, vjust = 0.5, 
               lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
    geom_curve(aes(x = 0.18, y = valor_ref_mort, xend = 0.105, yend = valor_ref_mort), colour = "#555555", curvature = 0, size=0.5,
               arrow = arrow(length = unit(0.03, "npc")))+
    annotate("segment", x = 0.124, xend = 0.124, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
    annotate("segment", x = 0.05, xend = 0.05, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_2.prob.mort.salar.png", clu2.prob.mort.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 





###
###  ...2] Productividad  -----------------------------------------------------------------------------------------------------------------------------------------  
###

#Histograma datos originales
hist(bd_cluster2$kg_cosecha_smolt)


### Ajustar a dist. de probabilidad ---
res2.prod<-fit.cont(bd_cluster2$kg_cosecha_smolt) 
res2.prod

res2.prod$fittedParams[["location"]]
res2.prod$fittedParams[["scale"]]


### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos 
simular.prod<- rlogis(5000, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]]) 

hist(simular.prod, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
#agregar curva teorica
x<- seq(0,100, by=1)
curve(dlogis(x, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]]), col="blue",lwd=2,add=TRUE)


#Percentil 5 y 95
(valor_5pct<- qlogis(0.05, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]], lower.tail=TRUE) )
(valor_95pct<- qlogis(0.95, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]], lower.tail=TRUE))

quantile(simular.prod, c(0.05, 0.50, 0.95))


(valor_inf<- qlogis(0.0001, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico
(valor_sup<- qlogis(0.9999, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico


#estadisticos
max(simular.prod)
min(simular.prod)
mean(simular.prod)
median(simular.prod)


# Grafico resultados simulados
(clu2.sim.prod.salar <- ggplot(as.data.frame(simular.prod))+
    geom_histogram(aes(simular.prod),fill="steelblue", color="grey95", binwidth = 0.2) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0), breaks = seq(1,7,0.5)) +
    labs(title = "Análisis de riesgo: Simulación de Productividad (Kg. Cosecha/smolt)",
         subtitle = "CLUSTER 2 - SALAR",
         x="\nKg. Cosecha/smolt",
         y="Frecuencia\n",
         caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
    geom_vline(xintercept = valor_5pct, color="red", lty=2, lwd=.7) +
    geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.ticks.x = element_line(color = "grey50"),
          axis.ticks.y=element_blank(),
          axis.line.x = element_line(colour="#333333"),
          plot.caption = element_text(color = "grey60", size=8),
          axis.title = element_text(color = "grey40", size=10),
          plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
          plot.title=element_text(size=14, color="#333333")) +
    #comentario percentil 5
    annotate("rect", xmin = min(simular.prod), xmax = valor_5pct, ymin = 975, ymax = 1025, alpha = .25) +
    annotate("text", x = valor_5pct, y = 1000, label="Percentil 5", size=3, hjust=2) +
    #comentario percentil 95
    annotate("rect", xmin = valor_95pct, xmax = max(simular.prod), ymin = 975, ymax = 1025, alpha = .25) +
    annotate("text", x = valor_95pct, y = 1000, label="Percentil 95", size=3, hjust=-1)
)

ggsave("cluster_2.sim.hist.prod.salar.png", clu2.sim.prod.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 




### Curvas de probabilidad acumulada ----

# F(x) de densidad
dlogis(4.57, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]], 
       log=FALSE) #probabilidad de tener un resultado = 4.2

# F(x) de probabilidad
plogis(4.57, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]], 
       lower.tail=FALSE, log.p = FALSE) #Prob resultado > 4.2


(ref_prod <- plogis(4.57, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]], 
                    lower.tail=TRUE, log.p = FALSE) #Prob resultado < 4.2
)


#Probabilidad resultado entre 2 valores
vProbX=plogis(c(4, 4.6), location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


mean(simular.prod <= 4)
mean(simular.prod <= 4.2)
mean(simular.prod <= 4.4)
mean(simular.prod <= 4.57)
mean(simular.prod <= 5)



#datos probailidad acum.
prob.prod <- curve(plogis(x, location = res2.prod$fittedParams[["location"]], scale = res2.prod$fittedParams[["scale"]]), xlim=c(0,6))

bd.prob.prod <- data.frame(prob.prod$x, prob.prod$y)
head(bd.prob.prod)


# Grafico Prob. Acum. Mortalidad ----
(clu2.prob.prod.salar <- ggplot(bd.prob.prod)+
   geom_line(aes(prob.prod.x, prob.prod.y), lwd=1.5, color="steelblue") +
   geom_area(aes(prob.prod.x, prob.prod.y), fill="steelblue", alpha=.15) +
   scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(1,6), breaks = seq(0,6,0.5), expand = c(0,0)) +
   labs(title = "Análisis de riesgo: Probabilidad de Productividad < 4.57 Kg.cosecha/smolt",
        subtitle = "CLUSTER 2 - SALAR",
        x="\nKg.cosecha/smolt",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 4.57, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 3.2 , y = ref_prod, label = str_glue("Probabilidad:\n {round(ref_prod*100,2)}%")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 4, y = ref_prod, xend = 4.35, yend = ref_prod), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc")))+
   annotate("segment", x = 4.55, xend = 4.55, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 5.46, xend = 5.46, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_2.prob.prod.salar.png", clu2.prob.prod.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 









###
###   ...3] Uso de antibióticos  ---------------------------------------------------------------------------------------------------------------------------  
### 

#Histograma datos originales
hist(bd_cluster2$gr_ab_ton_cosecha)


### Ajustar a dist. de probabilidad ----
res2.ab<-fit.cont(data2fit = bd_cluster2$gr_ab_ton_cosecha) 
res2.ab

res2.ab$fittedParams[["location"]]
res2.ab$fittedParams[["scale"]]



### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos 
simular.ab<- rlogis(5000, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]]) 

hist(simular.ab, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
x<- seq(0,100, by=1)
curve(dlogis(x, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]]), col="blue",lwd=2,add=TRUE)


#Percentil 5 y 95
(valor_5pct<- qlogis(0.05, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]], lower.tail=TRUE)) 
(valor_95pct<- qlogis(0.95, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]], lower.tail=TRUE))

#valor inferior y superior
(valor_inf<- qlogis(0.0001, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico
(valor_sup<- qlogis(0.999, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico


#transformar a data frame
max(simular.ab)
min(simular.ab)
mean(simular.ab)
median(simular.ab)



# Grafico resultados simulados ----
(clu2.sim.ab.salar <- ggplot(as.data.frame(simular.ab))+
   geom_histogram(aes(simular.ab),fill="steelblue", color="grey95", binwidth = 100) +
   scale_y_continuous(expand = c(0,0)) +
   scale_x_continuous(expand = c(0,50), breaks = seq(0,3000,200), limits = c(-100,max(simular.ab))) +
   labs(title = "Análisis de riesgo: Simulación uso de Antibióticos (Gr.Ab./ton. Cosechada)",
        subtitle = "CLUSTER 2 - SALAR",
        x="\nGr.Ab./ton. Cosechada",
        y="Frecuencia\n",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = valor_5pct, color="red", lty=2, lwd=.7) +
   geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario percentil 5
   annotate("rect", xmin = valor_5pct, xmax = -100, ymin = 980, ymax = 1020, alpha = .25) +
   annotate("text", x = (valor_5pct-50), y = 1000, label="P5", size=3) +
   #comentario percentil 95
   annotate("rect", xmin = valor_95pct, xmax = max(simular.ab), ymin = 980, ymax = 1020, alpha = .25) +
   annotate("text", x = (valor_95pct +200), y =1000, label="Percentil 95", size=3)
)

ggsave("cluster_2.sim.hist.ab.salar.png", clu2.sim.ab.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 




### 2.2.- curvas de probabilidad acumulada ----

# F(x) de densidad
dlogis(250, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]], log=FALSE) #probabilidad de tener un resultado = 

# F(x) de probabilidad
plogis(250, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]], 
       lower.tail=TRUE, log.p = FALSE) #Prob resultado <

(ref_ab <- plogis(250, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]], 
                  lower.tail=FALSE, log.p = FALSE) #Prob resultado > 250
)

#Probabilidad resultado entre 2 valores
vProbX=plogis(c(200,500), location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


mean(simular.ab >= 250)
mean(simular.ab >= 300)
mean(simular.ab >= 400)
mean(simular.ab >= 600)
mean(simular.ab >= 1000)


#datos probailidad acum.
prob.ab <- curve(1-plogis(x, location = res2.ab$fittedParams[["location"]], scale = res2.ab$fittedParams[["scale"]]), xlim=c(-100,2800))

bd.prob.ab <- data.frame(prob.ab$x, prob.ab$y)
head(bd.prob.ab)


# Grafico Prob. Acum. Mortalidad ----
(clu2.prob.ab.salar <- ggplot(bd.prob.ab)+
   geom_line(aes(prob.ab.x, prob.ab.y), lwd=1.5, color="steelblue") +
   geom_area(aes(prob.ab.x, prob.ab.y), fill="steelblue", alpha=.15) +
   scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(-100,2600), breaks = seq(0,2600,200), expand = c(0.02,0.02)) +
   labs(title = "Análisis de riesgo: Probabilidad de uso de antibióticos > 250 Gr.Ab/ton.cosechada",
        subtitle = "CLUSTER 2 - SALAR",
        x="\nGr.Ab/ton.cosechada",
        y="Prob.Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 250, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 450 , y = ref_ab, label = str_glue("Probabilidad:\n  {round(ref_ab*100, 2)}%")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 445, y = ref_ab, xend = 309, yend = ref_ab), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc")))+
   annotate("segment", x = 113, xend = 113, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 489, xend = 489, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_2.prob.ab.salar.png", clu2.prob.ab.salar, width = 22.8, height = 11.5, units = "cm", dpi=500)







### 
### ... 4] Baños Caligus  ------------------------------------------------------------------------------------------------------------------------------------  
### 


#Histograma datos originales
hist(bd_cluster2$banos_caligus)

#ajustar a dist. de probabilidad
res2.baño<-fit.cont(data2fit = bd_cluster2$banos_caligus) 
res2.baño

res2.baño$fittedParams[["mean"]]
res2.baño$fittedParams[["sd"]]

#---------------------------------------------------+
## 4.1- Simular 5000 resultado en base a Dist.Prob. ----
#---------------------------------------------------+
#Simular 5000 resultados posibles en base a parametros fijos 
simular.baño<- rnorm(5000, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]]) 

hist(simular.baño, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
x<- seq(0,100, by=1)
curve(dnorm(x, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]]), col="blue",lwd=2,add=TRUE)


#Percentil 5 y 95
valor_5pct<- qnorm(0.05, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]], lower.tail=TRUE) 
valor_95pct<- qnorm(0.95, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]], lower.tail=TRUE)

#Valor inferior y superior
valor_inf<- qnorm(0.0001, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]], lower.tail=TRUE) #solo para ubicar el texto en el gráfico
valor_sup<- qnorm(0.999, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]], lower.tail=TRUE) #solo para ubicar el texto en el gráfico


#transformar a data frame
max(simular.baño)
min(simular.baño)
mean(simular.baño)
median(simular.baño)


# Grafico resultados simulados ----
(clu2.sim.baño.salar <- ggplot(as.data.frame(simular.baño))+
   geom_histogram(aes(simular.baño),fill="steelblue", color="grey95", binwidth = 2) +
   scale_y_continuous(expand = c(0,0)) +
   scale_x_continuous(breaks = seq(0,36,2), limits = c(-1,max(simular.baño))) +
   labs(title = "Análisis de riesgo: Simulación total baños antiparasitarios por ciclo",
        subtitle = "CLUSTER 2 - SALAR",
        x="\nBaños (n°)",
        y="Frecuencia\n",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 0, color="red", lty=2, lwd=.7) +
   geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario percentil 5
   annotate("rect", xmin = -1, xmax = 0, ymin = 1230, ymax = 1280, alpha = .25) +
   annotate("text", x = -0.5, y = 1255, label="P5", size=3) +
   #comentario percentil 95
   annotate("rect", xmin = valor_95pct, xmax = max(simular.baño), ymin = 1230, ymax = 1280, alpha = .25) +
   annotate("text", x = (valor_95pct+2), y =1255, label="Percentil 95", size=3)
)

ggsave("cluster_2.sim.hist.baño.salar.png", clu2.sim.baño.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 




### Curvas de probabilidad acumulada ----

# F(x) de densidad
dnorm(10, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]], log=FALSE) #probabilidad de tener un resultado = 

# F(x) de probabilidad
pnorm(10, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]], 
      lower.tail=TRUE, log.p = FALSE) #Prob resultado <

(ref_baño <- pnorm(10, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]], 
                   lower.tail=FALSE, log.p = FALSE) #Prob resultado > 10
)


#Probabilidad resultado entre 2 valores
vProbX <- pnorm(c(3,6), mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


mean(simular.baño >= 6)
mean(simular.baño >= 8)
mean(simular.baño >= 10)
mean(simular.baño >= 12)
mean(simular.baño >= 15)




#datos probailidad acum.
prob.baño <- curve(1-pnorm(x, mean = res2.baño$fittedParams[["mean"]], sd = res2.baño$fittedParams[["sd"]]), xlim=c(-10,26))

bd.prob.baño <- data.frame(prob.baño$x, prob.baño$y)
head(bd.prob.baño)


# Grafico Prob. Acum. Mortalidad ----
(clu2.prob.baño.salar <- ggplot(bd.prob.baño)+
   geom_line(aes(prob.baño.x, prob.baño.y), lwd=1.5, color="steelblue") +
   geom_area(aes(prob.baño.x, prob.baño.y), fill="steelblue", alpha=.15) +
   scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(-2,36), breaks = seq(0,36,2), expand = c(0,0)) +
   labs(title = "Análisis de riesgo: Probabilidad de baños antiparasitarios en el ciclo > 10",
        subtitle = "CLUSTER 2 - SALAR",
        x="\nbaños (n°)",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 10, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 12.5 , y = ref_baño, label = str_glue("Probabilidad:\n {round(ref_baño*100,2)}% ")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 12.4, y = ref_baño, xend = 10.2, yend = ref_baño), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc"))) +
   annotate("segment", x = 0, xend = 0, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 9.1, xend = 9.1, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_2.prob.baño.salar.png", clu2.prob.baño.salar, width = 22.8, height = 11.5, units = "cm", dpi=500)






###
### ...5] GF3  --------------------------------------------------------------------------------------------------------------------------------------------------  
###

#Histograma datos originales
hist(bd_cluster2$gf3)

### Ajustar a dist. de probabilidad ----
res2.gf3<-fit.cont(data2fit = bd_cluster2$gf3) 
res2.gf3

res2.gf3$fittedParams[["mean"]]
res2.gf3$fittedParams[["sd"]]


### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos 
simular.gf3<- rnorm(5000, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]]) 

hist(simular.gf3, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
#agregar curva teorica
x<- seq(0,100, by=1)
curve(dnorm(x, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]]), col="blue",lwd=2,add=TRUE)

#Percentil 5 y 95
(valor_5pct<- qnorm(0.05, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]], lower.tail=TRUE) )
(valor_95pct<- qnorm(0.95, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]], lower.tail=TRUE) )

(valor_inf<- qnorm(0.0001, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico
(valor_sup<- qnorm(0.999, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico


#transformar a data frame
max(simular.gf3)
min(simular.gf3)
mean(simular.gf3)
median(simular.gf3)


# Grafico resultados simulados ----
(clu2.sim.gf3.salar <- ggplot(as.data.frame(simular.gf3))+
   geom_histogram(aes(simular.gf3),fill="steelblue", color="grey95", binwidth = 0.1) +
   scale_y_continuous(expand = c(0,0), breaks = seq(0,1500,250)) +
   scale_x_continuous(expand = c(0,0), breaks = seq(0,2.8,0.2)) +
   labs(title = "Análisis de riesgo: Simulación de GF3",
        subtitle = "CLUSTER 2 - SALAR",
        x="\nGF3",
        y="Frecuencia\n",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = valor_5pct, color="red", lty=2, lwd=.7) +
   geom_vline(xintercept = valor_95pct, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario percentil 5
   annotate("rect", xmin = min(simular.gf3), xmax = valor_5pct, ymin = 1220, ymax = 1280, alpha = .25) +
   annotate("text", x = (valor_5pct - 0.1), y = 1250, label="Percentil 5", size=3) +
   #comentario percentil 95
   annotate("rect", xmin = valor_95pct, xmax = max(simular.gf3), ymin = 1220, ymax = 1280, alpha = .25) +
   annotate("text", x = (valor_95pct + 0.1), y = 1250, label="Percentil 95", size=3)
)

ggsave("cluster_2.sim.hist.gf3.salar.png", clu2.sim.gf3.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 




### Curvas de probabilidad acumulada ----

# F(x) de densidad
dnorm(2.1, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]], 
      log=FALSE) #probabilidad de tener un resultado = 

# F(x) de probabilidad
(ref_gf3 <- pnorm(2.1, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]], 
                  lower.tail=TRUE, log.p = FALSE) #Prob resultado <
)

pnorm(2.1, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]], 
      lower.tail=FALSE, log.p = FALSE) #Prob resultado >


#Probabilidad resultado entre 2 valores
vProb <- pnorm(c(1.9,2.2), mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100

#datos probailidad acum.
prob.prod <- curve(pnorm(x, mean = res2.gf3$fittedParams[["mean"]], sd = res2.gf3$fittedParams[["sd"]]), xlim=c(1,3))

bd.prob.prod <- data.frame(prob.prod$x, prob.prod$y)
head(bd.prob.prod)


# Grafico Prob. Acum. Mortalidad ----
(clu2.prob.gf3.salar <- ggplot(bd.prob.prod)+
   geom_line(aes(prob.prod.x, prob.prod.y), lwd=1.5, color="steelblue") +
   geom_area(aes(prob.prod.x, prob.prod.y), fill="steelblue", alpha=.15) +
   scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(1.2,3), breaks = seq(1,2.8,0.2), expand = c(0,0)) +
   labs(title = "Análisis de riesgo: Probabilidad de GF3 < 2.1",
        subtitle = "CLUSTER 2 - SALAR",
        x="\nGF3",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 2.1, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 1.76 , y = ref_gf3, label = str_glue("Probabilidad:\n {round(ref_gf3*100,2)}%")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 2, y = ref_gf3, xend = 2.08, yend = ref_gf3), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc"))) +
   annotate("segment", x = 2.14, xend = 2.14, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 2.42, xend = 2.42, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)

ggsave("cluster_2.prob.gf3.salar.png", clu2.prob.gf3.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 







###
### ...6] SGR  --------------------------------------------------------------------------------------------------------------------------------------------------  
###

#Histograma datos originales
hist(bd_cluster2$sgr)

### Ajustar a dist. de probabilidad ----
res2.sgr <- fit.cont(data2fit = bd_cluster2$sgr) 
res2.sgr

res2.sgr$fittedParams[["shape"]]
res2.sgr$fittedParams[["scale"]]


### Simular 5000 resultado en base a Dist.Prob. ----

#Simular 5000 resultados posibles en base a parametros fijos 
simular.sgr<- rweibull(5000, shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]]) 

hist(simular.sgr, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados; Repet=5000", breaks = 10)
#agregar curva teorica
x<- seq(0,100, by=1)
curve(dweibull(x, shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]]), col="blue",lwd=2, add=TRUE)

#Percentil 5 y 95
(valor_5pct<- qweibull(0.05, shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]], lower.tail=TRUE) )
(valor_95pct<- qweibull(0.95, shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]], lower.tail=TRUE) )

(valor_inf<- qweibull(0.0001, shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico
(valor_sup<- qweibull(0.999, shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]], lower.tail=TRUE)) #solo para ubicar el texto en el gráfico


#transformar a data frame
max(simular.sgr)
min(simular.sgr)
mean(simular.sgr)
median(simular.sgr)





### Curvas de probabilidad acumulada ----

# F(x) de densidad
dweibull(0.85, shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]], log=FALSE) #probabilidad de tener un resultado = 

# F(x) de probabilidad
(ref_sgr <- pweibull(0.85, shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]], 
                     lower.tail=TRUE, log.p = FALSE) #Prob resultado <
)

pweibull(0.85, shape = res1.sgr$fittedParams[["shape"]], scale = res1.sgr$fittedParams[["scale"]], 
         lower.tail=FALSE, log.p = FALSE) #Prob resultado >


#Probabilidad resultado entre 2 valores
vProb <- pweibull(c(0.6, 0.8), shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]], lower.tail=TRUE) 
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100



#datos probailidad acum.
prob.sgr <- curve(pweibull(x, shape = res2.sgr$fittedParams[["shape"]], scale = res2.sgr$fittedParams[["scale"]]), xlim=c(0.2,1.3))

bd.prob.sgr <- data.frame(prob.sgr$x, prob.sgr$y)
head(bd.prob.sgr)


# Grafico Prob. Acum. Mortalidad ----
(clu2.prob.sgr.salar <- ggplot(bd.prob.sgr)+
   geom_line(aes(prob.sgr.x, prob.sgr.y), lwd=1.5, color="steelblue") +
   geom_area(aes(prob.sgr.x, prob.sgr.y), fill="steelblue", alpha=.15) +
   scale_y_continuous(limits = c(0,1.05),breaks = seq(0,1,0.1), labels = percent, expand = c(0.01,0.01)) +
   scale_x_continuous(limits = c(0.2, 1.2), breaks = seq(0.2,3,0.2), expand = c(0.01,0.01)) +
   labs(title = "Análisis de riesgo: Probabilidad de SGR < 0.85",
        subtitle = "CLUSTER 2 - SALAR",
        x="\nSGR",
        y="Prob. Acum.",
        caption = "Simulación de Montecarlo en base a 5.000 repeticiones \nFuente: Aquabench") +
   geom_vline(xintercept = 0.85, color="red", lty=2, lwd=.7) +
   theme_minimal() +
   theme(panel.grid.minor = element_blank(),
         panel.grid.major.x = element_blank(),
         axis.ticks.x = element_line(color = "grey50"),
         axis.ticks.y=element_blank(),
         axis.line.x = element_line(colour="#333333"),
         plot.caption = element_text(color = "grey60", size=8),
         axis.title = element_text(color = "grey40", size=10),
         plot.subtitle=element_text(size=10, color="steelblue", face="bold"),
         plot.title=element_text(size=14, color="#333333")) +
   #comentario y flecha: valor mínimo
   geom_label(aes(x = 0.58 , y = ref_sgr, label = str_glue("Probabilidad:\n {round(ref_sgr*100,2)}%")),  hjust = 0, vjust = 0.5, 
              lineheight = 0.8, colour = "#555555", fill = "grey90", label.size = NA, size = 4) +
   geom_curve(aes(x = 0.72, y = ref_sgr, xend = 0.84, yend = ref_sgr), colour = "#555555", curvature = 0, size=0.5,
              arrow = arrow(length = unit(0.03, "npc")))+
   annotate("segment", x = 0.69, xend = 0.69, y = 0, yend = 0.1, colour = "grey60", lwd=.8) +
   annotate("segment", x = 0.8, xend = 0.8, y = 0, yend = 0.1, colour = "grey60", lwd=.8)
)


ggsave("cluster_2.prob.sgr.salar.png", clu2.prob.sgr.salar, width = 22.8, height = 11.5, units = "cm", dpi=500) 

