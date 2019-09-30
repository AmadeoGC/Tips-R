###############################################################
###     Generar gráfico con su tabla de datos asociada      ###
###            ggplot +  ggpubr::ggarrange()                ###
###               <Amadeo Guzmán C.>                        ###
###############################################################

#Ejmplo obtenido de:
#Falta link -> Buscar

#Data frame
df <- structure(list(City = structure(c(2L, 3L, 1L), .Label = c("Minneapolis", "Phoenix","Raleigh"), class = "factor"), January = c(52.1, 40.5, 12.2), 
                     February = c(55.1, 42.2, 16.5), March = c(59.7, 49.2, 28.3), April = c(67.7, 59.5, 45.1), May = c(76.3, 67.4, 57.1),
                     June = c(84.6, 74.4, 66.9), July = c(91.2, 77.5, 71.9), August = c(89.1, 76.5, 70.2), September = c(83.8, 70.6, 60),
                     October = c(72.2, 60.2, 50), November = c(59.8, 50, 32.4), December = c(52.5, 41.2, 18.6)), .Names = c("City", "January",
                                                                                                            "February", "March", "April", "May", "June",
                                                                                                            "July", "August", "September", "October",
                                                                                                            "November", "December"), class = "data.frame",
                  row.names = c(NA, -3L))
df

#Pasar a formato largo con melt()
#install.packages("reshape", dependencies=TRUE)
library(reshape)
dfm <- melt(df, variable_name = "month")
dfm
levels(dfm$month) <- month.abb



#-------------------------------
## Generar gráfico principal
#-------------------------------
library(ggplot2)

#Gráfico ->  gráfico de lineas básico
(p1 <- ggplot(dfm, aes(month, value, group = City, colour = City)) +
               geom_line(size = 1)
  )

#se agregan mejoras de formato, lineas horizontales, verticales y texto
(p2 <- p1 + 
    geom_vline(xintercept = c(2.9, 5.9, 8.9, 11.9), colour = "grey85", alpha = 0.5) +
    geom_hline(yintercept = 32, colour = "grey80", alpha = 0.5) + 
    theme_classic() +
    annotate("text", x = 1.2, y = 35, label = "Freezing", colour = "grey80", size = 4) + 
    annotate("text", x = c(1.5, 4.5, 7.5, 10.5), y = 97, label = c("Winter","Spring", "Summer", "Autumn"), colour = "grey70",size = 4)
  )

#formato final con texto como leyenda
(p3 <- p2 + 
    geom_text(data = dfm[dfm$month == "Dec", ], aes(label = City), hjust = 0.7, vjust = 1) +
    theme(legend.position = "none"))



#------------------------------
## Generar tabla de datos
#------------------------------
#Para crear la tabla realmente se hace un gráfico con geom_text()
(data_table <- ggplot(dfm, aes(x = month, y = factor(City), label = format(value, nsmall = 1), colour = City)) +
                geom_text(size = 3.5) +
                theme_bw() +
                #theme_void() +
                scale_y_discrete(labels = abbreviate, limits = c("Minneapolis", "Raleigh", "Phoenix")) +
                #theme(panel.grid.major = "none", legend.position = "none", panel.border = "none", axis.text.x = "none", axis.ticks = "none") +
                #theme(plot.margin = unit(c(-0.5,1, 0, 0.5), "lines")) + 
                theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
                theme(axis.text.x = element_blank()) +
                theme(axis.ticks.x = element_blank()) +
                theme(legend.position = "none")+
                xlab(NULL) + 
                ylab(NULL)
)


#-------------------------------------
### juntar los dos objetos en 1 hoja
#-------------------------------------

#install.packages("ggpubr")
library(ggpubr)

ggarrange(p3, data_table, ncol = 1, nrow = 2,
          heights = c(1.2, 0.3))


