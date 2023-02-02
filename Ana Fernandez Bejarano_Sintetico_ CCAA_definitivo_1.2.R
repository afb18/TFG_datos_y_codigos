setwd("/Users/Ana/Desktop/TFG datos")
library(tidysynth)
library(dplyr)
library(ggplot2)
library(ggpubr)

rm(list=ls())
data <- read.csv("Ana_Fernández_Bejarano_datos_CCAA.csv")
data <- data[,2:19] #quitar los números que salen en la columna para cada observación

#PIB real per capita
data[,3] <- data[,3]/data[,4] 
## Se elimina este paso por concordancia con provincias
#tasa de empleo para cada variable: numero de ocupados/población mayor de 16 años
#data[,5:13] <- data[,5:13]/data[,14] 
#ordenar los datos
data <- arrange(data,region,year)

#comando para que las operaciones se hagan por grupos (AND,..)
data2 <- group_by(data, region)
#Crecimientos de las variables: (ocup_t-ocup_t-1)/ocup_t-1
data2 <- summarize(data2,occupationrate = 100*(Ocup_agreg_FEDEA-lag(Ocup_agreg_FEDEA))/lag(Ocup_agreg_FEDEA),
                   occupationrate2 = 100*(Ocup_agreg_FEDEA-lag(Ocup_agreg_FEDEA,2))/lag(Ocup_agreg_FEDEA,2),
                   populrate = 100*(Popul_FEDEA-lag(Popul_FEDEA))/lag(Popul_FEDEA),
                   pibrate=100*(PIB_real-lag(PIB_real))/lag(PIB_real),
                   constrate = 100*(Ocup_c_FEDEA-lag(Ocup_c_FEDEA))/lag(Ocup_c_FEDEA),
                   toursrate = 100*(Ocup_CyHyTyC_FEDEA-lag(Ocup_CyHyTyC_FEDEA))/lag(Ocup_CyHyTyC_FEDEA),
                   agrigrate = 100*(Ocup_AyP_FEDEA-lag(Ocup_AyP_FEDEA))/lag(Ocup_AyP_FEDEA),
                   constrate2 = 100*(Ocup_c_FEDEA-lag(Ocup_c_FEDEA,2))/lag(Ocup_c_FEDEA,2),
                   toursrate2 = 100*(Ocup_CyHyTyC_FEDEA-lag(Ocup_CyHyTyC_FEDEA,2))/lag(Ocup_CyHyTyC_FEDEA,2),
                   agrigrate2 = 100*(Ocup_AyP_FEDEA-lag(Ocup_AyP_FEDEA,2))/lag(Ocup_AyP_FEDEA,2),
                   Creaciongr = (Creacion - lag(Creacion)),
                   Creaciongr2 = (Creacion - lag(Creacion,2)))
#data2[,7:9] <- data2[,2:4]
data$Creacion <- data2$Creaciongr
data$Creacion2 <- data2$Creaciongr2
#Ponerlo en la base inicial data
data$Ocup_agreg_FEDEA <- data2$occupationrate
data$Ocup_agreg_FEDEA2 <- data2$occupationrate2
data$Popul_FEDEA <- data2$populrate
data$PIB_real <- data2$pibrate
data$Ocup_c_FEDEA <- data2$constrate
data$Ocup_c_FEDEA2 <- data2$constrate2
data$Ocup_CyHyTyC_FEDEA <- data2$toursrate
data$Ocup_CyHyTyC_FEDEA2 <- data2$toursrate2
data$Ocup_AyP_FEDEA <- data2$agrigrate
data$Ocup_AyP_FEDEA2 <- data2$agrigrate2
#Quitar la base de datos que me permitio agrupar para hacer los crecimientos
rm(data2)

#Cortar base a las 10 primeros años
data <- data[data$year <= 2012,]

################################################## PLAN DE RUTA ####################################################################################################################################################################

########################Sintético del crecimiento de la población agregada
#Proclamación pública de ciudad candidata 1981 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$PIB_real),1:5] %>% #Quito nivel nacional y Ceuta y Melilla y restrinjo a los años que tengo datos
  synthetic_control(outcome = Ocup_agreg_FEDEA,
                    unit = region,
                    time = year,
                    i_unit = "CAT",#region de interés
                    i_time = 1981,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Ocup_agreg_FEDEA_77 = Ocup_agreg_FEDEA) %>%
  generate_predictor(time_window = 1979,
                     Ocup_agreg_FEDEA_79 = Ocup_agreg_FEDEA) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
difagreg81 <- syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE) +
  theme_classic() + xlab("") + ylab("") + theme(legend.position = "none") + ggtitle("")

#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico datos con el sintético
trendagreg81 <- syntdat %>% plot_trends(time_window = 1960:2015) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#RMSPE
prederragreg81 <- syntdat %>% plot_mspe_ratio(time_window = 1960:2015)+
  theme_classic() + xlab("") +
  theme(legend.position = "none") + ggtitle("")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$PIB_real),1:5] %>% #Quito nivel nacional y Ceuta y Melilla y restrinjo a los años que tengo datos
  synthetic_control(outcome = Ocup_agreg_FEDEA,
                    unit = region,
                    time = year,
                    i_unit = "CAT",#region de interés
                    i_time = 1986,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Ocup_agreg_FEDEA_81 = Ocup_agreg_FEDEA) %>%
  generate_predictor(time_window = 1985,
                     Ocup_agreg_FEDEA_85 = Ocup_agreg_FEDEA) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
difagreg86 <- syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico datos con el sintético
trendagreg86 <- syntdat %>% plot_trends(time_window = 1960:2015)  +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#RMSPE
prederragreg86 <- syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  theme(legend.position = "none") + ggtitle("")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

################# Sintético de la variación de la creación de empresas (1961 a 2001)
#Proclamación pública de ciudad candidata 1981
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Creacion) & data$year < 2002,c(1:4,18)] %>%
  synthetic_control(outcome = Creacion,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1981,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Creacion_77 = Creacion) %>%
  generate_predictor(time_window = 1979,
                     Creacion_79 = Creacion) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
difemp81 <- syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)  +
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación de la creación de empresas")
#Gráfico datos con el sintético
trendemp81 <- syntdat %>% plot_trends(time_window = 1960:2015)+
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#RMSPE
prederremp81 <- syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  theme(legend.position = "none") + ggtitle("")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y creación de empresas (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Creacion)  & data$year < 2002,c(1:4,18)] %>%
  synthetic_control(outcome = Creacion,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1986,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Creacion_81 = Creacion) %>%
  generate_predictor(time_window = 1985,
                     Creacion_85 = Creacion) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
difemp86 <- syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)+
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico datos con el sintético
trendemp86 <- syntdat %>% plot_trends(time_window = 1960:2015)+
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#RMSPE
prederremp86 <- syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  theme(legend.position = "none") + ggtitle("")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y creación de empresa (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

################# Sintético del crecimiento de la población ocupada en la construcción
#Proclamación pública de ciudad candidata 1981 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$PIB_real),c(1:4,10)] %>%
  synthetic_control(outcome = Ocup_c_FEDEA,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1981,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Ocup_c_FEDEA_77 = Ocup_c_FEDEA) %>%
  generate_predictor(time_window = 1979,
                     Ocup_c_FEDEA_79 = Ocup_c_FEDEA) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
difconst81 <- syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)  +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("") 
#Gráfico datos con el sintético
trendconst81 <- syntdat %>% plot_trends(time_window = 1960:2015) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#RMSPE
prederrconst81 <- syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  theme(legend.position = "none") + ggtitle("")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$PIB_real),c(1:4,10)] %>%
  synthetic_control(outcome = Ocup_c_FEDEA,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1986,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1979,
                     Ocup_c_FEDEA_79 = Ocup_c_FEDEA) %>%
  generate_predictor(time_window = 1983,
                     Ocup_c_FEDEA_83 = Ocup_c_FEDEA) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
difconst86 <- syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)+
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico datos con el sintético
trendconst86 <- syntdat %>% plot_trends(time_window = 1960:2015)  +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#RMSPE
prederrconst86 <- syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  theme(legend.position = "none") + ggtitle("")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


################# Sintético del crecimiento de la población ocupada en turismo
#Proclamación pública de ciudad candidata 1981 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$PIB_real),c(1:4,13)] %>%
  synthetic_control(outcome = Ocup_CyHyTyC_FEDEA,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1981,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Ocup_CyHyTyC_FEDEA_77 = Ocup_CyHyTyC_FEDEA) %>%
  generate_predictor(time_window = 1979,
                     Ocup_CyHyTyC_FEDEA_79 = Ocup_CyHyTyC_FEDEA) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
diftur81 <- syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico datos con el sintético
trendtur81 <- syntdat %>% plot_trends(time_window = 1960:2015) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#RMSPE
prederrtur81 <- syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  theme(legend.position = "none") + ggtitle("")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


#Concesión como ciudad olímpica 1986 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$PIB_real),c(1:4,13)] %>%
  synthetic_control(outcome = Ocup_CyHyTyC_FEDEA,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1986,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Ocup_CyHyTyC_FEDEA_81 = Ocup_CyHyTyC_FEDEA) %>%
  generate_predictor(time_window = 1985,
                     Ocup_CyHyTyC_FEDEA_85 = Ocup_CyHyTyC_FEDEA) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
diftur86 <- syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico datos con el sintético
trendtur86 <- syntdat %>% plot_trends(time_window = 1960:2015) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#RMSPE
prederrtur86 <- syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  theme(legend.position = "none") + ggtitle("")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

# Celebración de los Juegos Olímpicos 1992 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$PIB_real),c(1:4,13)] %>%
  synthetic_control(outcome = Ocup_CyHyTyC_FEDEA,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1992,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:2012,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1988,
                     Ocup_CyHyTyC_FEDEA_88 = Ocup_CyHyTyC_FEDEA) %>%
  generate_predictor(time_window = 1991,
                     Ocup_CyHyTyC_FEDEA_85 = Ocup_CyHyTyC_FEDEA) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
diftur92 <- syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#Gráfico datos con el sintético
trendtur92 <- syntdat %>% plot_trends(time_window = 1960:2015) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none") + ggtitle("")
#RMSPE
prederrtur92 <- syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  theme(legend.position = "none") + ggtitle("")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y



################# Sintético del crecimiento de la tasa de empleo en agricultura y pesca (faltan provinciales debido a un error del escaneado)
#Proclamación pública de ciudad candidata 1981 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$PIB_real),c(1:4,6)] %>%
  synthetic_control(outcome = Ocup_AyP_FEDEA,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1981,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Ocup_AyP_FEDEA_77 = Ocup_AyP_FEDEA) %>%
  generate_predictor(time_window = 1979,
                     Ocup_AyP_FEDEA_79 = Ocup_AyP_FEDEA) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


#Concesión como ciudad olímpica 1986
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$PIB_real),c(1:4,6)] %>%
  synthetic_control(outcome = Ocup_AyP_FEDEA,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1986,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Ocup_AyP_FEDEA_81 = Ocup_AyP_FEDEA) %>%
  generate_predictor(time_window = 1985,
                     Ocup_AyP_FEDEA_85 = Ocup_AyP_FEDEA) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

###################### Síntetico del crecimiento de la población ocupada agregada cada DOS AÑOS
#Proclamación pública de ciudad candidata 1981 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Ocup_agreg_FEDEA2),c(1:4,20)] %>% #Quito nivel nacional y Ceuta y Melilla y restrinjo a los años que tengo datos
  synthetic_control(outcome = Ocup_agreg_FEDEA2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",#region de interés
                    i_time = 1981,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Ocup_agreg_FEDEA2_77 = Ocup_agreg_FEDEA2) %>%
  generate_predictor(time_window = 1979,
                     Ocup_agreg_FEDEA2_79 = Ocup_agreg_FEDEA2) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE) +
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento bianual de la ocupación agregada")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento bianual de la ocupación agregada")
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015) +
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético del crecimiento bianual de la población ocupada agregada")
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)+
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Ocup_agreg_FEDEA2),c(1:4,20)] %>% #Quito nivel nacional y Ceuta y Melilla y restrinjo a los años que tengo datos
  synthetic_control(outcome = Ocup_agreg_FEDEA2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",#region de interés
                    i_time = 1986,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Ocup_agreg_FEDEA2_81 = Ocup_agreg_FEDEA2) %>%
  generate_predictor(time_window = 1985,
                     Ocup_agreg_FEDEA2_85 = Ocup_agreg_FEDEA2) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE) +
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento bianual de la ocupación agregada")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE) +
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento bianual de la ocupación agregada")
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015) +
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético del crecimiento bianual de la población ocupada agregada")
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

###################### Síntetico del crecimiento de la población ocupada en construcción cada DOS AÑOS
#Proclamación pública de ciudad candidata 1981 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Ocup_c_FEDEA2),c(1:4,21)] %>%
  synthetic_control(outcome = Ocup_c_FEDEA2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1981,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Ocup_c_FEDEA2_77 = Ocup_c_FEDEA2) %>%
  generate_predictor(time_window = 1979,
                     Ocup_c_FEDEA2_79 = Ocup_c_FEDEA2) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la población ocupada en construcción")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la población ocupada en construcción")
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético de la variación bianual de la población ocupada en construcción")
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)+
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Ocup_c_FEDEA2),c(1:4,21)] %>%
  synthetic_control(outcome = Ocup_c_FEDEA2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1986,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1979,
                     Ocup_c_FEDEA2_79 = Ocup_c_FEDEA2) %>%
  generate_predictor(time_window = 1983,
                     Ocup_c_FEDEA2_83 = Ocup_c_FEDEA2) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la población ocupada en construcción")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la población ocupada en construcción")
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético de la variación bianual de la población ocupada en construcción")
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)+
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


################# Sintético del crecimiento de la población ocupada en servicios cada DOS AÑOS
#Proclamación pública de ciudad candidata 1981 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Ocup_CyHyTyC_FEDEA2),c(1:4,22)] %>%
  synthetic_control(outcome = Ocup_CyHyTyC_FEDEA2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1981,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1973,
                     Ocup_CyHyTyC_FEDEA2_73 = Ocup_CyHyTyC_FEDEA2) %>%
  generate_predictor(time_window = 1975,
                     Ocup_CyHyTyC_FEDEA2_75 = Ocup_CyHyTyC_FEDEA2) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la población ocupada en servicios")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la población ocupada en servicios")
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético de la variación bianual de la población ocupada en servicios")
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)+
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


#Concesión como ciudad olímpica 1986 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Ocup_CyHyTyC_FEDEA2),c(1:4,22)] %>%
  synthetic_control(outcome = Ocup_CyHyTyC_FEDEA2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1986,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Ocup_CyHyTyC_FEDEA2_81 = Ocup_CyHyTyC_FEDEA2) %>%
  generate_predictor(time_window = 1985,
                     Ocup_CyHyTyC_FEDEA2_85 = Ocup_CyHyTyC_FEDEA2) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la población ocupada en servicios")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la población ocupada en servicios")
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético de la variación bianual de la población ocupada en servicios")
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)+
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

################# Sintético del crecimiento de la tasa de empleo en agricultura y pesca cada DOS AÑOS (faltan provinciales debido a un error del escaneado)
#Proclamación pública de ciudad candidata 1981 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Ocup_AyP_FEDEA2),c(1:4,23)] %>%
  synthetic_control(outcome = Ocup_AyP_FEDEA2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1981,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1974,
                     Ocup_AyP_FEDEA2_74 = Ocup_AyP_FEDEA2) %>%
  generate_predictor(time_window = 1977,
                     Ocup_AyP_FEDEA2_80 = Ocup_AyP_FEDEA2) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


#Concesión como ciudad olímpica 1986
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Ocup_AyP_FEDEA2),c(1:4,23)] %>%
  synthetic_control(outcome = Ocup_AyP_FEDEA2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1986,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Ocup_AyP_FEDEA2_77 = Ocup_AyP_FEDEA2) %>%
  generate_predictor(time_window = 1981,
                     Ocup_AyP_FEDEA2_81 = Ocup_AyP_FEDEA2) %>%
  generate_predictor(time_window = 1985,
                     Ocup_AyP_FEDEA2_85 = Ocup_AyP_FEDEA2) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

###################### Síntetico de la variación de la creación de empresas cada DOS AÑOS (1961 a 2001)
#Proclamación pública de ciudad candidata 1981
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Creacion2) & data$year < 2002,c(1:4,19)] %>%
  synthetic_control(outcome = Creacion2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1981,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1980,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Creacion2_77 = Creacion2) %>%
  generate_predictor(time_window = 1979,
                     Creacion2_79 = Creacion2) %>%
  generate_weights(time_window = 1956:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la creación de empresas")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la creación de empresas")
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético de la variación bianual de la creación de empresas")
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015) +
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y creación de empresas (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 
syntdat <- data[data$region != "total" & data$region != "CyMel" &
                  !is.na(data$Creacion2) & data$year < 2002,c(1:4,19)] %>%
  synthetic_control(outcome = Creacion2,
                    unit = region,
                    time = year,
                    i_unit = "CAT",
                    i_time = 1988,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1956:1985,
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Popul_FEDEA = mean(Popul_FEDEA,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Creacion2_81 = Creacion2) %>%
  generate_predictor(time_window = 1985,
                     Creacion2_85 = Creacion2) %>%
  generate_weights(time_window = 1956:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1960:2015, prune= TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la creación de empresas")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1960:2015, prune= FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación bianual de la creación de empresas")
#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1960:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético de la variación bianual de la creación de empresas")
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1960:2015)+
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
syntdat %>% grab_signficance(time_window = 1960:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y creación de empresa (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


###########Quitar tendencias iniciales
data2 <- summarize(group_by(data,region),
                       dtempleoagreg = detrend(Ocup_agreg_FEDEA,'linear'),
                       dtempleoagreg2 = detrend(Ocup_agreg_FEDEA2,'linear'),
                       dtempleoconst = detrend(Ocup_c_FEDEA,'linear'),
                       dtempleoconst2 = detrend(Ocup_c_FEDEA2,'linear'),
                       dtempleoser = detrend(Ocup_CyHyTyC_FEDEA,'linear'),
                       dtempleoser2 = detrend(Ocup_CyHyTyC_FEDEA2,'linear'),
                       dtempleoayp = detrend(Ocup_AyP_FEDEA,'linear'),
                       dtempleoayp2 = detrend(Ocup_AyP_FEDEA2,'linear'),
                       dtcreacion = detrend(Creacion,'linear'),
                       dtcreacion2 = detrend(Creacion2,'linear'))
data$Ocup_agreg_FEDEA <- data2$dtempleoagreg
data$Ocup_agreg_FEDEA2 <- data2$dtempleoagreg2
data$Ocup_c_FEDEA <- data2$dtempleoconst
data$Ocup_c_FEDEA2 <- data2$dtempleoconst2
data$Ocup_CyHyTyC_FEDEA <- data2$dtempleoser
data$Ocup_CyHyTyC_FEDEA2 <- data2$dtempleoser2
data$Ocup_AyP_FEDEA <- data2$dtempleoayp
data$Ocup_AyP_FEDEA2 <- data2$dtempleoayp2
data$Creacion <- data2$dtcreacion
data$Creacion2 <- data2$dtcreacion2
rm(data2)
#Mismo código que sintético anterior porque se han pegado los datos


#### FIGURES ####
ggarrange(trendagreg81,trendagreg86, trendtur81, trendtur86,ncol = 2, nrow = 2)

ggarrange(trendconst81,trendconst86,
          trendemp81, trendemp86, ncol = 2, nrow = 2)

ggarrange(difagreg81, difagreg86,diftur81, diftur86, difconst81, difconst86,
          difemp81, difemp86, ncol = 2, nrow = 4)

ggarrange(prederragreg81, prederragreg86,prederrtur81, prederrtur86, nrow = 2, ncol = 2)

ggarrange( prederrconst81,
           prederrconst86, 
           prederremp81, prederremp86, ncol = 2, nrow = 2)

ggarrange(diftur92,prederrtur92, nrow = 1, ncol = 2)






