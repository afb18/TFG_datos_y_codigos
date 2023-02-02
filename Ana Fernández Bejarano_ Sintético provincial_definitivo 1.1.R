setwd("/Users/Ana/Desktop/TFG datos/provincias")
library(tidysynth)
library(tidyr)
library(dplyr)
library(pracma)
# rm(list=ls())
dataprov <- read.csv("Ana_Fernández_Bejarano_datos_provincial.csv")
dataprov <- dataprov[dataprov$region != "Ceuta" & dataprov$region != "Ceuta y Melilla" &
                       dataprov$region != "Melilla"
                     & dataprov$region != "Total",2:16] 

#Tasa de empleo
dataprov[,11:15] <- dataprov[,11:15]/dataprov$Pobl_16

#PIB per capita
dataprov[,5:6] <- dataprov[,5:6]/dataprov$Pobl_total
dataprov <- arrange(dataprov, region, year)

#Crecimiento de la población total
dataprov2 <- summarize(group_by(dataprov,region),
                       populrate = 100*(Pobl_total-lag(Pobl_total))/lag(Pobl_total))
dataprov$Pobl_total <- dataprov2$populrate
rm(dataprov2)

#Crecimiento de PIB per capita
dataprov2 <- summarize(group_by(dataprov,region),
                       pibrealrate = 100*(PIB_real-lag(PIB_real,2))/lag(PIB_real,2))

dataprov$PIB_real <- dataprov2$pibrealrate
rm(dataprov2)

#Crecimiento de la población ocupada (agregada y sectores) y de la creación de empresas
dataprov2 <- summarize(group_by(dataprov,region), 
                         empleo_agregadogr = 100*(Empleo_agreg - lag(Empleo_agreg,2))/lag(Empleo_agreg,2),
                         empleo_constgr = 100*(Empleo_constr - lag(Empleo_constr,2))/lag(Empleo_constr,2),
                         empleo_sergr = 100*(Empleo_ser - lag(Empleo_ser,2))/lag(Empleo_ser,2),
                         Creaciongr = (Creacion - lag(Creacion)),
                         Creaciongr2 = (Creacion - lag(Creacion,2)))
dataprov[,7:9] <- dataprov2[,2:4]
dataprov$Creacion <- dataprov2$Creaciongr
dataprov$Creacion2 <- dataprov2$Creaciongr2
rm(dataprov2)

dataprov$year <- as.numeric(dataprov$year)
subdata <- dataprov[!is.na(dataprov$Empleo_agreg),]

################################################################ PLAN DE RUTA ####################################################################################################################################################################

########################Sintético del crecimiento de la población ocupada agregada

#Proclamación pública de ciudad candidata 1981 (datos de 1957 a 1993 bianuales)
syntdat <- subdata[,c(1:3,6,7)] %>% 
  synthetic_control(outcome = Empleo_agreg,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1981,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1980,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Empleo_agreg_77 = Empleo_agreg) %>%
  generate_predictor(time_window = 1979,
                     Empleo_agreg_79 = Empleo_agreg) %>%
  generate_weights(optimization_window = 1957:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdifagreg81 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#Gráfico datos con el sintético
ptrendagreg81 <- syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#RMSPE
pprederragreg81 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("") + theme(legend.position = "none")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 (datos de 1957 a 1993 bianuales)
syntdat <- subdata[,c(1:3,6,7)] %>% 
  synthetic_control(outcome = Empleo_agreg,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1986,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1985,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Empleo_agreg_81 = Empleo_agreg) %>%
  generate_predictor(time_window = 1985,
                     Empleo_agreg_85 = Empleo_agreg) %>%
  generate_weights(optimization_window = 1957:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdifagreg86 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#Gráfico datos con el sintético
ptrendagreg86 <- syntdat %>% plot_trends(time_window = 1957:2015) + theme_classic() +
  ylab("") + ggtitle("") +
  theme(legend.position = "none") + xlab("")
#RMSPE
pprederragreg86 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("") + theme(legend.position = "none")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


#################################### Síntetico de la variación de la creación de empresas (1961 a 2001)
#Proclamación pública de ciudad candidata 1981 
subdata <- dataprov[!is.na(dataprov$Creacion) & dataprov$year %in% seq(1961,2001),]
syntdat <- subdata %>% 
  synthetic_control(outcome = Creacion,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1981,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1980,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Creacion_77 = Creacion) %>%
  generate_predictor(time_window = 1979,
                     Creacion_79 = Creacion) %>%
  generate_weights(optimization_window = 1957:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdifemp81 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético de la variación de la creación de empresas")

#Gráfico datos con el sintético
ptrendemp81 <- syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#RMSPE
pprederremp81 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("") + theme(legend.position = "none")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y creación (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 
subdata <- dataprov[!is.na(dataprov$Creacion) & dataprov$year %in% seq(1961,2001),]
syntdat <- subdata %>% 
  synthetic_control(outcome = Creacion,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1986,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1985,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Creacion_81 = Creacion) %>%
  generate_predictor(time_window = 1985,
                     Creacion_85 = Creacion) %>%
  generate_weights(optimization_window = 1957:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdifemp86 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#Gráfico datos con el sintético
ptrendemp86 <- syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#RMSPE
pprederremp86 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("") + theme(legend.position = "none")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y creación (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


########################## Síntético del crecimiento de la población ocupada en la contrucción
#Proclamación pública de ciudad candidata 1981 (datos de 1957 a 1993 bianuales)
subdata <- dataprov[!is.na(dataprov$Empleo_constr),]
syntdat <- subdata %>% 
  synthetic_control(outcome = Empleo_constr,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1981,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1980,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Empleo_constr_77 = Empleo_constr) %>%
  generate_predictor(time_window = 1979,
                     Empleo_constr_79 = Empleo_constr) %>%
  generate_weights(optimization_window = 1957:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdifconst81 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#Gráfico datos con el sintético
ptrendconst81 <- syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#RMSPE
pprederrconst81 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("") + theme(legend.position = "none")

#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 (datos de 1957 a 1993 bianuales)
subdata <- dataprov[!is.na(dataprov$Empleo_constr),]
syntdat <- subdata %>% 
  synthetic_control(outcome = Empleo_constr,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1986,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1985,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1979,
                     Empleo_constr_79 = Empleo_constr) %>%
  generate_predictor(time_window = 1983,
                     Empleo_constr_83 = Empleo_constr) %>%
  generate_weights(optimization_window = 1957:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdifconst86 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#Gráfico datos con el sintético
ptrendconst86 <- syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#RMSPE
pprederrconst86 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("") + theme(legend.position = "none")

#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

########################## Síntético del crecimiento de la población ocupada en el sector servicios
#Proclamación pública de ciudad candidata 1981 (datos de 1957 a 1993 bianuales)
subdata <- dataprov[!is.na(dataprov$Empleo_ser),]
syntdat <- subdata %>% 
  synthetic_control(outcome = Empleo_ser,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1981,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1980,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1973,
                     Empleo_ser_73 = Empleo_ser) %>%
  generate_predictor(time_window = 1975,
                     Empleo_ser_75 = Empleo_ser) %>%
  generate_weights(optimization_window = 1957:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdifserv81 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#Gráfico datos con el sintético
ptrendserv81 <- syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#RMSPE
pprederrserv81 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("") + theme(legend.position = "none")

#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 (datos de 1957 a 1993 bianuales)
subdata <- dataprov[!is.na(dataprov$Empleo_ser),]
syntdat <- subdata %>% 
  synthetic_control(outcome = Empleo_ser,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1986,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1985,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Empleo_constr_81 = Empleo_ser) %>%
  generate_predictor(time_window = 1985,
                     Empleo_constr_85 = Empleo_ser) %>%
  generate_weights(optimization_window = 1957:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdifserv86 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento de la ocupación servicios")

#Gráfico datos con el sintético
ptrendserv86 <- syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#RMSPE
pprederrserv86 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("") + theme(legend.position = "none")

#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

###################### Síntetico de la variación de la creación de empresas cada DOS AÑOS (1961 a 2001)
#Proclamación pública de ciudad candidata 1981 (datos de 1957 a 1993 bianuales)
subdata <- dataprov[!is.na(dataprov$Creacion2) & dataprov$year %in% seq(1961,2001),]
syntdat <- subdata %>% 
  synthetic_control(outcome = Creacion2,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1981,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1980,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     Creacion2_77 = Creacion2) %>%
  generate_predictor(time_window = 1979,
                     Creacion2_79 = Creacion2) %>%
  generate_weights(optimization_window = 1957:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)

#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1957:2015)
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1957:2015)
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#Concesión como ciudad olímpica 1986 (datos de 1957 a 1993 bianuales)
subdata <- dataprov[!is.na(dataprov$Creacion2) & dataprov$year %in% seq(1961,2001),]
syntdat <- subdata %>% 
  synthetic_control(outcome = Creacion2,
                    unit = region,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1986,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1957:1985,#la media de los predictores entre los años anteriores
                     PIB_real = mean(PIB_real,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1981,
                     Creacion2_81 = Creacion2) %>%
  generate_predictor(time_window = 1985,
                     Creacion2_85 = Creacion2) %>%
  generate_weights(optimization_window = 1957:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos$weight <- round(pesos$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)

#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1957:2015)
#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1957:2015)
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

###########Quitar tendencias iniciales
dataprov2 <- summarize(group_by(dataprov,region),
                       dtempleoagreg = detrend(Empleo_agreg,'linear'),
                       dtempleoconst = detrend(Empleo_constr,'linear'),
                       dtempleoser = detrend(Empleo_ser,'linear'),
                       dtcreacion = detrend(Creacion,'linear'),
                       dtcreacion2 = detrend(Creacion2,'linear'))
dataprov[,7:9] <- dataprov2[,2:4]
dataprov$Creacion <- dataprov2$dtcreacion
dataprov$Creacion2 <- dataprov2$dtcreacion2
rm(dataprov2)
#Mismo código que sintético anterior




