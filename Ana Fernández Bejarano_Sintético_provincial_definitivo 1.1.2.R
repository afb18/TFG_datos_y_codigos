setwd("/Users/Ana/Desktop/TFG datos/provincias")
library(readxl)
library(readxl)
library(Synth)
library(dplyr)
library(tidyr)
library(tidysynth)
library(pracma)
library(ggpubr)

dataprov <- read.csv("Ana Fernández Bejarano_2base_provincial.csv") %>%
  select(-X)

# Pib per cápita
dataprov$realpibturismo <- dataprov$realpibturismo/dataprov$Pobl_total
dataprov$realpibservicios <- dataprov$realpibservicios/dataprov$Pobl_total

dataprov2 <- summarise(group_by(dataprov,regions),
                       empleoservsgr = 100*(empleoservs - lag(empleoservs))/lag(empleoservs),
                       empleotursgr = 100*(empleoturs - lag(empleoturs))/lag(empleoturs),
                       realpibturismogr = 100*(realpibturismo - lag(realpibturismo))/lag(realpibturismo),
                       realpibserviciosgr = 100*(realpibservicios - lag(realpibservicios))/lag(realpibservicios),
                       poblgr = 100*(Pobl_total - lag(Pobl_total))/lag(Pobl_total),
                       poblalqgr = 100*(poblalq - lag(poblalq))/lag(poblalq))
dataprov$empleoservs <- dataprov2$empleoservsgr
dataprov$empleoturs <- dataprov2$empleotursgr
dataprov$realpibturismo <- dataprov2$realpibturismogr
dataprov$realpibservicios <- dataprov2$realpibserviciosgr
dataprov$Pobl_total <- dataprov2$poblgr
dataprov$poblalq <- dataprov2$poblalqgr
rm(dataprov2)
dataprov$year <- as.numeric(dataprov$year)
##### Crecimiento de la población ocupada turismo
#Proclamación pública de ciudad candidata 1981 (datos de 1957 a 1993 bianuales)
syntdat <- dataprov[!is.na(dataprov$empleoturs) & !is.na(dataprov$regions) &
                    dataprov$regions != "Ceuta" & dataprov$regions != "Melilla",] %>% 
  synthetic_control(outcome = empleoturs,
                    unit = regions,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1981,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1971:1977,#la media de los predictores entre los años anteriores
                     realpibturismo = mean(realpibturismo,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1977,
                     empleoturs_77 = mean(empleoturs)) %>%
  generate_predictor(time_window = 1979,
                     empleoturs_79 = mean(empleoturs)) %>%
  generate_weights(optimization_window = 1957:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos1 <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos1$weight <- round(pesos1$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdiftur81 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento de la ocupación turismo")

#Gráfico datos con el sintético
ptrendtur81 <- syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#RMSPE
pprederrtur81 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
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


#Proclamación pública de ciudad candidata 1986 (datos de 1957 a 1993 bianuales)
syntdat <- dataprov[!is.na(dataprov$empleoturs) & !is.na(dataprov$regions) &
                      dataprov$regions != "Ceuta" & dataprov$regions != "Melilla",] %>% 
  synthetic_control(outcome = empleoturs,
                    unit = regions,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1986,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1971:1985,#la media de los predictores entre los años anteriores
                     realpibturismo = mean(realpibturismo,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1979,
                     empleoturs_81 = mean(empleoturs)) %>%
  generate_predictor(time_window = 1983,
                     empleoturs_85 = mean(empleoturs)) %>%
  generate_weights(optimization_window = 1957:1985) %>%
  generate_control()

#Tabla: pesos de cada región
pesos1 <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos1$weight <- round(pesos1$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
pdiftur86 <- syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")

#Gráfico datos con el sintético
ptrendtur86 <- syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("") + theme(legend.position = "none")
#RMSPE
pprederrtur86 <- syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
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

#### Población ocupada Servicios ####
#Celebración de JO 1992 (datos de 1957 a 1993 bianuales)
syntdat <- dataprov[!is.na(dataprov$empleoservs) & !is.na(dataprov$regions) &
                      dataprov$regions != "Ceuta" & dataprov$regions != "Melilla",] %>% 
  synthetic_control(outcome = empleoservs,
                    unit = regions,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1992,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1971:1989,#la media de los predictores entre los años anteriores
                     realpibturismo = mean(realpibturismo,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  # generate_predictor(time_window = 1987,
  #                    empleoservs_77 = mean(empleoservs)) %>%
  # generate_predictor(time_window = 1989,
  #                    empleoservs_79 = mean(empleoservs)) %>%
  generate_weights(optimization_window = 1957:1989) %>%
  generate_control()

#Tabla: pesos de cada región
pesos1 <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos1$weight <- round(pesos1$weight, 3)
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

#### alquiler ####
#Proclamación pública de ciudad candidata 1981 (datos de 1957 a 1993 bianuales)
syntdat <- dataprov[!is.na(dataprov$poblalq) & !is.na(dataprov$regions) &
                      dataprov$regions != "Ceuta" & dataprov$regions != "Melilla",] %>% 
  synthetic_control(outcome = poblalq,
                    unit = regions,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1981,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1971:1977,#la media de los predictores entre los años anteriores
                     realpibturismo = mean(realpibturismo,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1979,
                     poblalq_81 = mean(poblalq)) %>%
  generate_predictor(time_window = 1977,
                     poblalq_85 = mean(poblalq)) %>%
  generate_weights(optimization_window = 1957:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos1 <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos1$weight <- round(pesos1$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento de la ocupación turismo")

#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento de la ocupación turismo")

#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético del crecimiento de la población ocupada turismo")

#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y



#Proclamación pública de ciudad candidata 1986 (datos de 1957 a 1993 bianuales)
syntdat <- dataprov[!is.na(dataprov$poblalq) & !is.na(dataprov$regions) &
                      dataprov$regions != "Ceuta" & dataprov$regions != "Melilla",] %>% 
  synthetic_control(outcome = poblalq,
                    unit = regions,
                    time = year,
                    i_unit = "Barcelona",#region de interés
                    i_time = 1986,#año de interés
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 1971:1977,#la media de los predictores entre los años anteriores
                     realpibturismo = mean(realpibturismo,na.rm = TRUE),
                     Pobl_total = mean(Pobl_total,na.rm = TRUE)) %>%
  generate_predictor(time_window = 1979,
                     poblalq_81 = mean(poblalq)) %>%
  generate_predictor(time_window = 1983,
                     poblalq_85 = mean(poblalq)) %>%
  generate_weights(optimization_window = 1957:1980) %>%
  generate_control()

#Tabla: pesos de cada región
pesos1 <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos1$weight <- round(pesos1$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
syntdat %>% plot_placebos(time_window = 1957:2015, prune =TRUE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento de la ocupación turismo")

#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1957:2015, prune =FALSE)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Diferencia entre el real y el sintético del crecimiento de la ocupación turismo")

#Gráfico datos con el sintético
syntdat %>% plot_trends(time_window = 1957:2015)+
  theme_classic() + xlab("") + ylab("") +
  ggtitle("Sintético del crecimiento de la población ocupada turismo")

#RMSPE
syntdat %>% plot_mspe_ratio(time_window = 1957:2015)+
  theme_classic() + xlab("") +
  ggtitle("Ratio RMSE post-intervención / RMSE pre-intervención")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1957:2015)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#### FIGURES ####
##################### NO OLVIDAR CORRER EL SINTÉTICO
ggarrange(ptrendagreg81, ptrendagreg86, ptrendtur81, ptrendtur86,nrow = 2, ncol = 2)

ggarrange(ptrendconst81, ptrendconst86,
          ptrendserv81, ptrendserv86, ptrendemp81,
          ptrendemp86, nrow = 3, ncol = 2)

ggarrange(pdifagreg81,pdifagreg86,pdiftur81, pdiftur86, 
          pdifconst81,pdifconst86,pdifserv81,
          pdifserv86, pdifemp81, pdifemp86,
          nrow = 5, ncol = 2)

ggarrange(pprederragreg81, pprederragreg86, pprederrtur81, pprederrtur86, nrow = 2, ncol = 2)

ggarrange(pprederrconst81, pprederrconst86, pprederrserv81, pprederrserv86,
          nrow = 2, ncol = 2)
ggarrange(pprederremp81, pprederremp86,
          nrow = 1, ncol = 2)




