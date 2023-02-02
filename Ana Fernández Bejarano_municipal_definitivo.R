setwd()
library(dplyr)
library(readxl)
library(tidyr)
library(tidysynth)
library(ggpubr)

rm(list=ls())
totdata <- read.csv("Ana Fernández Bejarano_municipal_datos.csv") %>%
        select(- X)

###### SINTÉTICO empresas ######
#Proclamación pública de ciudad candidata 1981 (datos de 1957 a 1993 bianuales)
syntdat <- totdata %>% 
        synthetic_control(outcome = empresas,
                          unit = Localidad,
                          time = year,
                          i_unit = "BARCELONA",#region de interés
                          i_time = 1981,#año de interés
                          generate_placebos = TRUE) %>%
        generate_predictor(time_window = 1965:1979,#la media de los predictores entre los años anteriores
                           telefonos = mean(telefonos,na.rm = TRUE),
                           bancos = mean(bancos,na.rm = TRUE),
                           ind_turismo = mean(ind_turismo,na.rm = TRUE)) %>%
        generate_weights(optimization_window = 1965:1979) %>%
        generate_control()

#Tabla: pesos de cada región
pesos1 <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos1$weight <- round(pesos1$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
mdifemp81 <- syntdat %>% plot_placebos(time_window = 1965:1991, prune =TRUE) +
        theme_classic() + xlab("") + ylab("") +
        ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1965:1991, prune =FALSE)

#Gráfico datos con el sintético
mtrendemp81 <- syntdat %>% plot_trends(time_window = 1965:1991)  +
        theme_classic() + xlab("") + ylab("") +
        ggtitle("") + theme(legend.position = "none")
#RMSPE
mprederremp81 <- syntdat %>% plot_mspe_ratio(time_window = 1965:1991) +
        theme_classic() + xlab("") + ylab("") +
        ggtitle("") + theme(legend.position = "none")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1965:1991)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y


###### SINTÉTICO empresas ######
#Proclamación pública de ciudad candidata 1986 (datos de 1957 a 1993 bianuales)
syntdat <- totdata %>% 
        synthetic_control(outcome = empresas,
                          unit = Localidad,
                          time = year,
                          i_unit = "BARCELONA",#region de interés
                          i_time = 1986,#año de interés
                          generate_placebos = TRUE) %>%
        generate_predictor(time_window = 1965:1985,#la media de los predictores entre los años anteriores
                           telefonos = mean(telefonos,na.rm = TRUE),
                           bancos = mean(bancos,na.rm = TRUE),
                           ind_turismo = mean(ind_turismo,na.rm = TRUE)) %>%
        generate_weights(optimization_window = 1965:1985) %>%
        generate_control()

#Tabla: pesos de cada región
pesos1 <- syntdat %>% grab_unit_weights(placebo=TRUE)
pesos1$weight <- round(pesos1$weight, 3)
#Gráfico de las diferencias entre los datos y el sintético (placebos y región de interés)
mdifemp86 <- syntdat %>% plot_placebos(time_window = 1965:1991, prune =TRUE) +
        theme_classic() + xlab("") + ylab("") +
        ggtitle("") + theme(legend.position = "none")
#Gráfico de las diferencias sin quitar anómalos
syntdat %>% plot_placebos(time_window = 1965:1991, prune =FALSE)

#Gráfico datos con el sintético
mtrendemp86 <- syntdat %>% plot_trends(time_window = 1965:1991) +
        theme_classic() + xlab("") + ylab("") +
        ggtitle("") + theme(legend.position = "none")
#RMSPE
mprederremp86 <- syntdat %>% plot_mspe_ratio(time_window = 1965:1991) +
        theme_classic() + xlab("") + ylab("") +
        ggtitle("") + theme(legend.position = "none")
#Tabla que saca: Pre-treatment RMSPE, Post-treatment RMSPE, Post-treatment/Pre-treatment RMSPE y p-value
signsynt <- syntdat %>% grab_signficance(time_window = 1965:1991)
#Tabla que saca: actual y sintético de la tasa de crecimiento de la población, PIB per capita y población ocupada (real y sintético)
syntdat %>% grab_balance_table()
#Grafico con los pesos de las regiones y los predictores
syntdat %>% plot_weights()
## Diferencias entre sintético y real de la variable de interés
syntheticadata <- grab_synthetic_control(syntdat)
syntheticadata$difference <- syntheticadata$real_y - syntheticadata$synth_y

#### FIGURES ####
ggarrange(mtrendemp81, mtrendemp86, mdifemp81, mdifemp86,
          nrow = 2, ncol =2)
ggarrange(mprederremp81, mprederremp86, nrow = 1, ncol = 2)




