setwd()
library(dplyr)
library(readxl)
library(tidyr)
library(tidysynth)
library(ggpubr)

# rm(list=ls())
totdata <- data.frame()
data <- read.csv("CSV/1963_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- rbind(totdata,locs)
colnames(totdata)[2] <- "1963"

data <- read.csv("CSV/1965_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[3] <- "1965"

data <- read.csv("CSV/1967_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[4] <- "1967"

data <- read.csv("CSV/1969_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[5] <- "1969"

data <- read.csv("CSV/1971_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[6] <- "1971"

data <- read.csv("CSV/1973_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[7] <- "1973"

data <- read.csv("CSV/1975_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[8] <- "1975"

data <- read.csv("CSV/1977_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[9] <- "1977"

data <- read.csv("CSV/1979_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[10] <- "1979"

data <- read.csv("CSV/1981_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[11] <- "1981"

data <- read.csv("CSV/1983_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[12] <- "1983"

data <- read.csv("CSV/1985_Export_1.csv",sep = ";")
locs <- as.data.frame(summarise(group_by(data,Localidad),n()))
totdata <- merge(totdata,locs,all = TRUE)
colnames(totdata)[13] <- "1985"
rm(locs)
totdata$Localidad <- gsub("TUDELA","TUDELA DE DUERO",totdata$Localidad)

data <- read_xlsx("Municipios_datos.xlsx",sheet = 1) %>%
        pivot_longer(cols = 2:16,names_to = "year", values_to = "telefonos")
data$year <- gsub("1990","1989",data$year)

totdata <- totdata[totdata$Localidad %in% unique(data$Localidad),]
totdata[is.na(totdata)] <- 0
totdata <- pivot_longer(totdata, cols = 2:13, names_to = "year", values_to = "empresas")

data2 <- read_xlsx("Municipios_datos.xlsx",sheet = 4) %>%
        pivot_longer(cols = 2:4, names_to = "year", values_to = "empresas")
totdata <- merge(totdata,data2,all = TRUE)
totdata <- merge(totdata,data,all = TRUE)

data <- read_xlsx("Municipios_datos.xlsx",sheet = 2) %>%
        pivot_longer(cols = 2:16,names_to = "year", values_to = "bancos")
data$year <- gsub("1990","1989",data$year)
totdata <- merge(totdata,data,all = TRUE)

data <- read_xlsx("Municipios_datos.xlsx",sheet = 3) %>%
        pivot_longer(cols = 2:16,names_to = "year", values_to = "ind_turismo")
data$year <- gsub("1990","1989",data$year)
totdata <- merge(totdata,data,all = TRUE)
rm(data,data2)

totdata2 <- summarize(group_by(totdata,Localidad),
                      dif_empresas = empresas - lag(empresas),
                      dif_bancos = bancos - lag(bancos),
                      dif_turismo = ind_turismo - lag(ind_turismo),
                      dif_telef = 100*(telefonos - lag(telefonos))/lag(telefonos))
totdata$empresas <- totdata2$dif_empresas
totdata$telefonos <- totdata2$dif_telef
totdata$bancos <- totdata2$dif_bancos
totdata$ind_turismo <- totdata2$dif_turismo
rm(totdata2)

totdata$year <- as.numeric(totdata$year)
totdata <- totdata[totdata$year != 1963,]

# totdata$numloc <- factor(totdata$Localidad)
# levels(totdata$numloc) <- as.character(seq(1,67))
# totdata$numloc <- as.numeric(totdata$numloc)

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
ggarrange(trendtur92, mtrendemp81, mtrendemp86, nrow = 1, ncol = 3)

ggarrange(mdifemp81, mdifemp86,
          nrow = 1, ncol =2)

ggarrange(mprederremp81, mprederremp86, nrow = 1, ncol = 2)
