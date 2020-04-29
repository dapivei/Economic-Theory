---
output: html_document
---
##  Desempleo en México {.tabset}





### Paquetería


#paquetería necesaria

library(ggplot2)
library (magrittr)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(plyr)
library(ggthemes)
library (ggmosaic)



### Revisión 



#importando los documentos necesarios
enoe_2T <- read_csv('/Users/danielapintoveizaga/Desktop/2019trim2_csv/COE2T219.csv')
enoe_1T <- read_csv('/Users/danielapintoveizaga/Desktop/2019trim2_csv/COE1T219.csv')
socio_demografico <- read_csv('/Users/danielapintoveizaga/Desktop/2019trim2_csv/SDEMT219.csv')



#|                Población Desocupada               	|                                                                     	|
#|:-------------------------------------------------:	|:-------------------------------------------------------------------:	|
#| Variable                                          	| Criterio                                                            	|
#| Iniciador                                         	| P1c= 11                                                             	|
#| Desocupados con búsqueda                          	| P1b= 2 y (P2_1=1 ó P2_2=2 ó P2_3=3) y (P2b=1) y (p2c ≠ 2 y 9)       	|
#| Ausentes sin ingreso ni nexo laboral con búsqueda 	| (P1d=2 ó 9) y (P2_1=1 ó P2_2=2 ó P2_3=3) y (P2b=1 ) y (p2c ≠ 2 y 9) 	|


#analizando las bd
glimpse (enoe_1T)
glimpse(enoe_2T)
glimpse(socio_demografico)


### Limpieza y Minería

#preparativos para el merge de las bases: excluir c_res (2: Ausente definitivo) y eda (menor que 12 o registro 99), nos quedamos con 323,624 observaciones
socio_demografico <- socio_demografico[socio_demografico$c_res!=2 & socio_demografico$eda > 11 & socio_demografico$r_def==00 & socio_demografico$eda !=99, ]
dim(socio_demografico)
#selección de variables de interés
socio_demografico <- subset(socio_demografico, select = c("cd_a","ent","con","n_hog","h_mud","v_sel","n_ren", "eda", "sex", "fac"))
enoe_1T <- subset(enoe_1T, select=c("cd_a","ent","con","n_hog","h_mud","v_sel","n_ren", "p1", "p1a1", "p1a2", "p1c", "p1d", "p1e", "p1b", "p2_1", "p2_2", "p2_3", "p2b", "p2c", "p1d"))
dim(socio_demografico)
dim(enoe_1T)
#merge de bd
registro_encuestados <- merge(socio_demografico, enoe_1T)
#head(registro_encuestados)
glimpse(registro_encuestados)

#generación de población "ocupados"

ocupados <- subset(registro_encuestados, 
                  ( eda >=15 & eda <= 98 & 
                  #ocupados en primera instancia
                  p1==1 |
                  #ocupados en segunda instancia
                  p1a1==1 | p1a1=='blanco'| p1a2==2 |
                  #ocupados en segunda instancia  
                  p1c== 01 | p1c== 02 | p1c == 03| p1c== 04 |
                  p1d == 1|
                  #ocupados en cuarta instancia
                  p1d==2 | p1d==9 | p1e==1))
                   
glimpse(ocupados)
#head(ocupados)

#generación de población desocupada

desocupados <- subset(registro_encuestados, (eda >=15 & 
               eda <= 98 & 
               #iniciadores
               (p1c==11 |
               #desocupados en busqueda
               (p1b==2 & (p2_1==1| p2_2==2 | p2_3==3) &
               p2b==1 & (p2c!=2 & p2c!=9)) | 
              #ausentes sin ingreso ni nexo laboral con búsqueda: 
              #((P1d=2 o9) y (P2_1=1 oP2_2=2 oP2_3=3) y (P2b=1 ) y(p2c ≠ 2 y 9)
               ((p1d==2| p1d==9) & (p2_1 == 1| p2_2==2 | p2_3 == 3) &
               p2b==1 & (p2c != 2 & p2c!=9)))))

glimpse(desocupados)
#head(desocupados)

#constriuyendo bd de pea
desocupados<-mutate(desocupados, 'situacion laboral' = 'Desocupados')
ocupados <- mutate(ocupados, 'situacion laboral' = 'Ocupados')
pea <- rbind(desocupados,ocupados)
dim(pea)

#renaming variables in bd desocupados
desocupados$sex[desocupados$sex=="1"] <- "Hombres"
desocupados$sex[desocupados$sex=="2"] <- "Mujeres"
desocupados$ent[desocupados$ent=="1"] <- "Ags"
desocupados$ent[desocupados$ent=="2"] <- "BC"
desocupados$ent[desocupados$ent=="3"] <- "BCS"
desocupados$ent[desocupados$ent=="4"] <- "Camp"
desocupados$ent[desocupados$ent=="5"] <- "Coah"
desocupados$ent[desocupados$ent=="6"] <- "Col"
desocupados$ent[desocupados$ent=="7"] <- "Chis"
desocupados$ent[desocupados$ent=="8"] <- "Chih"
desocupados$ent[desocupados$ent=="9"] <- "CDMX"
desocupados$ent[desocupados$ent=="10"] <- "Dgo"
desocupados$ent[desocupados$ent=="11"] <- "Gto"
desocupados$ent[desocupados$ent=="12"] <- "Gro"
desocupados$ent[desocupados$ent=="13"] <- "Hgo"
desocupados$ent[desocupados$ent=="14"] <- "Jal"
desocupados$ent[desocupados$ent=="15"] <- "Mex"
desocupados$ent[desocupados$ent=="16"] <- "Mich"
desocupados$ent[desocupados$ent=="17"] <- "Mor"
desocupados$ent[desocupados$ent=="18"] <- "Nay"
desocupados$ent[desocupados$ent=="19"] <- "NL"
desocupados$ent[desocupados$ent=="20"] <- "Oaxaca"
desocupados$ent[desocupados$ent=="21"] <- "Pue"
desocupados$ent[desocupados$ent=="22"] <- "Qro"
desocupados$ent[desocupados$ent=="23"] <- "Q_Roo"
desocupados$ent[desocupados$ent=="24"] <- "SLP"
desocupados$ent[desocupados$ent=="25"] <- "Sin"
desocupados$ent[desocupados$ent=="26"] <- "Son"
desocupados$ent[desocupados$ent=="27"] <- "Tab"
desocupados$ent[desocupados$ent=="28"] <- "Tamps"
desocupados$ent[desocupados$ent=="29"] <- "Tlax"
desocupados$ent[desocupados$ent=="30"] <- "Ver"
desocupados$ent[desocupados$ent=="31"] <- "Yuc"
desocupados$ent[desocupados$ent=="32"] <- "Zac"





### Gráficas (Sin FAC)


#PEA
ggplot(data = pea) +
  geom_bar(mapping = aes(x = "situacion laboral")) 

#desocupados por sexo
ggplot(data = desocupados) +
  geom_bar(mapping = aes(x = desocupados$sex, fill = desocupados$sex)) + ggtitle("Desocupados Por Sexo")

#desocupados por edad

ggplot(data = desocupados) +
  geom_bar(mapping = aes(x = desocupados$eda, fill = desocupados$sex)) + ggtitle("Desocupados Por Edad")  + labs(y="Frecuencia", x = "Edad", fill="Sexo") + theme(axis.text.x = element_text(angle = 90))


#gemo_bar de desocupados por estado de república
ggplot(data = desocupados) +
  geom_bar(mapping = aes(x = desocupados$ent, fill = desocupados$ent)) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Desocupados Por Entidad Federativa") + labs(y="Frecuencia", x = "Entidad Federativa", fill = "Entidad Federativa")




### Gráficas (Con FAC)




#PEA
ggplot(data = pea) +
  geom_bar(mapping = aes(x = `situacion laboral`, weight=fac, fill = 'situacion laboral')) + ggtitle("Población Económicamente Activa")  + labs(y="Frecuencia", x = "Situación Laboral", fill=element_blank(), caption = "Fuente: ENOE, Segundo Trimestre 2019", subtitle = "Por Situación Laboral") + theme_dark() + theme(legend.position = "none")


ggplot(data = desocupados) +
  geom_bar(mapping = aes(x = desocupados$sex, fill = desocupados$sex, weight=fac)) + ggtitle("Población Desocupada")  + labs(y="Frecuencia", x = "Sexo", fill="Sexo", caption="Fuente: ENOE, Segundo Trimestre 2019", subtitle = "Por Sexo")




ggplot(data = desocupados, mapping = aes(x = desocupados$eda, color = desocupados$sex, weight=fac)) + geom_histogram(binwidth = 5) + ggtitle("Población Desocupada")  + labs(y="Frecuencia", x = "Edad", color= "Sexo", caption = "Fuente: ENOE, Segundo Trimestre 2019", subtitle = "Por Edad y Sexo") + 
  theme(axis.text.x = element_text(angle = 90))





ggplot(data = desocupados) +
  geom_bar(mapping = aes(x = desocupados$ent, fill = desocupados$sex, weight=fac)) + theme_economist() + theme(axis.text.x = element_text(angle =80)) + ggtitle("Población Desocupada") + labs(y="Frecuencia", x=element_blank(), subtitle= "Por Entidad Federativa y Sexo", fill = "Sexo") + scale_fill_economist()




ggplot(data = desocupados) +
   geom_mosaic(aes(x= product(sex), fill=ent, weight=fac), na.rm=TRUE) + ggtitle("Desocupados Por Entidad Federativa y Sexo") + labs(y="Entidad Federativa", x= "Sexo", subtitle = "Porcentaje",
              caption = "Fuente: ENOE, Segundo Trimestre 2019", fill = "Entidad Federativa")



### Cálculos


#tasa de desempleo: aplicación de factores de expansión  
desocupados_factor <- sum(desocupados$fac)
desocupados_factor
ocupados_factor<-sum(ocupados$fac)
ocupados_factor
pea_factor=desocupados_factor+ocupados_factor
pea_factor
tasa_desempleo=(desocupados_factor/pea_factor)*100
tasa_desempleo

#desempleo: porcentaje 25 o más o 24 o menos
des_25omas <- filter(desocupados, eda >= 25)
v_des_25omas <- sum(des_25omas$fac)

des_24omenos <-filter(desocupados, eda < 25)
v_des_24omenos <- sum(des_24omenos$fac)

p_desempleo_25omas = (v_des_25omas/desocupados_factor)*100
p_desempleo_25omas

p_desempleo_24omenos = (v_des_24omenos/desocupados_factor)*100
p_desempleo_24omenos

#Desempleo: 25 o mas y sexo

des_25omasmujer <- filter(des_25omas, sex=='Mujeres')
head(des_25omasmujer)
v_25omasmujer=sum(des_25omasmujer$fac)
v_25omasmujer

p_25omasmujer=(v_25omasmujer/desocupados_factor)*100
p_25omasmujer

p_25omashombre = 100- p_25omasmujer
p_25omashombre

#Desempleo: 24 o menos y sexo

des_24omenosmujer <- filter(des_24omenos, sex=='Mujeres')
head (des_24omenosmujer)
v_24omenosmujer=sum(des_24omenosmujer$fac)
v_24omenosmujer

p_24omenosmujer=(v_24omenosmujer/desocupados_factor)*100
p_24omenosmujer

p_24omenoshombre = 100- p_24omenosmujer
p_24omenoshombre

#Desempleo: Porcentaje por estado

des_ags <- filter(desocupados, ent == "Ags")
v_ags=sum(des_ags$fac)
p_ags=(v_ags/desocupados_factor)*100

des_bc <- filter(desocupados, ent == "BC")
v_bc=sum(des_bc$fac)
p_bc=(v_bc/desocupados_factor)*100

des_bcs <- filter(desocupados, ent == "BCS")
v_bcs=sum(des_bcs$fac)
p_bcs=(v_bcs/desocupados_factor)*100

des_camp <- filter(desocupados, ent == "Camp")
v_camp=sum(des_camp$fac)
p_camp=(v_camp/desocupados_factor)*100

des_coah <- filter(desocupados, ent == "Coah")
v_coah=sum(des_coah$fac)
p_coah=(v_coah/desocupados_factor)*100

des_col <- filter(desocupados, ent == "Col")
v_col=sum(des_col$fac)
p_col=(v_col/desocupados_factor)*100

des_chis <- filter(desocupados, ent == "Chis")
v_chis=sum(des_chis$fac)
p_chis=(v_chis/desocupados_factor)*100

des_chih <- filter(desocupados, ent == "Chih")
v_chih=sum(des_chih$fac)
p_chih=(v_chih/desocupados_factor)*100

des_cdmx <- filter(desocupados, ent == "CDMX")
v_cdmx=sum(des_cdmx$fac)
p_cdmx=(v_cdmx/desocupados_factor)*100

des_dgo <- filter(desocupados, ent == "Dgo")
v_dgo=sum(des_dgo$fac)
p_dgo=(v_dgo/desocupados_factor)*100

des_gto <- filter(desocupados, ent == "Gto")
v_gto=sum(des_gto$fac)
p_gto=(v_gto/desocupados_factor)*100

des_gro <- filter(desocupados, ent == "Gro")
v_gro=sum(des_gro$fac)
p_gro=(v_gro/desocupados_factor)*100

des_hgo <- filter(desocupados, ent == "Hgo")
v_hgo=sum(des_hgo$fac)
p_hgo=(v_hgo/desocupados_factor)*100

des_jal <- filter(desocupados, ent == "Jal")
v_jal=sum(des_jal$fac)
p_jal=(v_jal/desocupados_factor)*100

des_mex <- filter(desocupados, ent == "Mex")
v_mex=sum(des_mex$fac)
p_mex=(v_mex/desocupados_factor)*100

des_mich <- filter(desocupados, ent == "Mich")
v_mich=sum(des_mich$fac)
p_mich=(v_mich/desocupados_factor)*100

des_mor <- filter(desocupados, ent == "Mor")
v_mor=sum(des_mor$fac)
p_mor=(v_mor/desocupados_factor)*100

des_nay <- filter(desocupados, ent == "Nay")
v_nay=sum(des_nay$fac)
p_nay=(v_nay/desocupados_factor)*100

des_nl <- filter(desocupados, ent == "NL")
v_nl=sum(des_nl$fac)
p_nl=(v_nl/desocupados_factor)*100

des_oaxaca <- filter(desocupados, ent == "Oaxaca")
v_oaxaca=sum(des_oaxaca$fac)
p_oaxaca=(v_oaxaca/desocupados_factor)*100

des_pue <- filter(desocupados, ent == "Pue")
v_pue=sum(des_pue$fac)
p_pue=(v_pue/desocupados_factor)*100

des_qro <- filter(desocupados, ent == "Qro")
v_qro=sum(des_qro$fac)
p_qro=(v_qro/desocupados_factor)*100

des_q_roo <- filter(desocupados, ent == "Q_Roo")
v_q_roo=sum(des_q_roo$fac)
p_qroo=(v_q_roo/desocupados_factor)*100

des_slp <- filter(desocupados, ent == "SLP")
v_slp=sum(des_slp$fac)
p_slp=(v_slp/desocupados_factor)*100

des_sin <- filter(desocupados, ent == "Sin")
v_sin=sum(des_sin$fac)
p_sin=(v_sin/desocupados_factor)*100

des_son <- filter(desocupados, ent == "Son")
v_son=sum(des_son$fac)
p_son=(v_son/desocupados_factor)*100

des_tab <- filter(desocupados, ent == "Tab")
v_tab=sum(des_tab$fac)
p_tab=(v_tab/desocupados_factor)*100

des_tamps <- filter(desocupados, ent == "Tamps")
v_tamps=sum(des_tamps$fac)
p_tamps=(v_tamps/desocupados_factor)*100

des_tlax <- filter(desocupados, ent == "Tlax")
v_tlax=sum(des_tlax$fac)
p_tlax=(v_tlax/desocupados_factor)*100

des_ver <- filter(desocupados, ent == "Ver")
v_ver=sum(des_ver$fac)
p_ver=(v_ver/desocupados_factor)*100

des_yuc <- filter(desocupados, ent == "Yuc")
v_yuc=sum(des_yuc$fac)
p_yuc=(v_yuc/desocupados_factor)*100

des_zac <- filter(desocupados, ent == "Zac")
v_zac=sum(des_zac$fac)
p_zac=(v_zac/desocupados_factor)*100

p_ags
p_bc
p_bcs
p_camp
p_coah
p_col
p_chis
p_chih
p_cdmx
p_dgo
p_gto
p_gro
p_hgo
p_jal
p_mex
p_mich
p_mor
p_nay
p_nl
p_oaxaca
p_pue
p_qro
p_qroo
p_slp
p_sin
p_son
p_tab
p_tamps
p_tlax
p_ver
p_yuc
p_zac


