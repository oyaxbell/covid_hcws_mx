#Assessing the burden of COVID-19 amongst healthcare workers in Mexico City: A data-driven call to action
#Data Analysis: Neftali Eduardo Antonio-Villa (nefoantonio@hotmail.com); Omar Yaxmehen Bello-Chavolla (oyaxbell@yahoo.com.mx)
#Latest version of Analysis 20-Sept-2020
#Any question regarding analysis contact Neftali Eduardo Antonio-Villa or Omar Yaxmehen Bello-Chavolla
#Disclaimer: The current dataset in part of the open-dataset from SINAVE-CDMX (https://datos.cdmx.gob.mx/explore/dataset/base-covid-sinave/export/)


#### Database management ####
#Library used#
library(tidyverse); library(dummies);library(lmerTest);library(lme4); library(polypoly)
library(dummies); library(glmnet);library(survival)
library(survminer)
library(jtools)
library(caret)
library(OptimalCutpoints)
library(pROC)
library(epiR)
library(reportROC)
library(haven)
library(lme4)
library(ggthemes)
library(cowplot)
library(readxl)
library(ggsci)

#Download the latest version of SINAVE dataset
temp <- tempfile()
download.file("https://datos.cdmx.gob.mx/explore/dataset/base-covid-sinave/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C",temp)
cdmx <- read.csv(temp,header=TRUE,sep = ",", encoding = "UTF-8")
write.csv(cdmx, "base-covid-sinave.csv")
unlink(temp)
#Personal use##
setwd("C:/Users/HP-PC/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/COVID-CDMX")
setwd("/Users/nefoantonio/UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/OMAR YAXMEHEN BELLO CHAVOLLA - COVID-CDMX/Personal de Salud")
cdmx<-read.csv("base-covid-sinave.csv")
#Data base managment
cdmx[cdmx=="SE IGNORA"]<-NA
cdmx$covid[cdmx$resultado_definitivo=="SARS-CoV-2"]<-1;cdmx$covid[cdmx$resultado_definitivo!="SARS-CoV-2"]<-0
cdmx[,c(19,20,23,25:26,32:61,81,84)]<-sapply(cdmx[,c(19,20,23,25:26,32:61,81,84)], as.numeric)
cdmx[,c(19,20,23,25:26,32:61,81,84)][cdmx[,c(19,20,23,25:26,32:61,81,84)]==1]<-0;cdmx[,c(19,20,23,25:26,32:61,81,84)][cdmx[,c(19,20,23,25:26,32:61,81,84)]==3]<-1
cdmx$fecha_defuncion<-as.character(cdmx$fecha_defuncion);cdmx$fecha_defuncion[cdmx$fecha_defuncion==""]<-NA
cdmx$unidad_cuidados_intensivos[is.na(cdmx$unidad_cuidados_intensivos)]<-0;cdmx$intubado[is.na(cdmx$intubado)]<-0
cdmx$FU_time_MORT<-as.Date(cdmx$fecha_defuncion)-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$fecha_inicio_sintomas<-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$FU_time_MORT[is.na(cdmx$FU_time_MORT)]<-as.Date(cdmx$fecha_ingreso)-as.Date(cdmx$fecha_inicio_sintomas);cdmx$FU_time_MORT<-as.numeric(cdmx$FU_time)
cdmx$FU_time_HOSP<-as.Date(cdmx$fecha_ingreso)-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$fecha_inicio_sintomas<-as.Date(cdmx$fecha_inicio_sintomas)
cdmx$FU_time_HOSP[is.na(cdmx$FU_time_HOSP)]<-as.Date(cdmx$fecha_ingreso)-as.Date(cdmx$fecha_inicio_sintomas);cdmx$FU_time_HOSP<-as.numeric(cdmx$FU_time_HOSP)
cdmx$numero_sintomas<-cdmx$fiebre+cdmx$tos+cdmx$disnea+cdmx$irritabilidad+cdmx$odinofagia+cdmx$diarrea+
  cdmx$dolor_toracico+cdmx$calofrios+cdmx$cefalea+cdmx$mialgias+cdmx$artralgias+cdmx$ataque_al_estado_general+
  cdmx$rinorrea+cdmx$polipnea+cdmx$vomito+cdmx$conjuntivitis+cdmx$cianosis+cdmx$inicio_subito_sintomas
cdmx$comorb<-cdmx$diabetes+cdmx$hipertension+cdmx$enfermedad_cardiaca+cdmx$insuficiencia_renal_cronica+
  cdmx$asma+cdmx$VIH_SIDA+cdmx$epoc+cdmx$obesidad+cdmx$tabaquismo+cdmx$otra_condicion
cdmx$asintomaticos[cdmx$numero_sintomas>0]<-0;cdmx$asintomaticos[cdmx$numero_sintomas==0]<-1
d1<-dummies::dummy(cdmx$evolucion_caso)
cdmx<-cbind(cdmx, d1)
d2<-dummies::dummy(cdmx$ocupacion)
cdmx<-cbind(cdmx, d2)
table(cdmx$diagnostico_clinico_neumonia)
NAC_clin<-NULL;NAC_clin[cdmx$diagnostico_clinico_neumonia==1]<-1;NAC_clin<-na.tools::na.replace(NAC_clin,0)
edad65<-NULL;edad65[cdmx$edad>=65]<-1;edad65[cdmx$edad<65]<-0
edad40<-NULL;edad40[cdmx$edad<=40]<-1;edad40[cdmx$edad>40]<-0
DM2_edad40<-NULL;DM2_edad40[edad40==1 & cdmx$diabetes==1]<-1;DM2_edad40<-na.tools::na.replace(DM2_edad40,0)
pers_sal<-cdmx$ocupacionMEDICOS+cdmx$ocupacionENFERMERAS+cdmx$ocupacionDENTISTAS+cdmx$`ocupacionOTROS TRABAJADORES DE LA SALUD`+
  cdmx$ocupacionLABORATORISTAS;pers_sal<-na.tools::na.replace(pers_sal,0)
pers_sal_cat<-NULL;pers_sal_cat[cdmx$ocupacionMEDICOS==1]<-1;pers_sal_cat[cdmx$ocupacionENFERMERAS==1]<-2;
pers_sal_cat[cdmx$ocupacionDENTISTAS==1]<-3;pers_sal_cat[cdmx$`ocupacionOTROS TRABAJADORES DE LA SALUD`==1]<-4;
pers_sal_cat[cdmx$ocupacionLABORATORISTAS==1]<-5;
pers_sal_cat_2<-NULL;pers_sal_cat_2[pers_sal_cat==1]<-2;pers_sal_cat_2[pers_sal_cat>=2]<-1
pers_sal_cat_3<-NULL;pers_sal_cat_3[pers_sal_cat==1]<-3;pers_sal_cat_3[pers_sal_cat==2]<-1;pers_sal_cat_3[pers_sal_cat>=3]<-2
indi_rec<-NULL;indi_rec[cdmx$es_indigena==1]<-1;indi_rec[cdmx$es_indigena==2]<-0
cdmx$intubado[cdmx$intubado==2]<-0
HOSP<-NULL;HOSP[cdmx$tipo_paciente=="HOSPITALIZADO"]<-1;HOSP<-na.tools::na.replace(HOSP,0)
cdmx$numero_sintomas<-cdmx$fiebre+cdmx$tos+cdmx$disnea+cdmx$irritabilidad+cdmx$odinofagia+cdmx$diarrea+
  cdmx$dolor_toracico+cdmx$calofrios+cdmx$cefalea+cdmx$mialgias+cdmx$artralgias+cdmx$ataque_al_estado_general+
  cdmx$rinorrea+cdmx$polipnea+cdmx$vomito+cdmx$conjuntivitis+cdmx$cianosis+cdmx$inicio_subito_sintomas
cdmx$asintomaticos[cdmx$numero_sintomas>0]<-0;cdmx$asintomaticos[cdmx$numero_sintomas==0]<-1
cdmx$asintomaticos<-na.tools::na.replace(cdmx$asintomaticos,0)
caso_grave<-cdmx$`evolucion_casoCASO GRAVE -`
tiempo_prolong<-NULL;tiempo_prolong[cdmx$FU_time_HOSP>=7]<-1;tiempo_prolong[cdmx$FU_time_HOSP<7]<-0
cdmx$contacto_viral<-NULL; cdmx$contacto_viral[cdmx$contacto_infeccion_viral=="SI"]<-1;
cdmx$contacto_viral<-na.tools::na.replace(cdmx$contacto_viral, 0)
cdmx$viaje_all<-NULL;cdmx$viaje_all[cdmx$viaje_1!=""]<-1
cdmx$viaje_all[cdmx$viaje_2!=""]<-2
cdmx$viaje_all[cdmx$viaje_3!=""]<-3
cdmx$viaje_all[cdmx$viaje_4!=""]<-4
cdmx$viaje_all[cdmx$viaje_5!=""]<-5
cdmx$viaje_sum<-NULL;cdmx$viaje_sum[cdmx$viaje_all>=1]<-1
cdmx$viaje_sum<-na.tools::na.replace(cdmx$viaje_sum, 0)
cdmx$severe_covid19[cdmx$evolucion_casoDEFUNCION+cdmx$intubado+cdmx$unidad_cuidados_intensivos>0]<-1;cdmx$severe_covid19[is.na(cdmx$severe_covid19)]<-0
cdmx<-cbind(cdmx,NAC_clin,edad65,edad40,DM2_edad40,pers_sal,indi_rec,HOSP,pers_sal_cat,pers_sal_cat_2,pers_sal_cat_3,tiempo_prolong,caso_grave)

#COVID positive
cdmx1<- cdmx %>% filter(resultado_definitivo=="SARS-CoV-2")
cdmx1_sal<-cdmx%>% filter(pers_sal==1)

#Populational based data
#HCWs
pop_hcw<- read_excel("Poblacion_HCW.xlsx")
table(pop_hcw$ID,pop_hcw$Personal_médico_en_formación)
pop_hcw$medicos<-pop_hcw$`Médicos generales_especialistas_Odontólogos`+pop_hcw$Personal_médico_en_formación+pop_hcw$Médicos_en_otras_labores
pop_hcw$enfermeras<-pop_hcw$Enfermeras_generales_especialistas+pop_hcw$Pasantes_de_enfermería+pop_hcw$Auxiliares_de_enfermería+pop_hcw$Personal_de_enfermería_en_otras_labores
pop_hcw$otros_hcw<-pop_hcw$Personal_profesional+pop_hcw$Personal_técnico+pop_hcw$Otro_personal
pop_hcw$TOTAL<-pop_hcw$medicos+pop_hcw$enfermeras+pop_hcw$otros_hcw

#General Population
pop<-read.csv("conapo.csv")
names(pop)<-c("r", "year", "ENTIDAD", "ID", "EDAD", "SEXO", "pop")
pop<-pop %>% filter(year==2020)%>% group_by(ID) %>%
  summarise(pop=sum(pop))%>%dplyr::select(ID, pop) %>% 
  filter(ID%in% c("9","15")) 
pop$ID<-str_pad(pop$ID, 2,pad = "0")

#Merge the dataset
cdmx1_pos<- cdmx %>% filter(cve_entidad_unidad_medica%in% c("9")) 
cdmx1_pos$ID<-str_pad(cdmx1_pos$cve_entidad_unidad_medica, 2,pad = "0")
cdmx1_pos<- cdmx1_pos %>% left_join(pop_hcw, by="ID") 
cdmx1_pos<- cdmx1_pos %>% left_join(pop, by="ID") 

#----HCW Cathegories(Figure 1)----

rate_hcws_grupos_hc<- cdmx1_pos %>%
  mutate(week=data.table::week(fecha_ingreso))%>%
  group_by(week,pers_sal)%>%
  summarise(cases=sum(covid), tested=n(), deaths=sum(evolucion_casoDEFUNCION==1 & covid==1), asinto=sum(asintomaticos))

rate_hcws_grupos_hc$pop<-NULL;
rate_hcws_grupos_hc$pop[rate_hcws_grupos_hc$pers_sal==0]<-median(cdmx1_pos$pop)-median(cdmx1_pos$TOTAL)
rate_hcws_grupos_hc$pop[rate_hcws_grupos_hc$pers_sal==1]<-median(cdmx1_pos$TOTAL)

rate_hcws_grupos_hc<- rate_hcws_grupos_hc %>%
  group_by(week,pers_sal)%>%
  mutate(rate_incidence=(cases/pop*100000), 
         rate_mort=(deaths/pop*100000), 
         rate_letal=(deaths/cases)*100,
         rate_pruebas=(tested/pop*100000), 
         rate_asinto=(asinto/pop*100000),
         positivity_rate=(cases/tested)*100)%>%
  dplyr::select(week, cases, pop, tested, deaths, asinto, rate_incidence,rate_mort,rate_letal,rate_pruebas,rate_asinto,positivity_rate)%>%
  filter(week>10 & week<38)

rate_hcws_grupos_hc$pers_sal<-factor(rate_hcws_grupos_hc$pers_sal, labels = c("Non-HCWs (General Population)", "Health-Care Workers"))
rate_hcws_grupos_hc$asinto
tapply(rate_hcws_grupos_hc$asinto, rate_hcws_grupos_hc$pers_sal, summary)

#Graficas

Figure1_A<-rate_hcws_grupos_hc %>% 
  ggplot(aes(x=week, y=cases, group=pers_sal, col=pers_sal))+
  geom_line(size=1, linetype ="twodash")+
  geom_point(size=2)+
  theme_hc()+ylab("Confirmed Cases 
(7-day rolling average)")+
  xlab("Epidemiological week, 2020")+
  labs(col="Population")+
  scale_x_continuous(breaks = seq(from = 10, to = 38, by = 2))+
  scale_y_log10()+
  scale_colour_jama()+guides(colour = guide_legend(nrow = 1))

#Positivity Rate
Figure1_B<-rate_hcws_grupos_hc %>% 
  ggplot(aes(x=week, y=positivity_rate,group=pers_sal, col=pers_sal))+
  geom_line(size=1, linetype ="twodash")+
  geom_point(size=2)+
  theme_hc()+ylab("Positivity Rate 
(7-day rolling average)")+
  xlab("Epidemiological week, 2020")+
  labs(col="Population")+
  scale_x_continuous(breaks = seq(from = 10, to = 38, by = 2))+
  scale_y_log10()+
  scale_colour_jama()+guides(colour = guide_legend(nrow = 3))

#Letality Rate
Figure1_C<-rate_hcws_grupos_hc %>% 
  ggplot(aes(x=week, y=rate_letal,group=pers_sal, col=pers_sal))+
  geom_line(size=1,linetype ="twodash")+
  geom_point(size=2)+
  theme_hc()+ylab("Letality Rate 
(7-day rolling average)")+
  xlab("Epidemiological week, 2020")+
  labs(col="Population")+
  scale_x_continuous(breaks = seq(from = 10, to = 38, by = 2))+
  scale_y_log10()+
  scale_colour_jama()+guides(colour = guide_legend(nrow = 3))+
  theme(legend.position="none")

#Numero de Asintomaticos
Figure1_D<-rate_hcws_grupos_hc %>% 
  ggplot(aes(x=week, y=asinto, group=pers_sal, col=pers_sal))+
  geom_line(size=1,linetype ="twodash")+
  geom_point(size=2)+
  theme_hc()+ylab("Asymptomatic Cases 
(7-day rolling average)")+
  xlab("Epidemiological week, 2020")+
  labs(col="HCW Category")+
  scale_x_continuous(breaks = seq(from = 10, to = 38, by = 2))+
  scale_y_log10()+
  scale_colour_jama()+guides(colour = guide_legend(nrow = 1))+
  theme(legend.position="none")

#Indicence rate
Figure1_E<-rate_hcws_grupos_hc %>% 
  ggplot(aes(x=week, y=rate_incidence, group=pers_sal, col=pers_sal))+
  geom_line(size=1,linetype ="twodash")+
  geom_point(size=2)+
  theme_hc()+ylab("Incidence Rate per 100,000 inhabitants 
(7-day rolling average)")+
  xlab("Epidemiological week, 2020")+
  labs(col="Population")+
  scale_x_continuous(breaks = seq(from = 10, to = 38, by = 2))+
  scale_y_log10()+
  scale_colour_jama()+guides(colour = guide_legend(nrow = 3))+
  theme(legend.position="none")

#Testing rate
Figure1_F<-rate_hcws_grupos_hc %>% 
  ggplot(aes(x=week, y=rate_pruebas,group=pers_sal, col=pers_sal))+
  geom_line(size=1,linetype ="twodash")+
  geom_point(size=2)+
  theme_hc()+ylab("Testing Performed per 100,000 inhabitants 
(7-day rolling average)")+
  xlab("Epidemiological week, 2020")+
  labs(col="Population")+
  scale_x_continuous(breaks = seq(from = 10, to = 38, by = 2))+
  scale_y_log10()+
  scale_colour_jama()+guides(colour = guide_legend(nrow = 3))+
  theme(legend.position="none")
#Mortality Rate

Figure1_G<-rate_hcws_grupos_hc %>% 
  ggplot(aes(x=week, y=rate_mort,group=pers_sal, col=pers_sal))+
  geom_line(size=1,linetype ="twodash")+
  geom_point(size=2)+
  theme_hc()+ylab("Mortality Rate per 100,000 inhabitants 
(7-day rolling average)")+
  xlab("Epidemiological week, 2020")+
  labs(col="Population")+
  scale_x_continuous(breaks = seq(from = 10, to = 38, by = 2))+
  scale_y_log10()+
  scale_colour_jama()+guides(colour = guide_legend(nrow = 3))+
  theme(legend.position="none")

#Rate de Asintomaticos
Figure1_H<-rate_hcws_grupos_hc %>% 
  ggplot(aes(x=week, y=rate_asinto,group=pers_sal, col=pers_sal))+
  geom_line(size=1,linetype ="twodash")+
  geom_point(size=2)+
  theme_hc()+ylab("Asymptomatic Rate per 100,000 inhabitants 
(7-day rolling average)")+
  xlab("Epidemiological week, 2020")+
  labs(col="HCW Category")+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(from = 10, to = 38, by = 2))+
  scale_colour_jama()+guides(colour = guide_legend(nrow = 1))+
  theme(legend.position="none")


Figure_1<-ggarrange( Figure1_A,Figure1_B,Figure1_D,Figure1_C,
                     Figure1_E,Figure1_F,Figure1_H,Figure1_G,
                     common.legend = T,nrow = 2, ncol = 4, labels = c("A", "B", "C", "D",
                                                                      "D", "E", "F", "G"))

ggsave(file = "Figure_1.png", 
       Figure_1,
       width = 58, 
       height = 25,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

#----Epidemiological Rates (Analysis Figure 1) ----

rate_hcws_grupos_hc<- cdmx1_pos %>%
  group_by(pers_sal)%>%
  summarise(cases=sum(covid), tested=n(), deaths=sum(evolucion_casoDEFUNCION==1 & covid==1), asinto=sum(asintomaticos))

rate_hcws_grupos_hc$pop<-NULL;
rate_hcws_grupos_hc$pop[rate_hcws_grupos_hc$pers_sal==0]<-median(cdmx1_pos$pop)-median(cdmx1_pos$TOTAL)
rate_hcws_grupos_hc$pop[rate_hcws_grupos_hc$pers_sal==1]<-median(cdmx1_pos$TOTAL)

rate_hcws_grupos_hc<- rate_hcws_grupos_hc %>%
  group_by(pers_sal)%>%
  mutate(rate_incidence=((cases/pop)*100000), 
         rate_mort=((deaths/pop)*100000), 
         rate_letal=((deaths/cases)*100),
         rate_pruebas=((tested/pop)*100000), 
         rate_asinto=((asinto/pop)*100000),
         positivity_rate=((cases/tested)*100))%>%
  dplyr::select(cases, pop, tested, deaths, asinto, rate_incidence,rate_mort,rate_letal,rate_pruebas,rate_asinto,positivity_rate)

head(rate_hcws_grupos_hc)

#----Risk Models (Figure 2)----

#Modelo de Cox de Hospitalización-
m5<-coxph(Surv(FU_time_HOSP, HOSP)~factor(pers_sal_cat_3)+edad65+obesidad+diabetes+VIH_SIDA+hipertension+polipnea+disnea+fiebre+
            diarrea+conjuntivitis+odinofagia+frailty(cve_entidad_unidad_medica), data=cdmx1_sal)
cox.zph(m5); 
summary(m5)

HR<-as.data.frame(cbind(exp(coef(m5)),exp(confint(m5))))
HR
HR$Covariate<-c("Other HCWs",
                "Physicians",
                "Age ≥65 years", 
                "Obesity", 
                "Diabetes",
                "HIV/AIDS", "Arterial hypertension", 
                "Polypnea at evaluation", 
                "Dysnea at evaluation",
                "Fever at evaluation", 
                "Diarrhea at evaluation",
                "Conjunctivitis at evaluation", 
                "Odynophagia at evaluation")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
h4 <- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 1, color = "#e08f44") +
  geom_point(size = 2, color = "white") +
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("Risk for Hospitalization in Health-Care Workers with SARS-CoV-2, Hazard ratio (HR, 95%CI)")+
  scale_x_log10(limits=c(0.1,45))
h4

#Modelo de Cox COVID GRAVE

m4<-coxph(Surv(FU_time_MORT, severe_covid19)~factor(pers_sal_cat_3)+edad65+obesidad+diabetes+disnea+polipnea+fiebre
          +frailty(cve_entidad_unidad_medica), data=cdmx1_sal)
summary(m4)
HR<-as.data.frame(cbind(exp(coef(m4)),exp(confint(m4))))
HR
HR$Covariate<-c("Other HCWs",
                "Physicians",
                "Age ≥65 years", 
                "Obesity",
                "Diabetes",
                "Dysnea at evaluation", 
                "Polypnea at evaluation", 
                "Fever at evaluation")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
h3 <- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 1, color = "#e08f44") +
  geom_point(size = 2, color = "white") +
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("Risk for Severe Outcome in Health-Care Workers with SARS-CoV-2, Hazard ratio (HR, 95%CI)")+
  scale_x_log10(limits=c(0.1,45))

h3
#Modelo de Cox INTUBACION

m2_or<-glm(intubado~+factor(pers_sal_cat_3)+edad65+disnea+tiempo_prolong+obesidad+diabetes+diabetes*edad40,family="binomial", data=cdmx1_sal)
summary(m2_or)
OR<-as.data.frame(cbind(exp(coef(m2_or)),exp(confint(m2_or))))
OR<-OR[-c(1),]
OR$Covariate<-c("Other HCWs", "Physicians","Age ≥65 years", "Dysnea at Evaluation", "≥7 Days for Clinical Assessment",
                "Obesity","Diabetes","Age <40 years","Early Onset Diabetes 
(Diabetes and Age <40 years)")
colnames(OR)<-c("OR", "ciLow", "ciHigh", "Covariate")
OR$breaks<-seq(1:nrow(OR))

h2<-ggplot(OR, aes(x = OR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 1, color = "#e08f44") +
  geom_point(size = 2, color = "white") +
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = OR$breaks, labels = OR$Covariate) +
  ylab("") +
  xlab("Associated Factors to MVS in Health-Care Workers with SARS-CoV-2, Mixed effect model (OR, 95%CI)")+
  scale_x_log10(limits=c(0.1,45))

h2

h2.1<-plot_summs(m2_or, exp=T, colors = "black",legend.title = "Symptoms", 
               coefs = c("Dysnea at evaluation"="disnea",
                         "Early Onset Diabetes 
(Diabetes and Age <40 years)"="diabetes:edad40",
                         "Age ≥65 years"="edad65",
                         "≥7 Days for Clinical Assessment"="tiempo_prolong",
                         "Diabetes"="diabetes",
                         "Age <40 years"="edad40",
                         "Physicians"="factor(pers_sal_cat_3)3",
                         "Other HCWs"="factor(pers_sal_cat_3)2"))+
  xlab("Associated Factors to MVS in Health-Care Workers with SARS-CoV-2, Mixed effect model (OR, 95%CI)")+
  ylab("")+
  scale_x_log10(limits=c(0.1,45))+
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) 
h2.1

#Modelo de Cox DEFUNCION

m1<-coxph(Surv(FU_time_MORT, evolucion_casoDEFUNCION)~factor(pers_sal_cat_3)+NAC_clin+indi_rec+edad65+obesidad+diabetes*edad40+frailty(cve_entidad_unidad_medica), data=cdmx1_sal)
summary(m1)

HR<-as.data.frame(cbind(exp(coef(m1)),exp(confint(m1))))
HR
HR$Covariate<-c("Other HCWs","Physicians","Clinical pneumonia at evaluation",  "Self Reported as Indigenous", "Age ≥65 years", "Obesity", "Diabetes", "Age <40 years",
                  "Early Onset Diabetes 
(Diabetes and Age <40 years)")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
h1 <- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 1, color = "#e08f44") +
  geom_point(size = 2, color = "white") +
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("Risk for Lethality in Health-Care Workers with SARS-CoV-2, Hazard ratio (HR, 95%CI)")+scale_x_log10(limits=c(0.1,45))

figure2<-ggarrange(h4, h3, h2, h1, 
                labels = c("A", "B", "C", "D"), 
                ncol = 2, nrow = 2)
figure2
ggsave(file = "Figure2.png", 
       figure2,
       width = 55, 
       height = 20,
       units=c("cm"),
       dpi = 400,
       limitsize = FALSE)

#----Outcomes in HCWs (Figure 3)------
cdmx1_sal<-cdmx%>% filter(pers_sal==1 & covid==1)
cdmx1<- cdmx %>% filter(covid==1)
cdmx1$pers_sal_rec<-NULL;cdmx1$pers_sal_rec[cdmx1$pers_sal==1]<-0;cdmx1$pers_sal_rec[cdmx1$pers_sal==0]<-1

#Modelo de Personal de Salud HOSPITALIZACION
cdmx1<- cdmx %>% filter(resultado_definitivo=="SARS-CoV-2")

m1<-coxph(Surv(FU_time_HOSP, HOSP)~factor(pers_sal)+frailty(cve_entidad_unidad_medica), data=cdmx1)
summary(m1)

m2<-coxph(Surv(FU_time_MORT, severe_covid19)~factor(pers_sal)+frailty(cve_entidad_unidad_medica), data=cdmx1)
summary(m2)

m3<-coxph(Surv(FU_time_MORT, evolucion_casoDEFUNCION)~factor(pers_sal)+frailty(cve_entidad_unidad_medica), data=cdmx1)
summary(m3)

#Modelos de Kaplan Meier
mod1_km<-survfit(Surv(FU_time_HOSP, HOSP) ~ pers_sal_cat_3, data = cdmx1_sal)
KM_fig1<-ggsurvplot(mod1_km, data = cdmx1_sal, size = 1, palette="jama", conf.int = T,
                    risk.table = T,pval = TRUE, ggtheme = theme_classic(),xlab="Time (Days)",
                    legend.labs = c("Nurses",
                                    "Others Health-Care Workers",
                                    "Physicians"),
                    title="Hospitalization Risk in Health-Care Workers with SARS-CoV-2",
                    xlim = c(0,15),
                    ylim= c(0.3,1.0),
                    break.y.by= c(0.05),
                    break.x.by= c(5),
                    pval.coord = c(0, 0.60))+theme_survminer(base_size = 9,
                                                             base_family = "Arial")

KM_fig1 <-KM_fig1 + theme_survminer(base_size = 8,
                                    base_family = "Arial",
                                    font.x = c(8, "plain" ), 
                                    font.y = c(8, "plain"),
                                    font.main = c(10, "plain"),
                                    font.caption = c(8, "plain"), 
                                    font.legend = c(8, "plain"),
                                    font.tickslab = c(8, "plain"))

fig3D<-ggarrange(KM_fig1$plot, KM_fig1$table, heights = c(2, 0.7),
                 ncol = 1, nrow = 2)
fig3D

#Modelo de Personal de Salud COVID-GRAVE

mod2_km<-survfit(Surv(FU_time_HOSP, caso_grave) ~ pers_sal_cat_3, data = cdmx1_sal)
KM_fig2<-ggsurvplot(mod2_km, data = cdmx1_sal, size = 1,palette = "jama",conf.int = T,
                    risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                    legend.labs = c("Nurses",
                                    "Others Health-Care Workers",
                                    "Physicians"),
                    title="Severe Outcome in Health-Care Workers with SARS-CoV-2",
                    xlim = c(0,15),
                    ylim= c(0.5,1.0),
                    break.y.by= c(0.05),
                    break.x.by= c(5),
                    pval.coord = c(0, 0.70))+theme_survminer(base_size = 9,
                                                             base_family = "Arial")

KM_fig2 <-KM_fig2 + theme_survminer(base_size = 8,
                                    base_family = "Arial",
                                    font.x = c(8, "plain" ), 
                                    font.y = c(8, "plain"),
                                    font.main = c(10, "plain"),
                                    font.caption = c(8, "plain"), 
                                    font.legend = c(8, "plain"),
                                    font.tickslab = c(8, "plain"))

fig3E<-ggarrange(KM_fig2$plot, KM_fig2$table, heights = c(2, 0.7),
                 ncol = 1, nrow = 2);fig3E


#Modelo de Personal de Salud MORTALIDAD

mod3_km<-survfit(Surv(FU_time_MORT, evolucion_casoDEFUNCION) ~ pers_sal_cat_3, data = cdmx1_sal)
mod3_km
KM_fig3<-ggsurvplot(mod3_km, data = cdmx1_sal, size = 1,palette = "jama",conf.int = T,
                 risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                 title="Lethality Risk in Health-Care Workers with SARS-CoV-2",
                 legend.labs = c("Nurses",
                                 "Others Health-Care Workers",
                                 "Physicians"),
                 xlim = c(0,30),
                 ylim= c(0,1.0),
                 break.y.by= c(0.05),
                 break.x.by= c(5),
                 pval.coord = c(0, 0.60))+theme_survminer(base_size = 9,
                                                          base_family = "Arial")



KM_fig3 <-KM_fig3 + theme_survminer(base_size = 8,
                              base_family = "Arial",
                              font.x = c(8, "plain" ), 
                              font.y = c(8, "plain"),
                              font.main = c(10, "plain"),
                              font.caption = c(8, "plain"), 
                              font.legend = c(8, "plain"),
                              font.tickslab = c(8, "plain"))

fig3F<-ggarrange(KM_fig3$plot, KM_fig3$table, heights = c(2, 0.7),
                 ncol = 1, nrow = 2)
fig3F

#Personal de Salud vs NO PERSONAL DE SALUD

#Modelo de Personal de Salud HOSPITALIZACION

table(cdmx1$pers_sal_rec)
mod1_km<-survfit(Surv(FU_time_HOSP, HOSP) ~ pers_sal_rec, data = cdmx1)

KM_fig1<-ggsurvplot(mod1_km, data = cdmx1, size = 1,palette = "jama",conf.int = T,
                    risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                    legend.labs = c("Health-Care Workers","Other professions"),
                    title="Hospitalization Risk in Mexico City Workers with SARS-CoV-2",
                    xlim = c(0,15),
                    ylim= c(0.3,1.0),
                    break.y.by= c(0.05),
                    break.x.by= c(5),
                    pval.coord = c(0, 0.60))+theme_survminer(base_size = 9,
                                                             base_family = "Arial")

KM_fig1 <-KM_fig1 + theme_survminer(base_size = 8,
                                    base_family = "Arial",
                                    font.x = c(8, "plain" ), 
                                    font.y = c(8, "plain"),
                                    font.main = c(10, "plain"),
                                    font.caption = c(8, "plain"), 
                                    font.legend = c(8, "plain"),
                                    font.tickslab = c(8, "plain"))


fig3A<-ggarrange(KM_fig1$plot, KM_fig1$table, heights = c(2, 0.7),
                 ncol = 1, nrow = 2)
fig3A

#Modelo de Personal de Salud COVID-GRAVE

mod2_km<-survfit(Surv(FU_time_HOSP, caso_grave) ~ pers_sal_rec, data = cdmx1)
KM_fig2<-ggsurvplot(mod2_km, data = cdmx1, size = 1,palette = "jama",conf.int = T,
                    risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                    legend.labs = c("Health-Care Workers","Other professions"),
                    title="Severe Outcome Risk in Mexico City Workers with SARS-CoV-2",
                    xlim = c(0,15),
                    ylim= c(0.5,1.0),
                    break.y.by= c(0.05),
                    break.x.by= c(5),
                    pval.coord = c(0, 0.70))+theme_survminer(base_size = 9,
                                                             base_family = "Arial")

KM_fig2 <-KM_fig2 + theme_survminer(base_size = 8,
                                    base_family = "Arial",
                                    font.x = c(8, "plain" ), 
                                    font.y = c(8, "plain"),
                                    font.main = c(10, "plain"),
                                    font.caption = c(8, "plain"), 
                                    font.legend = c(8, "plain"),
                                    font.tickslab = c(8, "plain"))

fig3B<-ggarrange(KM_fig2$plot, KM_fig2$table, heights = c(2, 0.7),
                 ncol = 1, nrow = 2)
fig3B

#Modelo de Personal de Salud MORTALIDAD

mod3_km<-survfit(Surv(FU_time_MORT, evolucion_casoDEFUNCION) ~ pers_sal_rec, data = cdmx1)
KM_fig3<-ggsurvplot(mod3_km, data = cdmx1, size = 1,palette = "jama",conf.int = T,
                    risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                    title="Lethality Risk in Mexico City Workers with SARS-CoV-2",
                    legend.labs = c("Health-Care Workers","Other professions"),
                    xlim = c(0,30),
                    ylim= c(0,1.0),
                    break.y.by= c(0.05),
                    break.x.by= c(5),
                    pval.coord = c(0, 0.60))+theme_survminer(base_size = 9,
                                                             base_family = "Arial")

KM_fig3 <-KM_fig3 + theme_survminer(base_size = 8,
                                    base_family = "Arial",
                                    font.x = c(8, "plain" ), 
                                    font.y = c(8, "plain"),
                                    font.main = c(10, "plain"),
                                    font.caption = c(8, "plain"), 
                                    font.legend = c(8, "plain"),
                                    font.tickslab = c(8, "plain"))

fig3C<-ggarrange(KM_fig3$plot, KM_fig3$table, heights = c(2, 0.7),
                 ncol = 1, nrow = 2)

FIGURE3<-ggarrange(fig3A,fig3B,fig3C,fig3D,fig3E,fig3F,
                   labels = c("A", "B", "C", "D", "E", "F"),
                   ncol=3,nrow =2)

ggsave(file = "Figure3.png", 
       FIGURE3,
       width = 55, 
       height = 35,
       units=c("cm"),
       dpi = 400,
       limitsize = FALSE)


#----Positivity Associated Factors (Suplementary Figure 1)-----
#Modelo de Personal de Salud UNICAMENTE con positividad
cdmx$pers_sal_cat_3
cdmx1_sal<-cdmx%>% filter(pers_sal==1) 
#Modelo de Sintomas#

cdmx1_pos_1<-cdmx1_sal%>%dplyr::select(covid,fiebre,tos,ataque_al_estado_general,calofrios,mialgias,artralgias,rinorrea,
              dolor_toracico,polipnea,tos,diarrea,inicio_subito_sintomas,irritabilidad,
              cve_municipio_residencia,contacto_viral,asintomaticos)%>%drop_na()

cdmx1_pos_1$asinto_viral<-NULL; cdmx1_pos_1$asinto_viral[cdmx1_pos_1$asintomaticos==1 & cdmx1_pos_1$contacto_viral==1]<-1
cdmx1_pos_1$asinto_viral<-na.tools::na.replace(cdmx1_pos_1$asinto_viral,0)

#Capacidad sintomas
m_pos_sin<-glm(covid~factor(pers_sal_cat_3)+fiebre+tos+ataque_al_estado_general+calofrios+mialgias+artralgias+rinorrea+dolor_toracico+polipnea+tos+
                 diarrea+inicio_subito_sintomas+irritabilidad,family = "binomial", data=cdmx)
summary(m_pos_sin)
exp(confint(m_pos_sin))
#Capacidad tracing
m_pos_sin<-glm(covid~asinto_viral,family = "binomial", data=cdmx1_pos_1)
summary(m_pos_sin)
cdmx1_pos_1$pred<-predict(m_pos_sin,newdata=cdmx1_pos_1)
roc(cdmx1_pos_1$covid, cdmx1_pos_1$pred)
plot.roc(roc(cdmx1_pos_1$covid, cdmx1_pos_1$pred))
reportROC(cdmx1_pos_1$covid,(as.numeric(cdmx1_pos_1$pred)),important="se",plot=F)

#Modelo de Sintomas
m_pos_sin<-glm(covid~fiebre+tos+ataque_al_estado_general+calofrios+mialgias+artralgias+rinorrea+dolor_toracico+polipnea+tos+
                 diarrea+inicio_subito_sintomas+irritabilidad,
               family = "binomial", data=cdmx1_sal)

H_pos<-plot_summs(m_pos_sin, exp=T, colors = "black",legend.title = "Symptoms",coefs = c("Fever"="fiebre",
                                                                                         "Cough"="tos",
                                                                                         "Malaise"="ataque_al_estado_general",
                                                                                         "Myalgias"="mialgias",
                                                                                         "Shivering"="calofrios",
                                                                                         "Rhinorrhea"="rinorrea",
                                                                                         "Arthralgia"="artralgias",
                                                                                         "Chest pain"="dolor_toracico",
                                                                                         "Polypnea"="polipnea",
                                                                                         "Sudden Onset of Symptoms"="inicio_subito_sintomas",
                                                                                         "Irritability"="irritabilidad",
                                                                                         "Diarrhea"="diarrea"))+
  xlab("Associated Symptoms to Confirmed SARS-CoV2 Test in Health-Care Workers, Mixed effect model (OR, 95%CI)")+
  ylab("")+
  scale_x_log10(limits=c(0.3,2.5))+
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank())

H_pos
#Modelo de Comorbilidades

H_com<-glm(covid~esta_emabarazada+sexo+obesidad+diabetes+tabaquismo+puerperio+tiempo_prolong+
              viaje_sum+contacto_viral,family = "binomial", data=cdmx1_pos)
summary(H_com)

H_com<-plot_summs(H_com, exp=T, colors = "black",legend.title = "Symptoms",coefs = c("Pregnancy"="esta_emabarazada",
                                                                                    "Male Sex"="sexoMASCULINO",
                                                                                    "≥7 Days for Clinical Assessment"="tiempo_prolong",
                                                                                    "Obesity"="obesidad",
                                                                                    "Diabetes"="diabetes",
                                                                                    "Active Smoking"="tabaquismo",
                                                                                    "Puerperium"="puerperio",
                                                                                    "Contact with suspected case"="contacto_viral",
                                                                                    "Recent Traveling Out of Mexico"="viaje_sum"))+
  xlab("Associated Conditions to Confirmed SARS-CoV2 Test in Health-Care Workers, Mixed effect model (OR, 95%CI)")+
  ylab("")+
  scale_x_log10(limits=c(0.3,2.5))+
  theme_hc() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank())
H_com

suplementary_figure_x<-plot_grid(H_pos,H_com,
                                 ncol=1, labels=LETTERS[1:2])

ggsave(file = "Suplementary_Figure_2.png", 
       H_com,
       width = 30, 
       height = 15,
       units=c("cm"),
       dpi = 150,
       limitsize = FALSE)


#----Pneumonia in HCWs (Suplementary Figure 2)-----

mod1_km<-survfit(Surv(FU_time_MORT, evolucion_casoDEFUNCION) ~ NAC_clin+pers_sal_rec, data = cdmx1)
SUP_KM2<-ggsurvplot(mod1_km, data = cdmx1, size = 1, palette = "jama",conf.int = T,
                 risk.table = T,pval = TRUE,ggtheme = theme_hc(),xlab="Time (Days)",
                 legend.labs = c("No-Pneumonia & Health-Care Workers", 
                                 "No-Pneumonia & Non-Health-Care Workers",
                                 "Pneumonia & Health-Care Workers",
                                 "Pneumonia & Non-Health-Care Workers"),
                 xlim = c(0,30),
                 ylim= c(0,1.0),
                 break.y.by= c(0.05),
                 break.x.by= c(5),
                 pval.coord = c(0, 0.25))+theme_survminer(base_size = 9,
                                                          base_family = "Arial")+guides(colour = guide_legend(nrow = 2))

SUP_KM2 <-SUP_KM2 + theme_survminer(base_size = 8,
                              base_family = "Arial",
                              font.x = c(8, "plain" ), 
                              font.y = c(8, "plain"),
                              font.caption = c(8, "plain"), 
                              font.tickslab = c(8, "plain"))

SUP_KM2<-ggarrange(SUP_KM2$plot, SUP_KM2$table, heights = c(2, 0.7),
                 ncol = 1, nrow = 2)

ggsave(file = "Suplementary_Figure_3.png", 
       SUP_KM2,
       width = 30, 
       height = 20,
       units=c("cm"),
       dpi = 400,
       limitsize = FALSE)

#----Symptoms Analysis (Suplementary Table 2)-----

set.seed(123)
cdmx1_pos<- cdmx%>% filter(pers_sal==1)
table(cdmx1_pos$covid)

reportROC(cdmx1_pos$covid,cdmx1_pos$fiebre,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$tos,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$odinofagia,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$disnea,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$irritabilidad,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$diarrea,important="se",plot=F)

reportROC(cdmx1_pos$covid,cdmx1_pos$dolor_toracico,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$calofrios,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$cefalea,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$mialgias,important="se",plot=F)

reportROC(cdmx1_pos$covid,cdmx1_pos$artralgias,important="se",plot=F)

reportROC(cdmx1_pos$covid,cdmx1_pos$ataque_al_estado_general,important="se",plot=F)

reportROC(cdmx1_pos$covid,cdmx1_pos$rinorrea,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$polipnea,important="se",plot=F)

reportROC(cdmx1_pos$covid,cdmx1_pos$vomito,important="se",plot=F)

reportROC(cdmx1_pos$covid,cdmx1_pos$dolor_abdominal,important="se",plot=F)

reportROC(cdmx1_pos$covid,cdmx1_pos$conjuntivitis,important="se",plot=F)

reportROC(cdmx1_pos$covid,cdmx1_pos$cianosis,important="se",plot=F)

reportROC(cdmx1_pos$covid,cdmx1_pos$inicio_subito_sintomas,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$tres_sin_sen,important="se",plot=F)
reportROC(cdmx1_pos$covid,cdmx1_pos$tres_sin_esp,important="se",plot=F)

tres_sin_sen<-NULL;tres_sin_sen[cdmx1_pos$tos==1 & cdmx1_pos$cefalea==1]<-1;tres_sin_sen<-na.tools::na.replace(tres_sin_sen,0)
tres_sin_esp<-NULL;tres_sin_esp[cdmx1_pos$disnea==1 & cdmx1_pos$polipnea==1]<-1;tres_sin_esp<-na.tools::na.replace(tres_sin_esp,0)
cdmx1_pos<-cbind(cdmx1_pos,tres_sin_sen,tres_sin_esp)
#----Descriptive Analysis (Tabla 1)#####
#1 Nurses
#2 Other HCWs
#3 Physicians

cdmx1<- cdmx 
nrow(cdmx)
cdmx_sal<-cdmx1%>% filter(pers_sal==1 & covid==1)
table(cdmx_sal$comorb==0)

cdmx1_sal_cat<-cdmx1%>% filter(pers_sal==1 & pers_sal_cat_3==2)

table(cdmx1_sal_cat$covid)
prop.table(table(cdmx1_sal_cat$covid))*100
chisq.test(table(cdmx_sal$covid, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$asintomaticos)
prop.table(table(cdmx1_sal_cat$asintomaticos))*100
chisq.test(table(cdmx_sal$asintomaticos, cdmx_sal$pers_sal_cat_3))

nrow(cdmx1_sal_cat)

table(cdmx1_sal_cat$sexo)
prop.table(table(cdmx1_sal_cat$sexo))*100
chisq.test(table(cdmx_sal$sexo, cdmx_sal$pers_sal_cat_3))

psych::describe(cdmx1_sal_cat$edad)
wilcox.test(cdmx_sal$edad,cdmx_sal$pers_sal_cat_3)

table(cdmx1_sal_cat$ocupacionMEDICOS)
prop.table(table(cdmx1_sal_cat$ocupacionMEDICOS))*100
chisq.test(table(cdmx_sal$ocupacionMEDICOS, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$ocupacionENFERMERAS)
prop.table(table(cdmx1_sal_cat$ocupacionENFERMERAS))*100
chisq.test(table(cdmx_sal$ocupacionENFERMERAS, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$ocupacionDENTISTAS)
prop.table(table(cdmx1_sal_cat$ocupacionDENTISTAS))*100
chisq.test(table(cdmx_sal$ocupacionDENTISTAS, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$ocupacionLABORATORISTAS)
prop.table(table(cdmx1_sal_cat$ocupacionLABORATORISTAS))*100
chisq.test(table(cdmx_sal$ocupacionLABORATORISTAS, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$`ocupacionOTROS TRABAJADORES DE LA SALUD`)
prop.table(table(cdmx1_sal_cat$`ocupacionOTROS TRABAJADORES DE LA SALUD`))
chisq.test(table(cdmx_sal$`ocupacionOTROS TRABAJADORES DE LA SALUD`, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$HOSP)
prop.table(table(cdmx1_sal_cat$HOSP))*100
chisq.test(table(cdmx_sal$HOSP, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$severe_covid19)
prop.table(table(cdmx1_sal_cat$severe_covid19))*100
chisq.test(table(cdmx_sal$severe_covid19, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$intubado)
prop.table(table(cdmx1_sal_cat$intubado))*100
chisq.test(table(cdmx_sal$intubado, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$evolucion_casoDEFUNCION)
prop.table(table(cdmx1_sal_cat$evolucion_casoDEFUNCION))*100
chisq.test(table(cdmx_sal$evolucion_casoDEFUNCION, cdmx_sal$pers_sal_cat_3))



table(cdmx1_sal_cat$tiempo_prolong)
prop.table(table(cdmx1_sal_cat$tiempo_prolong))*100
chisq.test(table(cdmx_sal$tiempo_prolong, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$comorb==0)
prop.table(table(cdmx1_sal_cat$comorb==0))*100
chisq.test(table(cdmx_sal$comorb==0, cdmx_sal$pers_sal_cat_3))


table(cdmx1_sal_cat$diabetes)
prop.table(table(cdmx1_sal_cat$diabetes))*100
chisq.test(table(cdmx_sal$diabetes, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$epoc)
prop.table(table(cdmx1_sal_cat$epoc))*100
chisq.test(table(cdmx_sal$epoc, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$asma)
prop.table(table(cdmx1_sal_cat$asma))*100
chisq.test(table(cdmx_sal$asma, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$VIH_SIDA)
prop.table(table(cdmx1_sal_cat$VIH_SIDA))*100
chisq.test(table(cdmx_sal$VIH_SIDA, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$obesidad)
prop.table(table(cdmx1_sal_cat$obesidad))*100
chisq.test(table(cdmx_sal$obesidad, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$tabaquismo)
prop.table(table(cdmx1_sal_cat$tabaquismo))*100
chisq.test(table(cdmx_sal$tabaquismo, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$insuficiencia_renal_cronica)
prop.table(table(cdmx1_sal_cat$insuficiencia_renal_cronica))*100
chisq.test(table(cdmx_sal$insuficiencia_renal_cronica, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$esta_emabarazada)
prop.table(table(cdmx1_sal_cat$esta_emabarazada))*100
chisq.test(table(cdmx_sal$esta_emabarazada, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$indi_rec)
prop.table(table(cdmx1_sal_cat$indi_rec))*100
chisq.test(table(cdmx_sal$indi_rec, cdmx_sal$pers_sal_cat_3))

#END#