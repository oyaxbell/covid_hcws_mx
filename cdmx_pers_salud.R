#### Database management ####
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


setwd("C:/Users/HP-PC/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/COVID-CDMX")
setwd("/Users/nefoantonio/UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/OMAR YAXMEHEN BELLO CHAVOLLA - COVID-CDMX/Personal de Salud")

cdmx<-read.csv("base-covid-sinave.csv")
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
d1<-dummy(cdmx$evolucion_caso)
cdmx<-cbind(cdmx, d1)
d2<-dummy(cdmx$ocupacion)
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
pers_sal_cat_3<-NULL;pers_sal_cat_3[pers_sal_cat==1]<-1;pers_sal_cat_3[pers_sal_cat==2]<-2;pers_sal_cat_3[pers_sal_cat>=3]<-3
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
cdmx<-cbind(cdmx,NAC_clin,edad65,edad40,DM2_edad40,pers_sal,indi_rec,HOSP,pers_sal_cat,pers_sal_cat_2,pers_sal_cat_3,tiempo_prolong,caso_grave)


#### Análisis personal de salud ####
#Personal de Salud con COVID positivo
cdmx1<- cdmx %>% filter(resultado_definitivo=="SARS-CoV-2")
cdmx1_sal<-cdmx1%>% filter(pers_sal==1)
cdmx1_pos<- cdmx%>% filter(pers_sal==1)

#----Frecuencia de positividad-----

cdmx1_pos$covid<-factor(cdmx1_pos$covid,
                        levels = c(0,1),
                        labels = c("Negative Case","Positive Case"))
regitro_date<-as.Date(cdmx1_pos$fecha_de_registro, format="%Y-%m-%d")
Sup_figure1<-ggplot(cdmx1_pos, aes(x=regitro_date,colour=factor(covid)))+
  geom_density(binwidth=1)+
  theme_classic()+
  labs(colour="COVID-19 Status")+
  ylab("Frecuency") + xlab("Date of Registry") +
  scale_x_date(date_breaks = "2 week",
               date_minor_breaks = "1 day",
               limits = c(as.Date("2020-03-15"), 
                          as.Date("2020-06-30")))
ggsave(file = "Suplementary_Figure_1.jpg", 
       Sup_figure1,
       width = 30, 
       height = 15,
       units=c("cm"),
       dpi = 400,
       limitsize = FALSE)

#----Modelo de positividad por COVID-----
#Modelo de Personal de Salud UNICAMENTE con positividad

m_pos_sin<-glm(covid~pers_sal+edad+sexo+diabetes+hipertension+fiebre+tos+ataque_al_estado_general+calofrios+mialgias+artralgias+rinorrea+dolor_toracico+polipnea+tos+diarrea+inicio_subito_sintomas+irritabilidad+cefalea,family = "binomial", data=cdmx)
summary(m_pos_sin)
exp(m_pos_sin$coefficients)
exp(confint(m_pos_sin))

#Modelo de Sintomas#

m_pos_sin<-glmer(covid~fiebre+tos+ataque_al_estado_general+calofrios+mialgias+artralgias+rinorrea+dolor_toracico+polipnea+tos+diarrea+inicio_subito_sintomas+irritabilidad+cefalea+(1|cve_entidad_unidad_medica),
               family = "binomial", data=cdmx1_pos)

summary(m_pos_sin)
H_pos<-plot_summs(m_pos_sin, exp=T, legend.title = "Symptoms",coefs = c("Fever"="fiebre",
                                                                    "Cough"="tos",
                                                                    "Malaise"="ataque_al_estado_general",
                                                                    "Shivering"="calofrios",
                                                                    "Myalgias"="mialgias",
                                                                    "Arthralgia"="artralgias",
                                                                    "Rhinorrhea"="rinorrea",
                                                                    "Chest pain"="dolor_toracico",
                                                                    "Polypnea"="polipnea",
                                                                    "Diarrhea"="diarrea",
                                                                    "Sudden Onset of Symptoms"="inicio_subito_sintomas",
                                                                    "Irritability"="irritabilidad",
                                                                    "Headache"="cefalea"))+
  xlab("Associated symptoms to confirmed COVID-19 in Health-Care Workers, Mixed effect model Odds Ratio (OR, 95%CI)")+
  ylab("")+
  scale_x_log10(limits=c(0.6,2.5))+
  theme_classic() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank())

H_pos
#Modelo de Comorbilidades
m_fx<-glmer(covid~esta_emabarazada+sexo+obesidad+diabetes+tabaquismo+puerperio+tiempo_prolong+(1|cve_entidad_unidad_medica),family = "binomial", data=cdmx1_pos)
summary(m_fx)
H_com<-plot_summs(m_fx, exp=T, legend.title = "Symptoms",coefs = c("Pregnancy"="esta_emabarazada",
                                                                   "Male Sex"="sexoMASCULINO",
                                                                   "≥7 Days for Clinical Assessment"="tiempo_prolong",
                                                                   "Obesity"="obesidad",
                                                                   "Diabetes"="diabetes",
                                                                   "Active Smoking"="tabaquismo",
                                                                   "Puerperium"="puerperio"))+
  xlab("Associated conditions to confirmed COVID-19 in Health-Care Workers, Mixed effect model Odds Ratio (OR, 95%CI)")+
  ylab("")+
  scale_x_log10(limits=c(0.6,2.5))+
  theme_classic() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank())
fig1<-ggarrange(H_pos,H_com,
                labels = c("A", "B"), 
                ncol = 2, nrow = 1); fig1

ggsave(file = "Figure1.png", 
       fig1,
       width = 50, 
       height = 12,
       units=c("cm"),
       dpi = 400,
       limitsize = FALSE)

#----Modelo de Cox DEFUNCION-----
cdmx1_sal$evolucion_casoDEFUNCION

m1<-coxph(Surv(FU_time_MORT, evolucion_casoDEFUNCION)~NAC_clin+edad65+obesidad+diabetes, data=cdmx1_sal)
cox.zph(m1); summary(m1)

HR<-as.data.frame(cbind(exp(coef(m1)),exp(confint(m1))))
HR$Covariate<-c("Clinical pneumonia at evaluation", "Age ≥65 years", "Obesity", "Diabetes")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
h1 <- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 0.5, color = "deepskyblue2") +
  geom_point(size = 2, color = "white") +
  theme_classic() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("Risk for Lethality in Health-Care Workers with SARS-CoV-2, Hazard ratio (HR, 95%CI)")+scale_x_log10(limits=c(0.4,40))
h1

#----Modelo de Cox INTUBACION-----

m2_or<-glmer(intubado~edad65+diabetes+disnea++(1|cve_entidad_unidad_medica),family="binomial", data=cdmx1_sal)
summary(m2_or)
h2<-plot_summs(m2_or, exp=T, legend.title = "Symptoms", 
           coefs = c("Age ≥65 years"="edad65",
                     "Diabetes"="diabetes",
                     "Dysnea at evaluation"="disnea",
                     "≥7 Days for Clinical Assessment"="tiempo_prolong"))+
  xlab("Associated Factors to MVS in Health-Care Workers with SARS-CoV-2, Mixed effect model Odds Ratio  (OR, 95%CI)")+
  ylab("")+
  scale_x_log10(limits=c(0.4,40))+
  theme_classic() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank())
h2
#----Modelo de Cox COVID GRAVE-----
cdmx1_sal$caso_grave
m4<-coxph(Surv(FU_time_HOSP, caso_grave)~edad65+disnea+polipnea+fiebre, data=cdmx1_sal)
cox.zph(m4); summary(m4)
HR<-as.data.frame(cbind(exp(coef(m4)),exp(confint(m4))))
HR
HR$Covariate<-c("Age ≥65 years", "Dysnea at evaluation", 
                "Polypnea at evaluation", "Fever at evaluation")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
h3 <- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 0.5, color = "deepskyblue2") +
  geom_point(size = 2, color = "white") +
  theme_classic() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("Risk for Severe Outcome in Health-Care Workers with SARS-CoV-2, Hazard ratio (HR, 95%CI)")+scale_x_log10(limits=c(0.4,40))
h3
#----Modelo de Cox de Hospitalización----

m5<-coxph(Surv(FU_time_HOSP, HOSP)~edad65+obesidad+diabetes+VIH_SIDA+hipertension+polipnea+disnea+fiebre+
            diarrea+conjuntivitis+odinofagia, data=cdmx1_sal)
cox.zph(m5); summary(m5)

HR<-as.data.frame(cbind(exp(coef(m5)),exp(confint(m5))))
HR
HR$Covariate<-c("Age ≥65 years", "Obesity", "Diabetes",
                "HIV/AIDS", "Arterial hypertension", "Polypnea at evaluation", 
                "Dysnea at evaluation",
                "Fever at evaluation", 
                "Diarrhea at evaluation",
                "Conjunctivitis at evaluation", 
                "Odynophagia at evaluation")
colnames(HR)<-c("HR", "ciLow", "ciHigh", "Covariate")
HR$breaks<-seq(1:nrow(HR))
h4 <- ggplot(HR, aes(x = HR, y = breaks)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_pointrange(aes(xmax = ciHigh, xmin = ciLow), size = 0.5, color = "deepskyblue2") +
  geom_point(size = 2, color = "white") +
  theme_classic() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank()) +
  scale_y_continuous(breaks = HR$breaks, labels = HR$Covariate) +
  ylab("") +
  xlab("Risk for Hospitalization in Health-Care Workers with SARS-CoV-2, Hazard ratio (HR, 95%CI)")+scale_x_log10(limits=c(0.4,40))
h4
fig2<-ggarrange(h4, h3, h2, h1, 
                labels = c("A", "B", "C", "D"), 
                ncol = 2, nrow = 2)
ggsave(file = "Figure2.png", 
       fig2,
       width = 50, 
       height = 18,
       units=c("cm"),
       dpi = 400,
       limitsize = FALSE)

#----Outcomes por personal de Salud------

cdmx1_sal<-cdmx%>% filter(pers_sal==1 & covid==1)

#Modelo de Personal de Salud HOSPITALIZACION
mod1<-coxph(Surv(FU_time_HOSP, HOSP)~pers_sal_cat_2+edad+sexo+diabetes+hipertension+asma+obesidad+tabaquismo, data=cdmx1_sal)
summary(mod1)
exp(mod1$coefficients)
exp(confint(mod1))

mod1_km<-survfit(Surv(FU_time_HOSP, HOSP) ~ pers_sal_cat_3, data = cdmx1_sal)
KM_fig1<-ggsurvplot(mod1_km, data = cdmx1_sal, size = 1,palette = "bw",conf.int = T,
                    risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                    legend.labs = c("Physicians",
                                    "Nurses",
                                    "Others Health-Care Workers"),
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
#Modelo de Personal de Salud COVID-GRAVE


mod2<-coxph(Surv(FU_time_HOSP, caso_grave)~pers_sal_cat_2+edad+sexo+diabetes+hipertension+asma+obesidad+tabaquismo, data=cdmx1_sal)
summary(mod2)
exp(mod2$coefficients)
exp(confint(mod2))


mod2_km<-survfit(Surv(FU_time_HOSP, caso_grave) ~ pers_sal_cat_3, data = cdmx1_sal)
KM_fig2<-ggsurvplot(mod2_km, data = cdmx1_sal, size = 1,palette = "bw",conf.int = T,
                    risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                    legend.labs = c("Physicians",
                                    "Nurses",
                                    "Others Health-Care Workers"),
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
                 ncol = 1, nrow = 2)

#Modelo de Personal de Salud MORTALIDAD
cdmx1_sal$evolucion_casoDEFUNCION

mod3<-coxph(Surv(FU_time_MORT, evolucion_casoDEFUNCION)~indi_rec+edad+sexo+diabetes+hipertension+asma+obesidad+tabaquismo, data=cdmx1_sal)
summary(mod3)
exp(mod3$coefficients)
exp(confint(mod3))

table(cdmx1_sal$pers_sal_cat_3)

mod3_km<-survfit(Surv(FU_time_MORT, evolucion_casoDEFUNCION) ~ pers_sal_cat_3, data = cdmx1_sal)
mod3_km
KM_fig3<-ggsurvplot(mod3_km, data = cdmx1_sal, size = 1,palette = "bw",conf.int = T,
                 risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                 title="Lethality Risk in Health-Care Workers with SARS-CoV-2",
                 legend.labs = c("Physicians",
                                 "Nurses",
                                 "Others Health-Care Workers"),
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
#----Personal de Salud vs NO PERSONAL DE SALUD -------

cdmx1<- cdmx %>% filter(covid==1)

#Modelo de Personal de Salud HOSPITALIZACION

mod1<-coxph(Surv(FU_time_HOSP, HOSP)~pers_sal+edad+sexo, data=cdmx1)
summary(mod1)
exp(mod1$coefficients)
exp(confint(mod1))

mod1_km<-survfit(Surv(FU_time_HOSP, HOSP) ~ pers_sal, data = cdmx1)
mod1_km
KM_fig1<-ggsurvplot(mod1_km, data = cdmx1, size = 1,palette = "bw",conf.int = T,
                    risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                    legend.labs = c("Other professions",
                                    "Health-Care Workers"),
                    title="Hospitalization Risk in Mexico City Workers with SARS-CoV-2",
                    xlim = c(0,15),
                    ylim= c(0,1.0),
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


mod2<-coxph(Surv(FU_time_HOSP, caso_grave)~pers_sal+edad+sexo, data=cdmx1)
summary(mod2)
exp(mod2$coefficients)
exp(confint(mod2))


mod2_km<-survfit(Surv(FU_time_HOSP, caso_grave) ~ pers_sal, data = cdmx1)
KM_fig2<-ggsurvplot(mod2_km, data = cdmx1, size = 1,palette = "bw",conf.int = T,
                    risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                    legend.labs = c("Other professions",
                                    "Health-Care Workers"),
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

mod3<-coxph(Surv(FU_time_MORT, evolucion_casoDEFUNCION)~pers_sal+edad+sexo, data=cdmx1)
summary(mod3)
exp(mod3$coefficients)
exp(confint(mod3))
cdmx1$evolucion_casoDEFUNCION
mod3_km<-survfit(Surv(FU_time_MORT, evolucion_casoDEFUNCION) ~ pers_sal, data = cdmx1)
KM_fig3<-ggsurvplot(mod3_km, data = cdmx1, size = 1,palette = "bw",conf.int = T,
                    risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                    title="Lethality Risk in Mexico City Workers with SARS-CoV-2",
                    legend.labs = c("Other professions",
                                    "Health-Care Workers"),
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
ggsave(file = "Figure3_SALUD_2.2.png", 
       FIGURE3,
       width = 55, 
       height = 40,
       units=c("cm"),
       dpi = 400,
       limitsize = FALSE)


#Modelo de Neumonia
mod4<-coxph(Surv(FU_time_MORT, evolucion_casoDEFUNCION)~pers_sal_cat_2, data=cdmx1_sal)
summary(mod4)
exp(mod4$coefficients)
exp(confint(mod4))

mod1_km<-survfit(Surv(FU_time_MORT, evolucion_casoDEFUNCION) ~ NAC_clin+pers_sal, data = cdmx1)
SUP_KM2<-ggsurvplot(mod1_km, data = cdmx1, size = 1,palette = "bw",conf.int = T,
                 risk.table = T,pval = TRUE,ggtheme = theme_classic(),xlab="Time (Days)",
                 legend.labs = c("No-Pneumonia & Non-Health-Care Workers", 
                                 "No-Pneumonia & Health-Care Workers",
                                 "Pneumonia & Non-Health-Care Workers",
                                 "Pneumonia & Health-Care Workers"),
                 xlim = c(0,30),
                 ylim= c(0,1.0),
                 break.y.by= c(0.05),
                 break.x.by= c(5),
                 pval.coord = c(0, 0.25))+theme_survminer(base_size = 9,
                                                          base_family = "Arial")

SUP_KM2 <-SUP_KM2 + theme_survminer(base_size = 8,
                              base_family = "Arial",
                              font.x = c(8, "plain" ), 
                              font.y = c(8, "plain"),
                              font.caption = c(8, "plain"), 
                              font.tickslab = c(8, "plain"))


SUP_KM2<-ggarrange(SUP_KM2$plot, SUP_KM2$table, heights = c(2, 0.7),
                 ncol = 1, nrow = 2)

ggsave(file = "Sup_Figure2.2_SALUD.jpg", 
       SUP_KM2,
       width = 30, 
       height = 20,
       units=c("cm"),
       dpi = 400,
       limitsize = FALSE)

#----Tabla de sintomatologia de COVID 19-----

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

#----Analisis Descriptivo----

#Muestra total
nrow(cdmx1_pos)
table(cdmx1_pos$pers_sal_cat)
prop.table(table(cdmx1_pos$pers_sal_cat))

#Muestra con casos positivo

cdmx1_sal<-cdmx%>% filter(pers_sal==1 & covid==1)
cdmx_sal<-cdmx%>% filter(pers_sal==1)

table(cdmx1_sal$asintomaticos)
prop.table(table(cdmx1_sal$asintomaticos))

nrow(cdmx1_sal)
table(cdmx1_sal$sexo)
prop.table(table(cdmx1_sal$sexo))
chisq.test(table(cdmx_sal$sexo, cdmx_sal$covid))


table(cdmx1_sal$tiempo_prolong)
prop.table(table(cdmx1_sal$tiempo_prolong))
chisq.test(table(cdmx_sal$tiempo_prolong, cdmx_sal$covid))


psych::describe(cdmx1_sal$edad)
wilcox.test(cdmx_sal$edad,cdmx_sal$covid)

summary(cdmx1_sal$FU_time_HOSP)
wilcox.test(cdmx_sal$FU_time_HOSP,cdmx_sal$covid)


table(cdmx1_sal$ocupacionMEDICOS)
prop.table(table(cdmx1_sal$ocupacionMEDICOS))
chisq.test(table(cdmx_sal$ocupacionMEDICOS, cdmx_sal$covid))

table(cdmx1_sal$ocupacionENFERMERAS)
prop.table(table(cdmx1_sal$ocupacionENFERMERAS))
chisq.test(table(cdmx_sal$ocupacionENFERMERAS, cdmx_sal$covid))

table(cdmx1_sal$ocupacionDENTISTAS)
prop.table(table(cdmx1_sal$ocupacionDENTISTAS))
chisq.test(table(cdmx_sal$ocupacionDENTISTAS, cdmx_sal$covid))

table(cdmx1_sal$ocupacionLABORATORISTAS)
prop.table(table(cdmx1_sal$ocupacionLABORATORISTAS))
chisq.test(table(cdmx_sal$ocupacionLABORATORISTAS, cdmx_sal$covid))

table(cdmx1_sal$`ocupacionOTROS TRABAJADORES DE LA SALUD`)
prop.table(table(cdmx1_sal$`ocupacionOTROS TRABAJADORES DE LA SALUD`))
chisq.test(table(cdmx_sal$`ocupacionOTROS TRABAJADORES DE LA SALUD`, cdmx_sal$covid))

table(cdmx1_sal$evolucion_casoDEFUNCION)
prop.table(table(cdmx1_sal$evolucion_casoDEFUNCION))
chisq.test(table(cdmx_sal$evolucion_casoDEFUNCION, cdmx_sal$covid))

table(cdmx1_sal$HOSP)
prop.table(table(cdmx1_sal$HOSP))
chisq.test(table(cdmx_sal$HOSP, cdmx_sal$covid))

table(cdmx1_sal$caso_grave)
prop.table(table(cdmx1_sal$caso_grave))
chisq.test(table(cdmx_sal$caso_grave, cdmx_sal$covid))

table(cdmx1_sal$NAC_clin)
prop.table(table(cdmx1_sal$NAC_clin))
chisq.test(table(cdmx_sal$NAC_clin, cdmx_sal$covid))

table(cdmx1_sal$intubado)
prop.table(table(cdmx1_sal$intubado))
chisq.test(table(cdmx_sal$intubado, cdmx_sal$covid))

table(cdmx1_sal$diabetes)
prop.table(table(cdmx1_sal$diabetes))
chisq.test(table(cdmx_sal$diabetes, cdmx_sal$covid))

table(cdmx1_sal$epoc)
prop.table(table(cdmx1_sal$epoc))
chisq.test(table(cdmx_sal$epoc, cdmx_sal$covid))

table(cdmx1_sal$asma)
prop.table(table(cdmx1_sal$asma))
chisq.test(table(cdmx_sal$asma, cdmx_sal$covid))

table(cdmx1_sal$VIH_SIDA)
prop.table(table(cdmx1_sal$VIH_SIDA))
chisq.test(table(cdmx_sal$VIH_SIDA, cdmx_sal$covid))

table(cdmx1_sal$asma)
prop.table(table(cdmx1_sal$asma))
chisq.test(table(cdmx_sal$asma, cdmx_sal$covid))

table(cdmx1_sal$obesidad)
prop.table(table(cdmx1_sal$obesidad))
chisq.test(table(cdmx_sal$obesidad, cdmx_sal$covid))

table(cdmx1_sal$tabaquismo)
prop.table(table(cdmx1_sal$tabaquismo))
chisq.test(table(cdmx_sal$tabaquismo, cdmx_sal$covid))

table(cdmx1_sal$insuficiencia_renal_cronica)
prop.table(table(cdmx1_sal$insuficiencia_renal_cronica))
chisq.test(table(cdmx_sal$insuficiencia_renal_cronica, cdmx_sal$covid))

table(cdmx1_sal$esta_emabarazada)
prop.table(table(cdmx1_sal$esta_emabarazada))
chisq.test(table(cdmx_sal$esta_emabarazada, cdmx_sal$covid))

table(cdmx1_sal$indi_rec)
prop.table(table(cdmx1_sal$indi_rec))
chisq.test(table(cdmx_sal$indi_rec, cdmx_sal$covid))

#----Analisis de Grupos de salud descriptivo#####


cdmx1<- cdmx %>% filter(resultado_definitivo=="SARS-CoV-2")
cdmx_sal<-cdmx1%>% filter(pers_sal==1)
cdmx1_sal_cat<-cdmx1%>% filter(pers_sal==1 &pers_sal_cat_3==3)

nrow(cdmx1_sal_cat)

table(cdmx1_sal_cat$sexo)
prop.table(table(cdmx1_sal_cat$sexo))
chisq.test(table(cdmx_sal$sexo, cdmx_sal$pers_sal_cat_3))

psych::describe(cdmx1_sal_cat$edad)
wilcox.test(cdmx_sal$edad,cdmx_sal$pers_sal_cat_3)

table(cdmx1_sal_cat$ocupacionMEDICOS)
prop.table(table(cdmx1_sal_cat$ocupacionMEDICOS))
chisq.test(table(cdmx_sal$ocupacionMEDICOS, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$tiempo_prolong)
prop.table(table(cdmx1_sal_cat$tiempo_prolong))
chisq.test(table(cdmx_sal$tiempo_prolong, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$ocupacionENFERMERAS)
prop.table(table(cdmx1_sal_cat$ocupacionENFERMERAS))
chisq.test(table(cdmx_sal$ocupacionENFERMERAS, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$ocupacionDENTISTAS)
prop.table(table(cdmx1_sal_cat$ocupacionDENTISTAS))
chisq.test(table(cdmx_sal$ocupacionDENTISTAS, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$ocupacionLABORATORISTAS)
prop.table(table(cdmx1_sal_cat$ocupacionLABORATORISTAS))
chisq.test(table(cdmx_sal$ocupacionLABORATORISTAS, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$`ocupacionOTROS TRABAJADORES DE LA SALUD`)
prop.table(table(cdmx1_sal_cat$`ocupacionOTROS TRABAJADORES DE LA SALUD`))
chisq.test(table(cdmx_sal$`ocupacionOTROS TRABAJADORES DE LA SALUD`, cdmx_sal$pers_sal_cat_3))


table(cdmx1_sal_cat$evolucion_casoDEFUNCION)
prop.table(table(cdmx1_sal_cat$evolucion_casoDEFUNCION))
chisq.test(table(cdmx_sal$evolucion_casoDEFUNCION, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$HOSP)
prop.table(table(cdmx1_sal_cat$HOSP))
chisq.test(table(cdmx_sal$HOSP, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$caso_grave)
prop.table(table(cdmx1_sal_cat$caso_grave))
chisq.test(table(cdmx_sal$caso_grave, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$NAC_clin)
prop.table(table(cdmx1_sal_cat$NAC_clin))
chisq.test(table(cdmx_sal$NAC_clin, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$intubado)
prop.table(table(cdmx1_sal_cat$intubado))
chisq.test(table(cdmx_sal$intubado, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$diabetes)
prop.table(table(cdmx1_sal_cat$diabetes))
chisq.test(table(cdmx_sal$diabetes, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$epoc)
prop.table(table(cdmx1_sal_cat$epoc))
chisq.test(table(cdmx_sal$epoc, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$asma)
prop.table(table(cdmx1_sal_cat$asma))
chisq.test(table(cdmx_sal$asma, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$VIH_SIDA)
prop.table(table(cdmx1_sal_cat$VIH_SIDA))
chisq.test(table(cdmx_sal$VIH_SIDA, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$asma)
prop.table(table(cdmx1_sal_cat$asma))
chisq.test(table(cdmx_sal$asma, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$obesidad)
prop.table(table(cdmx1_sal_cat$obesidad))
chisq.test(table(cdmx_sal$obesidad, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$tabaquismo)
prop.table(table(cdmx1_sal_cat$tabaquismo))
chisq.test(table(cdmx_sal$tabaquismo, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$insuficiencia_renal_cronica)
prop.table(table(cdmx1_sal_cat$insuficiencia_renal_cronica))
chisq.test(table(cdmx_sal$insuficiencia_renal_cronica, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$esta_emabarazada)
prop.table(table(cdmx1_sal_cat$esta_emabarazada))
chisq.test(table(cdmx_sal$esta_emabarazada, cdmx_sal$pers_sal_cat_3))

table(cdmx1_sal_cat$indi_rec)
prop.table(table(cdmx1_sal_cat$indi_rec))
chisq.test(table(cdmx_sal$indi_rec, cdmx_sal$pers_sal_cat_3))

#END#