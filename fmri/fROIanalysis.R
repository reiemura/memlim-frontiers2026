library(dplyr)
library(tidyverse)
library(plotrix)
library(ggplot2)
library(lmerTest)
library(sjPlot)

#### LIFG tri ####
df_tri <- read.csv("LIFGtri.csv", header=T)
df_tri <- df_tri %>%  pivot_longer(
  cols = c(NoPla, NoImp, ArtPla, ArtImp),
  names_to = "Condition",
  values_to = "Activation"
) %>% 
  mutate(
    Plausibility = case_when(
      Condition %in% c("ArtPla", "NoPla") ~ "Plausible",
      Condition %in% c("ArtImp", "NoImp") ~ "Implausible"
    ),
    Task = case_when(
      Condition %in% c("ArtPla", "ArtImp") ~ "Articulation",
      Condition %in% c("NoPla", "NoImp") ~ "NoTask"
    )
  )

df_tri <- df_tri %>%
  mutate(
    Plausibility = factor(Plausibility, levels = c("Plausible", "Implausible"))
  ) %>%
  mutate(
    Task = factor(Task, levels = c("NoTask", "Articulation"))
  )




#LME
lme_tri <- lmer(Activation ~  Plausibility * Task + (1|Participant),
                data = df_tri)
summary(lme_tri)

tab_model(lme_tri)
plot_model(lme_tri, type="pred", terms=c("Task", "Plausibility")) + theme_bw() + theme(legend.position = "bottom")

#Accuracy
#ArtImp
cor_acc_artimp_tri <- cor.test(df_artimp_tri$Accuracy, df_artimp_tri$Activation, method="pearson")
cor_acc_artimp_tri #r = 0.01834043 

#ArtPla
cor_acc_artpla_tri <- cor.test(df_artpla_tri$Accuracy, df_artpla_tri$Activation, method="pearson")
cor_acc_artpla_tri #r = -0.07672059

#NoImp
cor_acc_noimp_tri <- cor.test(df_noimp_tri$Accuracy, df_noimp_tri$Activation, method="pearson")
cor_acc_noimp_tri #r = -0.04068773   

#NoPla
cor_acc_nopla_tri <- cor.test(df_nopla_tri$Accuracy, df_nopla_tri$Activation, method="pearson")
cor_acc_nopla_tri #r = 0.05739311 

#WMC
#ArtImp
cor_wmc_artimp_tri <- cor.test(df_artimp_tri$PCU, df_artimp_tri$Activation, method="pearson")
cor_wmc_artimp_tri #r = -0.2309876

#ArtPla
cor_wmc_artpla_tri <- cor.test(df_artpla_tri$PCU, df_artpla_tri$Activation, method="pearson")
cor_wmc_artpla_tri #r = -0.08334556 

#NoImp
cor_wmc_noimp_tri <- cor.test(df_noimp_tri$PCU, df_noimp_tri$Activation, method="pearson")
cor_wmc_noimp_tri #r = 0.07144178

#NoPla
cor_wmc_nopla_tri <- cor.test(df_nopla_tri$PCU, df_nopla_tri$Activation, method="pearson")
cor_wmc_nopla_tri #r = -0.03943836 


#### LIFGorb ####

df_orb <- read.csv("LIFGorb.csv", header=T)
df_orb <- df_orb %>%  pivot_longer(
  cols = c(NoPla, NoImp, ArtPla, ArtImp),
  names_to = "Condition",
  values_to = "Activation"
) %>% 
  mutate(
    Plausibility = case_when(
      Condition %in% c("ArtPla", "NoPla") ~ "Plausible",
      Condition %in% c("ArtImp", "NoImp") ~ "Implausible"
    ),
    Task = case_when(
      Condition %in% c("ArtPla", "ArtImp") ~ "Articulation",
      Condition %in% c("NoPla", "NoImp") ~ "NoTask"
    )
  )

df_orb <- df_orb %>%
  mutate(
    Plausibility = factor(Plausibility, levels = c("Plausible", "Implausible"))
  ) %>%
  mutate(
    Task = factor(Task, levels = c("NoTask", "Articulation"))
  )


#LME 
lme_orb <- lmer(Activation ~  Plausibility * Task + (1|Participant),
                     data = df_orb)
summary(lme_orb)
tab_model(lme_orb)

lm_orb <- lm(Activation ~  Plausibility * Task, data = df_orb)
summary(lm_orb)

#Accuracy
#ArtImp
cor_acc_artimp_orb <- cor.test(df_artimp_orb$Accuracy, df_artimp_orb$Activation, method="pearson")
cor_acc_artimp_orb #r = 0.0009953529

#ArtPla
cor_acc_artpla_orb <- cor.test(df_artpla_orb$Accuracy, df_artpla_orb$Activation, method="pearson")
cor_acc_artpla_orb #r = 0.04914118 

#NoImp
cor_acc_noimp_orb <- cor.test(df_noimp_orb$Accuracy, df_noimp_orb$Activation, method="pearson")
cor_acc_noimp_orb #r = 0.1113009  

#NoPla
cor_acc_nopla_orb <- cor.test(df_nopla_orb$Accuracy, df_nopla_orb$Activation, method="pearson")
cor_acc_nopla_orb #r = 0.1494958 

#WMC
#ArtImp
cor_wmc_artimp_orb <- cor.test(df_artimp_orb$PCU, df_artimp_orb$Activation, method="pearson")
cor_wmc_artimp_orb #r = -0.08401179

#ArtPla
cor_wmc_artpla_orb <- cor.test(df_artpla_orb$PCU, df_artpla_orb$Activation, method="pearson")
cor_wmc_artpla_orb #r = 0.008533848 

#NoImp
cor_wmc_noimp_orb <- cor.test(df_noimp_orb$PCU, df_noimp_orb$Activation, method="pearson")
cor_wmc_noimp_orb #r = 0.1514021

#NoPla
cor_wmc_nopla_orb <- cor.test(df_nopla_orb$PCU, df_nopla_orb$Activation, method="pearson")
cor_wmc_nopla_orb #r = 0.06551583 


