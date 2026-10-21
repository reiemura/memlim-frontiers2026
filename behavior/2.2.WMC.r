library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(buildmer)
library(sjPlot)
library(ggplot2)


df_filtered <- read.csv("df_filtered.csv", header = T, fill = T)
df_filtered$plausibility <- relevel(factor(df_filtered$plausibility), ref = "plausible")
df_filtered$task <- relevel(factor(df_filtered$task), ref = "nosecondary")

wmc <- read.csv("pyspan_original.csv", header=T, fill=T)

df_filtered_wwmc <- df_filtered %>%
  left_join(wmc, by = "participant") 
  
df_filtered_wwmc_artimp <- df_filtered_wwmc %>%
  filter(plausibility == 'implausible' & task == 'articulation')

glm_wmc <- glmer(correctness ~ pcu + (1|participant) + (1|item),
                 data = df_filtered_wwmc_artimp, family = binomial)
summary(glm_wmc)
#no significant
cor.test(df_filtered_wwmc_artimp$pcu, df_filtered_wwmc_artimp$correctness)
#ns

df_filtered_wwmc_artpla <- df_filtered_wwmc %>%
  filter(plausibility == 'plausible' & task == 'articulation')
glm_wmc_artpla <- glmer(correctness ~ pcu + (1|participant) + (1|item),
                 data = df_filtered_wwmc_artpla, family = binomial)
summary(glm_wmc_artpla)
#no significant
cor.test(df_filtered_wwmc_artpla$pcu, df_filtered_wwmc_artpla$correctness)
#ns

df_filtered_wwmc_noimp <- df_filtered_wwmc %>%
  filter(plausibility == 'implausible' & task == 'nosecondary')

glm_wmc_noimp <- glmer(correctness ~ pcu + (1|participant) + (1|item),
                 data = df_filtered_wwmc_noimp, family = binomial)
summary(glm_wmc_noimp)
#no significant
cor.test(df_filtered_wwmc_noimp$pcu, df_filtered_wwmc_noimp$correctness)
#ns

df_filtered_wwmc_nopla <- df_filtered_wwmc %>%
  filter(plausibility == 'plausible' & task == 'nosecondary')

glm_wmc_nopla <- glmer(correctness ~ pcu + (1|participant) + (1|item),
                       data = df_filtered_wwmc_nopla, family = binomial)
summary(glm_wmc_nopla)
#no significant
cor.test(df_filtered_wwmc_nopla$pcu, df_filtered_wwmc_nopla$correctness)
#ns



