library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(buildmer)
library(sjPlot)

df_filtered <- read.csv("df_filtered.csv", header = T, fill = T)

df_filtered$plausibility <- relevel(factor(df_filtered$plausibility), ref = "plausible")
df_filtered$task <- relevel(factor(df_filtered$task), ref = "nosecondary")


glm_corr <- glmer(correctness ~ plausibility * task +  
                    (1+plausibility+task+plausibility:task|item) + 
                    (1+plausibility+task+plausibility:task|participant),
                  data = df_filtered, family = "binomial")
#failed to converge
buildmer(formula = correctness ~ plausibility * task + (1 + plausibility + task + plausibility:task | item) + 
           (1 + plausibility + task + plausibility:task | participant),
         data=df_filtered,
         family="binomial")
glm_corr <- glmer(correctness ~ plausibility * task +  
                    (1 + plausibility | participant) + 
                    (1 | item),
                  data = df_filtered, family = "binomial")

summary(glm_corr)




