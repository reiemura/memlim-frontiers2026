library(tidyverse)
library(dplyr)

df_original <- read.csv("df_original.csv", fill=T)
df_target <- read.csv("df_target.csv", fill=T)

### Screening ###
bad_participants <- df_original %>%
  group_by(participant) %>%
  filter(plausibility != "implausible") %>%
  summarise(correctness = mean(correctness)) %>%
  filter(correctness < 0.8) 
bad_participants

#Motion Checkで除外する被験者
bad_participants <- rbind(bad_participants, 
                          c("M25", NA),
                          c("M37", NA), 
                          c("M39", NA))
bad_participants

df_filtered <- df_target %>%
  filter(!(participant %in% bad_participants$participant))
write.csv(df_filtered, "df_filtered.csv", row.names=F)

#### correctness ####
summary_corr <- df_filtered %>%
  group_by(task, plausibility) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))

summary_corr_n12 <- df_filtered %>%
  group_by(task, plausibility, n12) %>%
  summarise(mean = mean(correctness), sd = sd(correctness))


#### summary by subject & item ####
summary_bysubject <- df_filtered %>%
  group_by(participant, task, plausibility) %>%
  summarise(mean_corr = mean(correctness)) %>%
  pivot_wider(values_from = mean_corr, names_from = plausibility) %>%
  mutate(difference = plausible - implausible) %>%
  select(participant, task, difference) %>%
  pivot_wider(values_from = difference, names_from = task) 

summary_byitem <- df_filtered %>%
  group_by(item, task, plausibility) %>%
  summarise(mean_corr = mean(correctness), mean_rt = mean(correctness))

accuracy_bysubject <- df_filtered %>%
  group_by(participant) %>%
  summarise(mean_corr = mean(correctness))


  
  