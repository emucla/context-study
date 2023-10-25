setwd("~/Desktop")

library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(dplyr)
library(contrast)
library(emmeans)
library(svglite)
library(ggeffects)

#read in the data 
pitch_org <- read.table('master.txt', header=T)

#clean data
pitch <- pitch_org %>% 
  #filter(noisy==1)
  mutate(no_topic = if_all(convo:vocalplay, `==`,0)) %>% 
  filter(!(no_topic & noisy == 1)) %>% # exclude 107 rows that are annotated exclusively as noisy (final count = 3620)
  mutate(register = recode(register, ADS = '0', IDS = '1' ), #binary code register
         register = as.factor(register)) %>%
  mutate(adu_gender = recode(adu_gender, female = '0', male = '1' ), 
         adu_gender = as.factor(adu_gender)) %>%
  mutate(chi_gender = recode(chi_gender, female = '0', male = '1' ), 
         chi_gender = as_factor(chi_gender)) %>%
  drop_na(adu_gender)


####--------------------Figure 1 plots-----------------------####

#log transform pitch variables 
pitch[,3:7] <- log(pitch[,3:7])

#center and scale log-transformed pitch variables 
pitch[,3:7] <- scale(pitch[,3:7], center = TRUE, scale = TRUE)

type_labels <- c("Inform","Conversational Basics",  "Question",  "Imperative")
names(type_labels) <- c("inform", "convo", 'question', "imperative")

dv_labs <- c("Mean Pitch", "Standard Deviation of Pitch")
names(dv_labs) <- c("m_pitch", "sd_pit")

y_limits_mean <- c(-1, 1)
y_limits_sd <- c(-1, 1)


#all horizontal 
pitch %>% 
  dplyr::select(m_pitch,sd_pit, adu_gender, register, inform, convo, question, imperative) %>%
  drop_na(adu_gender) %>%
  pivot_longer(cols = c(inform, convo, question, imperative), names_to="type", values_to="mark") %>%
  pivot_longer(cols = c(m_pitch, sd_pit), names_to = "dv", values_to = "val") %>%
  filter(mark == 1) %>% 
  mutate(adu_gender = factor(adu_gender, labels = c("Female", "Male")), 
         type = factor(type, levels = c("inform", "convo", "question" , "imperative"))) %>%
  ggplot(aes(x = adu_gender, y = val,fill=register)) + 
  geom_violin()+
  geom_point(aes(fill=register),colour = "white",pch=21, position=position_dodge(width=0.9))+ 
  ggh4x::facet_grid2(rows = vars(dv), cols=vars(type), 
                     labeller = labeller(type =type_labels, 
                                         dv = dv_labs), 
                     scales="free",
                     independent="y",
                     switch="y")+ 
  labs(x = "Adult Speaker Gender", y = NULL, fill = "Register")+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"), labels = c("ADS", "IDS"))+
  scale_y_continuous(position = "right")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12), 
        aspect.ratio=1.25)

ggsave("all_horizontal.svg")


####--------------------Figure 2 plots-----------------------####

IDSpitch <- pitch[which(pitch$register == 1),]

type_labels <- c("Inform",  "Conversational Basics", "Questions", "Imperatives", "Reading", "Singing", "Comfort")
names(type_labels) <- c("convo", "inform", "question", "imperative", "read", "sing", "comfort")

dv_labs <- c("Mean Pitch", "Standard Deviation of Pitch")
names(dv_labs) <- c("m_pitch", "sd_pit")

y_limits_mean <- c(-1, 1)
y_limits_sd <- c(-1, 1)


#horizontal 
IDSpitch %>% 
  dplyr::select(m_pitch,sd_pit, adu_gender, inform, convo, question, imperative, read, sing, comfort) %>%
  drop_na(adu_gender) %>%
  pivot_longer(cols = c(inform, convo, question, imperative, read, sing, comfort), names_to="type", values_to="mark") %>%
  pivot_longer(cols = c(m_pitch, sd_pit), names_to = "dv", values_to = "val") %>%
  filter(mark == 1) %>% 
  mutate(adu_gender = factor(adu_gender, labels = c("Female", "Male")), 
         type = factor(type, levels = c("inform", "convo", "question", "imperative", "read", "sing", "comfort"))) %>%
  ggplot(aes(x = adu_gender, y = val, fill=adu_gender)) + 
  geom_violin()+
  geom_point(aes(fill=adu_gender),colour = "white",pch=21, position=position_dodge(width=0.9))+ 
  ggh4x::facet_grid2(rows = vars(dv), cols=vars(type), 
                     labeller = labeller(type =type_labels, 
                                         dv = dv_labs), 
                     scales="free", 
                     switch="y")+ 
  labs(x = "Adult Speaker Gender", y = NULL)+
  scale_fill_manual(values=c( "#CC5500","#702963"), labels = c("ADS", "IDS"))+
  scale_y_continuous(position = "right")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12), 
        aspect.ratio=1.25,
        legend.position = "none")

ggsave("fig2_horizontal.svg")


####--------------------Figure 3 plots-----------------------####

type_labels <- c("Inform",  "Singing")
names(type_labels) <- c("inform", "sing")

dv_labs <- c("Mean Pitch", "Standard Deviation of Pitch")
names(dv_labs) <- c("m_pitch", "sd_pit")


#horizontal 
IDSpitch %>% 
  dplyr::select(m_pitch,sd_pit, adu_gender, chi_gender, inform, sing) %>%
  drop_na(adu_gender) %>%
  pivot_longer(cols = c(inform, sing), names_to="type", values_to="mark") %>%
  pivot_longer(cols = c(m_pitch, sd_pit), names_to = "dv", values_to = "val") %>%
  filter(mark == 1) %>% 
  mutate(chi_gender = factor(chi_gender, labels = c("Female", "Male")), 
         type = factor(type, levels = c("inform", "sing"))) %>%
  ggplot(aes(x = chi_gender, y = val, fill=adu_gender)) + 
  geom_violin()+
  geom_point(aes(fill=adu_gender),colour = "white",pch=21, position=position_dodge(width=0.9))+ 
  ggh4x::facet_grid2(rows = vars(dv), cols=vars(type), 
                     labeller = labeller(type =type_labels, 
                                         dv = dv_labs), 
                     scales="free",
                     independent = "y",
                     switch="y")+ 
  labs(x = "Child Speaker Gender", y = NULL, fill = "Adult Speaker Gender")+
  scale_fill_manual(values=c( "#CC5500","#702963"), labels = c("Female", "Male"))+
  scale_y_continuous(position = "right")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12), 
        aspect.ratio=1.25)
        #legend.position = "none")

ggsave("fig3_horizontal.svg")
