setwd("~/Desktop")

library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(sjPlot)
library(dplyr)
library(plyr)

#read in the data 
pitch_org <- read.table('pilotdata.txt', header=T)

# exclude rows that are annotated exclusively as noisy

pitch <- pitch_org[!(!apply(pitch_org[,8:15], 1,any) & pitch_org[,16] == 1),]
pitch <- pitch %>%
  mutate(register = recode(register, ADS = 'ADS', CDS = 'IDS' ))


##----------------- Proportion of Context Occurrences Logistic Mixed Effects Model ---------###

pitchlog <- pitch %>%
  mutate(register = recode(register, ADS = '0', IDS = '1' ))
pitchlog$register <- as.factor(pitchlog$register)
prop <- glmer(register ~ convo + comfort + sing+ inform + read + imperative + question + vocalplay + (1 | ID) + (1 | coder), data = pitchlog, family=binomial)
summary(prop)
tab_model(prop, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Convo", "Comfort", "Sing", "Inform", "Read", "Imperative", "Question", "Vocal Play"),
          dv.labels= "Proportion")

#####------------------ Pitch Analyses w/o Speaker Gender (and plots) --------------------####

#conversational basics mean pitch 
mpitchcb = lmer(log(m_pitch) ~ convo + register + (1 | ID) + (1 | coder), data = pitch)
summary(mpitchcb)
tab_model(mpitchcb, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics", "Register"),
          dv.labels= "Mean Pitch")

#inform mean pitch 
mpitchin = lmer(log(m_pitch) ~ inform + register + (1 | ID) + (1 | coder), data = pitch)
summary(mpitchin)
tab_model(mpitchin, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Inform", "Register"),
          dv.labels= "Mean Pitch")

#questions mean pitch 
mpitchquest = lmer(log(m_pitch) ~ question + register + (1 | ID) + (1 | coder), data = pitch)
summary(mpitchquest)
tab_model(mpitchquest, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Question", "Register"),
          dv.labels= "Mean Pitch")

#violin plots for mean pitch 
pitch %>% 
  group_by (register) %>% 
  ungroup() %>% 
  mutate(convo = factor(convo, labels = c("Other Contexts","Conversational\n Basics"))) %>%
  ggplot(aes(x= convo, y= log(m_pitch), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  labs(x = "Context", y = "log(Mean Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

pitch %>% 
  group_by (register) %>% 
  ungroup() %>% 
  mutate(inform = factor(inform, labels = c("Other Contexts","Inform"))) %>%
  ggplot(aes(x= inform, y= log(m_pitch), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  labs(x = "Context", y = "log(Mean Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

pitch %>% 
  group_by (register) %>% 
  ungroup() %>% 
  mutate(question = factor(question, labels = c("Other Contexts","Questions"))) %>%
  ggplot(aes(x= question, y= log(m_pitch), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  labs(x = "Context", y = "log(Mean Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

#conversational basics pitch variability
sdpitchcb = lmer(log(sd_pit) ~ convo + register + (1 | ID) + (1 | coder), data = pitch)
summary(sdpitchcb)
tab_model(sdpitchcb, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Conversational Basics", "Register"),
          dv.labels= "Pitch Variability")

#inform mean pitch 
sdpitchin = lmer(log(sd_pit) ~ inform + register + (1 | ID) + (1 | coder), data = pitch)
summary(sdpitchin)
tab_model(sdpitchin, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Inform", "Register"),
          dv.labels= "Pitch Variability")

#questions mean pitch 
sdpitchquest = lmer(log(sd_pit) ~ question + register + (1 | ID) + (1 | coder), data = pitch)
summary(sdpitchquest)
tab_model(sdpitchquest, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Question", "Register"),
          dv.labels= "Pitch Variability")

#violin plots for pitch variability
pitch %>% 
  group_by (register) %>% 
  ungroup() %>% 
  mutate(convo = factor(convo, labels = c("Other Contexts","Conversational\n Basics"))) %>%
  ggplot(aes(x= convo, y= log(sd_pit), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  labs(x = "Context", y = "log(SD of Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

pitch %>% 
  group_by (register) %>% 
  ungroup() %>% 
  mutate(inform = factor(inform, labels = c("Other Contexts","Inform"))) %>%
  ggplot(aes(x= inform, y= log(sd_pit), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  labs(x = "Context", y = "log(SD of Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

pitch %>% 
  group_by (register) %>% 
  ungroup() %>% 
  mutate(question = factor(question, labels = c("Other Contexts","Questions"))) %>%
  ggplot(aes(x= question, y= log(sd_pit), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  labs(x = "Context", y = "log(SD of Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

#####------------------ Pitch Analyses w/ Speaker Gender (and plots) --------------------####
#mean pitch for conversational basics 

mpitchconvo = lmer(log(m_pitch) ~ register + convo + adu_gender + (1 | ID) + (1 | coder), data = pitch)
summary(mpitchconvo)
tab_model(mpitchconvo, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Register", "Conversational Basics", "Speaker Gender"),
          dv.labels= "Mean Pitch")

#violin plot for mean pitch for convo by gender of adult speaker 
#subset data for just female adult speakers
femdata <- pitch[which(pitch$adu_gender == "FEMALE"),]

femdata %>% 
  group_by (convo, register) %>% 
  ungroup() %>% 
  mutate(convo = factor(convo, labels = c("Other Contexts", "Conversational\n Basics"))) %>%
  ggplot(aes(x= convo, y= log(m_pitch), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(4.75, 6) +
  labs(x = "Context", y = "log(Mean Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

#subset data for just male adult speakers
maledata <- pitch[which(pitch$adu_gender == "MALE"),]

maledata %>% 
  group_by (convo, register) %>% 
  ungroup() %>% 
  mutate(convo = factor(convo, labels = c("Other Contexts", "Conversational\n Basics"))) %>%
  ggplot(aes(x= convo, y= log(m_pitch), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(4.75, 6) +
  labs(x = "Context", y = "log(Mean Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

#pitch variability for conversational basics
sdpitchconvo = lmer(log(sd_pit) ~ register + convo + adu_gender + (1 | ID) + (1 | coder), data = pitch)
summary(sdpitchconvo)
tab_model(sdpitchconvo, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Register", "Conversational Basics", "Speaker Gender"),
          dv.labels= "Pitch Variability")

#violin plot for pitch variability by speaker for convo 
femdata %>% 
  group_by (convo, register) %>% 
  ungroup() %>% 
  mutate(convo = factor(convo, labels = c("Other Contexts", "Conversational\n Basics"))) %>%
  ggplot(aes(x= convo, y= log(sd_pit), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(1,5)+
  labs(x = "Context", y = "log(SD of Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

maledata %>% 
  group_by (convo, register) %>% 
  ungroup() %>% 
  mutate(convo = factor(convo, labels = c("Other Contexts", "Conversational\n Basics"))) %>%
  ggplot(aes(x= convo, y= log(sd_pit), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(1,5)+
  labs(x = "Context", y = "log(SD of Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

#mean pitch for inform

mpitchinform = lmer(log(m_pitch) ~ register + inform + adu_gender + (1 | ID) + (1 | coder), data = pitch)
summary(mpitchinform)
tab_model(mpitchinform, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Register", "Inform", "Speaker Gender"),
          dv.labels= "Mean Pitch")

#violin plot for mean pitch of inform by adult speaker
femdata %>% 
  group_by (inform, register) %>% 
  ungroup() %>% 
  mutate(inform = factor(inform, labels = c("Other Contexts", "Inform"))) %>%
  ggplot(aes(x= inform, y= log(m_pitch), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(4.5,6)+
  labs(x = "Context", y = "log(Mean Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

maledata %>% 
  group_by (inform, register) %>% 
  ungroup() %>% 
  mutate(inform = factor(inform, labels = c("Other Contexts", "Inform"))) %>%
  ggplot(aes(x= inform, y= log(m_pitch), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(4.5,6)+
  labs(x = "Context", y = "log(Mean Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

#pitch variability for inform 
sdpitchinform = lmer(log(sd_pit) ~ register + inform + adu_gender + (1 | ID) + (1 | coder), data = pitch)
summary(sdpitchinform)
tab_model(sdpitchinform, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Register", "Inform", "Speaker Gender"),
          dv.labels= "Pitch Variability")

#violin plot for pitch variability of inform by adult speaker
femdata %>% 
  group_by (inform, register) %>% 
  ungroup() %>% 
  mutate(inform = factor(inform, labels = c("Other Contexts", "Inform"))) %>%
  ggplot(aes(x= inform, y= log(sd_pit), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(1,5)+
  labs(x = "Context", y = "log(SD of Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

maledata %>% 
  group_by (inform, register) %>% 
  ungroup() %>% 
  mutate(inform = factor(inform, labels = c("Other Contexts", "Inform"))) %>%
  ggplot(aes(x= inform, y= log(sd_pit), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(1,5)+
  labs(x = "Context", y = "log(SD of Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

#mean pitch for questions

mpitchquest = lmer(log(m_pitch) ~ register + question + adu_gender + (1 | ID) + (1 | coder), data = pitch)
summary(mpitchquest)
tab_model(mpitchquest, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Register", "Question", "Speaker Gender"),
          dv.labels= "Mean Pitch")

#violin plot for mean pitch of question by adult speaker
femdata %>% 
  group_by (question, register) %>% 
  ungroup() %>% 
  mutate(question = factor(question, labels = c("Other Contexts", "Questions"))) %>%
  ggplot(aes(x= question, y= log(m_pitch), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(4.5, 6)+
  labs(x = "Context", y = "log(Mean Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

maledata %>% 
  group_by (question, register) %>% 
  ungroup() %>% 
  mutate(question = factor(inform, labels = c("Other Contexts", "Questions"))) %>%
  ggplot(aes(x= question, y= log(m_pitch), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(4.5, 6)+
  labs(x = "Context", y = "log(Mean Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

#pitch variability for questions
sdpitchquest = lmer(log(sd_pit) ~ register + question + adu_gender + (1 | ID) + (1 | coder), data = pitch)
summary(sdpitchquest)
tab_model(sdpitchquest, show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Register", "Question", "Speaker Gender"),
          dv.labels= "Pitch Variability")

#violin plot for pitch variability of question by speaker 
femdata %>% 
  group_by (question, register) %>% 
  ungroup() %>% 
  mutate(question = factor(question, labels = c("Other Contexts", "Questions"))) %>%
  ggplot(aes(x= question, y= log(sd_pit), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(1,5)+
  labs(x = "Context", y = "log(SD of Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))

maledata %>% 
  group_by (question, register) %>% 
  ungroup() %>% 
  mutate(question = factor(inform, labels = c("Other Contexts", "Questions"))) %>%
  ggplot(aes(x= question, y= log(sd_pit), fill= register)) + 
  geom_violin()+ 
  geom_point(aes(fill=register), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  ylim(1,5)+
  labs(x = "Context", y = "log(SD of Pitch)", fill = "Register")+
  theme_bw()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,face = "bold", size = 12))+
  scale_fill_manual(values=c( "#56B4E9","#E69F00"))
